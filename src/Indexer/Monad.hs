{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Indexer.Monad
  ( IndexerM,
    execIndexerM,
    tellParamNames,
    tellDef,
    tellDecl,
    tellUsage,
    tellImport,
    tellNamedArgUsage,
    withParent,
    NameLike (..),
    AmbiguousNameLike (..),
    SymbolKindLike (..),
    TypeLike (..),
    HasParamNames (..),
    UnknownType (UnknownType),
    NoType (NoType),
  )
where

import Agda.Position (toLspRange)
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Common as C
import Agda.Syntax.Position (HasRange, getRange)
import Agda.Utils.IORef (IORef, modifyIORef', newIORef, readIORef)
import Agda.Utils.Lens (over, (^.))
import Agda.Utils.List1 (List1, concatMap1)
import Agda.Utils.Maybe (isNothing)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.Trans (lift)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Server as LSP
import Monad (ServerM)
import Options (Config)
import Server.Model.AgdaFile (AgdaFile, agdaFileRefs, agdaFileSymbols, emptyAgdaFile, insertRef, insertSymbolInfo)
import Server.Model.Monad (WithAgdaLibM)
import Server.Model.Symbol (Ref (Ref), RefKind (..), SymbolInfo (..), SymbolKind (..), refKind, refRange)

data NamedArgUsage = NamedArgUsage
  { nauHead :: !A.AmbiguousQName,
    nauArg :: !C.NamedName
  }

data Env = Env
  { envAgdaFile :: !(IORef AgdaFile),
    envParent :: !(Maybe A.QName),
    -- | Parameter names in implicit arguments, such as x in `f {x = y} = y` and
    -- `g y = f {x = y}`, are represented by strings in abstract syntax. This is
    -- because we need type checking information to resolve these names, not
    -- just scope checking information.
    --
    -- Agda doesn't seem to expose its internal type checker resolution of these
    -- names. We hack around it by storing the implicit parameter names for each
    -- function ourselves, then looking up the strings at the end and emitting
    -- them as references. Ideally, this solution is eventually replaced by
    -- changing the type checker so it emits this information for us.
    --
    -- These are the stored parameter names, indexed by the name of the function
    -- (or other defined symbol).
    envParamNames :: !(IORef (Map A.QName [A.QName])),
    envNamedArgUsages :: !(IORef [NamedArgUsage])
  }

initEnv :: (MonadIO m) => m Env
initEnv = do
  agdaFile <- liftIO $ newIORef emptyAgdaFile
  let parent = Nothing
  paramNames <- liftIO $ newIORef Map.empty
  namedArgUsages <- liftIO $ newIORef []
  return $ Env agdaFile parent paramNames namedArgUsages

type IndexerM = ReaderT Env WithAgdaLibM

execIndexerM :: IndexerM a -> WithAgdaLibM AgdaFile
execIndexerM x = do
  env <- initEnv
  _ <- runReaderT (x >> postprocess) env
  liftIO $ readIORef $ envAgdaFile env

--------------------------------------------------------------------------------

-- Used when inserting a new `SymbolInfo` into a map already containing a
-- `SymbolInfo` for the given symbol
updateSymbolInfo :: SymbolInfo -> SymbolInfo -> SymbolInfo
updateSymbolInfo new old =
  SymbolInfo
    (symbolKind old <> symbolKind new)
    (symbolType old <|> symbolType new)
    (symbolParent old <|> symbolParent new)

tellSymbolInfo' :: A.QName -> SymbolInfo -> IndexerM ()
tellSymbolInfo' name symbolInfo = do
  agdaFileRef <- asks envAgdaFile
  liftIO $ modifyIORef' agdaFileRef $ insertSymbolInfo updateSymbolInfo name symbolInfo

tellSymbolInfo ::
  (NameLike n, SymbolKindLike s, TypeLike t) =>
  n -> s -> t -> IndexerM ()
tellSymbolInfo nameLike symbolKindLike typeLike = do
  let name = toQName nameLike
      symbolKind = toSymbolKind symbolKindLike
  type' <- lift $ toTypeString typeLike
  parent <- asks envParent
  let symbolInfo = SymbolInfo symbolKind type' parent
  tellSymbolInfo' name symbolInfo

tellRef' :: A.AmbiguousQName -> Ref -> IndexerM ()
tellRef' ambiguousName ref = do
  agdaFileRef <- asks envAgdaFile
  liftIO $ modifyIORef' agdaFileRef $ insertRef ambiguousName ref

tellRef ::
  (AmbiguousNameLike n) =>
  n -> RefKind -> IndexerM ()
tellRef nameLike refKind = do
  let name = toAmbiguousQName nameLike
      range = toLspRange $ getRange name
      ref = Ref refKind range (A.isAmbiguous name)
  tellRef' name ref

tellParamNames :: (NameLike n, HasParamNames p) => n -> p -> IndexerM ()
tellParamNames nameLike hasParamNames = do
  let name = toQName nameLike
  let paramNames = getParamNames hasParamNames
  paramNamesRef <- asks envParamNames
  liftIO $ modifyIORef' paramNamesRef $ Map.insert name paramNames

tellDef ::
  (NameLike n, SymbolKindLike s, TypeLike t, HasParamNames t) =>
  n -> s -> t -> IndexerM ()
tellDef n s t = do
  tellSymbolInfo n s t
  tellRef n Def
  tellParamNames n t

tellDecl ::
  (NameLike n, SymbolKindLike s, TypeLike t, HasParamNames t) =>
  n -> s -> t -> IndexerM ()
tellDecl n s t = do
  tellSymbolInfo n s t
  tellRef n Decl
  tellParamNames n t

tellUsage :: (AmbiguousNameLike n) => n -> IndexerM ()
tellUsage n = tellRef n Usage

tellImport :: (AmbiguousNameLike n) => n -> IndexerM ()
tellImport n = tellRef n Import

tellNamedArgUsage :: (AmbiguousNameLike n) => n -> C.NamedName -> IndexerM ()
tellNamedArgUsage headNameLike argName = do
  let headName = toAmbiguousQName headNameLike
      namedArgUsage = NamedArgUsage headName argName
  namedArgUsagesRef <- asks envNamedArgUsages
  liftIO $ modifyIORef' namedArgUsagesRef $ (namedArgUsage :)

withParent :: (NameLike n) => n -> IndexerM a -> IndexerM a
withParent nameLike = local $ \e -> e {envParent = Just $ toQName nameLike}

--------------------------------------------------------------------------------

postprocess :: IndexerM ()
postprocess = do
  agdaFileRef <- asks envAgdaFile
  -- TODO: resolve named arg usages
  liftIO $ modifyIORef' agdaFileRef $ over agdaFileRefs $ Map.map dedupSimultaneousDeclDef

-- | We sometimes emit a `Decl` and `Def` for the same source range. For
-- example, we must emit a distinct `Decl` and `Def` when a `data` is declared
-- and defined separately, but not when it is declared and defined all at once.
-- It is harder distinguish these in abstract syntax than to idenfify and
-- correct them at the end.
dedupSimultaneousDeclDef :: [Ref] -> [Ref]
dedupSimultaneousDeclDef refs =
  case find (\ref -> refKind ref == Decl) refs of
    Nothing -> refs
    Just decl ->
      let pred ref = not (refKind ref == Def && refRange ref == refRange decl)
       in filter pred refs

--------------------------------------------------------------------------------

class (AmbiguousNameLike n) => NameLike n where
  toQName :: n -> A.QName

class AmbiguousNameLike n where
  toAmbiguousQName :: n -> A.AmbiguousQName

instance (AmbiguousNameLike n) => AmbiguousNameLike (List1 n) where
  toAmbiguousQName = A.AmbQ . concatMap1 (A.unAmbQ . toAmbiguousQName)

class SymbolKindLike a where
  toSymbolKind :: a -> SymbolKind

class HasParamNames p where
  getParamNames :: p -> [A.QName]
  default getParamNames :: (Foldable f, HasParamNames b, p ~ f b) => p -> [A.QName]
  getParamNames = foldMap getParamNames

instance (HasParamNames p) => HasParamNames (Maybe p) where
  getParamNames = maybe [] getParamNames

class TypeLike t where
  -- | Try to render a type to a @String@. Strings were chosen because:
  --  - they are easily obtained from abstract and internal terms
  --  - they do not depend on the scope, context, or other TC state
  --  - they have a low memory footprint compared to unrendered @Doc@s
  --
  -- However, strings do lose semantic information otherwise available to us,
  -- so this representation may be switched in the future if that information is
  -- needed.
  toTypeString :: t -> WithAgdaLibM (Maybe String)

instance (TypeLike t) => TypeLike (Maybe t) where
  toTypeString = maybe (return Nothing) toTypeString

data UnknownType = UnknownType

instance HasParamNames UnknownType where
  getParamNames UnknownType = []

instance TypeLike UnknownType where
  toTypeString UnknownType = return Nothing

data NoType = NoType

instance HasParamNames NoType where
  getParamNames NoType = []

instance TypeLike NoType where
  toTypeString NoType = return Nothing
