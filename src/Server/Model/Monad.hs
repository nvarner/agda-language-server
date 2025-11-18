{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Server.Model.Monad
  ( MonadAgdaLib (..),
    useAgdaLib,
    MonadAgdaFile (..),
    useAgdaFile,
    WithAgdaLibT,
    runWithAgdaLibT,
    WithAgdaLibM,
    runWithAgdaLib,
    WithAgdaFileT,
    runWithAgdaFileT,
    WithAgdaFileM,
  )
where

import Agda.Interaction.Options (CommandLineOptions (optPragmaOptions), PragmaOptions)
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Position (getRange)
import Agda.TypeChecking.Monad (HasOptions (..), MonadTCEnv (..), MonadTCM (..), MonadTCState (..), MonadTrace, PersistentTCState (stPersistentOptions), ReadTCState (..), TCEnv (..), TCM, TCMT (..), TCState (stPersistentState), catchError_, modifyTCLens, setTCLens, stPragmaOptions, useTC, viewTC)
import qualified Agda.TypeChecking.Monad as TCM
import qualified Agda.TypeChecking.Pretty as TCM
import Agda.Utils.IORef (modifyIORef', readIORef, writeIORef)
import Agda.Utils.Lens (Lens', locally, over, use, view, (<&>), (^.))
import Agda.Utils.Monad (and2M, bracket_, ifNotM, unless)
import Agda.Utils.Null (null)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as LSP
import Monad (ServerM, ServerT, askModel, catchTCError)
import Options (Config)
import qualified Server.Model as Model
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib, agdaLibTcEnv, agdaLibTcStateRef)
import Prelude hiding (null)
#if MIN_VERSION_Agda(2,8,0)
import Agda.Utils.FileId (File, getIdFile)
#endif
#if MIN_VERSION_Agda(2,7,0)
import Agda.Interaction.Response (Response_boot(Resp_HighlightingInfo))
import Server.AgdaLibResolver (findAgdaLib)
#else
import Agda.Interaction.Response (Response(Resp_HighlightingInfo))
#endif

--------------------------------------------------------------------------------

class (MonadTCM m, ReadTCState m) => MonadAgdaLib m where
  askAgdaLib :: m AgdaLib
  localAgdaLib :: (AgdaLib -> AgdaLib) -> m a -> m a

useAgdaLib :: (MonadAgdaLib m) => Lens' AgdaLib a -> m a
useAgdaLib lens = do
  agdaLib <- askAgdaLib
  return $ agdaLib ^. lens

class (MonadAgdaLib m) => MonadAgdaFile m where
  askAgdaFile :: m AgdaFile
  localAgdaFile :: (AgdaFile -> AgdaFile) -> m a -> m a

useAgdaFile :: (MonadAgdaFile m) => Lens' AgdaFile a -> m a
useAgdaFile lens = do
  agdaFile <- askAgdaFile
  return $ agdaFile ^. lens

--------------------------------------------------------------------------------

defaultAskTC :: (MonadAgdaLib m) => m TCEnv
defaultAskTC = useAgdaLib agdaLibTcEnv

defaultLocalTC :: (MonadAgdaLib m) => (TCEnv -> TCEnv) -> m a -> m a
defaultLocalTC f = localAgdaLib (over agdaLibTcEnv f)

defaultGetTC :: (MonadAgdaLib m) => m TCState
defaultGetTC = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  liftIO $ readIORef tcStateRef

defaultPutTC :: (MonadAgdaLib m) => TCState -> m ()
defaultPutTC tcState = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  liftIO $ writeIORef tcStateRef tcState

defaultModifyTC :: (MonadAgdaLib m) => (TCState -> TCState) -> m ()
defaultModifyTC f = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  liftIO $ modifyIORef' tcStateRef f

-- Taken from TCMT implementation
defaultLocallyTCState :: (MonadAgdaLib m) => Lens' TCState a -> (a -> a) -> m b -> m b
defaultLocallyTCState lens f = bracket_ (useTC lens <* modifyTCLens lens f) (setTCLens lens)

-- Taken from TCMT implementation
defaultPragmaOptionsImpl :: (MonadAgdaLib m) => m PragmaOptions
defaultPragmaOptionsImpl = useTC stPragmaOptions

-- Taken from TCMT implementation
defaultCommandLineOptionsImpl :: (MonadAgdaLib m) => m CommandLineOptions
defaultCommandLineOptionsImpl = do
  p <- useTC stPragmaOptions
  cl <- stPersistentOptions . stPersistentState <$> getTC
  return $ cl {optPragmaOptions = p}

defaultLiftTCM :: (MonadAgdaLib m) => TCM a -> m a
defaultLiftTCM (TCM f) = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  tcEnv <- useAgdaLib agdaLibTcEnv
  liftIO $ f tcStateRef tcEnv

-- Taken from TCM implementation
defaultTraceClosureCall :: (MonadAgdaLib m, MonadTrace m) => TCM.Closure TCM.Call -> m a -> m a
defaultTraceClosureCall cl m = do
  -- Compute update to 'Range' and 'Call' components of 'TCEnv'.
  let withCall =
        localTC $
          foldr (.) id $
            concat $
              [ [\e -> e {envCall = Just cl} | TCM.interestingCall call],
                [ \e -> e {envHighlightingRange = callRange}
                | callHasRange && highlightCall
                    || isNoHighlighting
                ],
                [\e -> e {envRange = callRange} | callHasRange]
              ]

  -- For interactive highlighting, also wrap computation @m@ in 'highlightAsTypeChecked':
  ifNotM
    (pure highlightCall `and2M` do (TCM.Interactive ==) . envHighlightingLevel <$> askTC)
    {-then-} (withCall m)
    {-else-} $ do
      oldRange <- envHighlightingRange <$> askTC
      TCM.highlightAsTypeChecked oldRange callRange $
        withCall m
  where
    call = TCM.clValue cl
    callRange = getRange call
    callHasRange = not $ null callRange

    -- Should the given call trigger interactive highlighting?
    highlightCall = case call of
      TCM.CheckClause {} -> True
      TCM.CheckLHS {} -> True
      TCM.CheckPattern {} -> True
      TCM.CheckPatternLinearityType {} -> False
      TCM.CheckPatternLinearityValue {} -> False
      TCM.CheckLetBinding {} -> True
      TCM.InferExpr {} -> True
      TCM.CheckExprCall {} -> True
      TCM.CheckDotPattern {} -> True
      TCM.IsTypeCall {} -> True
      TCM.IsType_ {} -> True
      TCM.InferVar {} -> True
      TCM.InferDef {} -> True
      TCM.CheckArguments {} -> True
      TCM.CheckMetaSolution {} -> False
      TCM.CheckTargetType {} -> False
      TCM.CheckDataDef {} -> True
      TCM.CheckRecDef {} -> True
      TCM.CheckConstructor {} -> True
      TCM.CheckConArgFitsIn {} -> False
      TCM.CheckFunDefCall _ _ _ h -> h
      TCM.CheckPragma {} -> True
      TCM.CheckPrimitive {} -> True
      TCM.CheckIsEmpty {} -> True
      TCM.CheckConfluence {} -> False
      TCM.CheckIApplyConfluence {} -> False
      TCM.CheckModuleParameters {} -> False
      TCM.CheckWithFunctionType {} -> True
      TCM.CheckSectionApplication {} -> True
      TCM.CheckNamedWhere {} -> False
      TCM.ScopeCheckExpr {} -> False
      TCM.ScopeCheckDeclaration {} -> False
      TCM.ScopeCheckLHS {} -> False
      TCM.NoHighlighting {} -> True
      TCM.CheckProjection {} -> False
      TCM.SetRange {} -> False
      TCM.ModuleContents {} -> False

    isNoHighlighting = case call of
      TCM.NoHighlighting {} -> True
      _ -> False

    printHighlightingInfo remove info = do
      modToSrc <- useTC TCM.stModuleToSource
      method <- viewTC TCM.eHighlightingMethod
      -- reportSDoc "highlighting" 50 $
      --   pure $
      --     vcat
      --       [ "Printing highlighting info:",
      --         nest 2 $ (text . show) info,
      --         "File modules:",
      --         nest 2 $ pretty modToSrc
      --       ]
      unless (null info) $ do
        TCM.appInteractionOutputCallback $
          Resp_HighlightingInfo info remove method modToSrc

#if MIN_VERSION_Agda(2,8,0)
-- Taken from TCMT implementation
defaultFileFromId :: (MonadAgdaLib m) => TCM.FileId -> m File
defaultFileFromId fi = useTC TCM.stFileDict <&> (`getIdFile` fi)

-- Taken from TCMT implementation
defaultIdFromFile :: (MonadAgdaLib m) => File -> m TCM.FileId
defaultIdFromFile = TCM.stateTCLens TCM.stFileDict . TCM.registerFileIdWithBuiltin
#endif

--------------------------------------------------------------------------------

newtype WithAgdaLibT m a = WithAgdaLibT {unWithAgdaLibT :: ReaderT AgdaLib m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runWithAgdaLibT :: AgdaLib -> WithAgdaLibT m a -> m a
runWithAgdaLibT agdaLib = flip runReaderT agdaLib . unWithAgdaLibT

type WithAgdaLibM = WithAgdaLibT ServerM

runWithAgdaLib :: LSP.Uri -> WithAgdaLibM a -> ServerM a
runWithAgdaLib uri x = do
  agdaLib <- findAgdaLib uri
  runWithAgdaLibT agdaLib x

instance (MonadIO m) => MonadAgdaLib (WithAgdaLibT m) where
  askAgdaLib = WithAgdaLibT ask
  localAgdaLib f = WithAgdaLibT . local f . unWithAgdaLibT

instance (MonadIO m) => MonadTCEnv (WithAgdaLibT m) where
  askTC = defaultAskTC
  localTC = defaultLocalTC

instance (MonadIO m) => MonadTCState (WithAgdaLibT m) where
  getTC = defaultGetTC
  putTC = defaultPutTC
  modifyTC = defaultModifyTC

instance (MonadIO m) => ReadTCState (WithAgdaLibT m) where
  getTCState = defaultGetTC
  locallyTCState = defaultLocallyTCState

instance (MonadIO m) => HasOptions (WithAgdaLibT m) where
  pragmaOptions = defaultPragmaOptionsImpl
  commandLineOptions = defaultCommandLineOptionsImpl

-- TODO: how should this really be implemented?
instance (MonadIO m) => MonadTrace (WithAgdaLibT m) where
  traceClosureCall = defaultTraceClosureCall
  printHighlightingInfo _ _ = return ()

instance (MonadIO m) => MonadTCM (WithAgdaLibT m) where
  liftTCM = defaultLiftTCM

#if MIN_VERSION_Agda(2,8,0)
instance (MonadIO m) => TCM.MonadFileId (WithAgdaLibT m) where
  fileFromId = defaultFileFromId
  idFromFile = defaultIdFromFile
#endif

--------------------------------------------------------------------------------

data WithAgdaFileEnv = WithAgdaFileEnv
  { _withAgdaFileEnvAgdaLib :: !AgdaLib,
    _withAgdaFileEnvAgdaFile :: !AgdaFile
  }

withAgdaFileEnvAgdaLib :: Lens' WithAgdaFileEnv AgdaLib
withAgdaFileEnvAgdaLib f a = f (_withAgdaFileEnvAgdaLib a) <&> \x -> a {_withAgdaFileEnvAgdaLib = x}

withAgdaFileEnvAgdaFile :: Lens' WithAgdaFileEnv AgdaFile
withAgdaFileEnvAgdaFile f a = f (_withAgdaFileEnvAgdaFile a) <&> \x -> a {_withAgdaFileEnvAgdaFile = x}

newtype WithAgdaFileT m a = WithAgdaFileT
  {unWithAgdaFileT :: ReaderT WithAgdaFileEnv m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runWithAgdaFileT :: AgdaLib -> AgdaFile -> WithAgdaFileT m a -> m a
runWithAgdaFileT agdaLib agdaFile =
  let env = WithAgdaFileEnv agdaLib agdaFile
   in flip runReaderT env . unWithAgdaFileT

type WithAgdaFileM = WithAgdaFileT ServerM

instance (MonadIO m) => MonadAgdaLib (WithAgdaFileT m) where
  askAgdaLib = WithAgdaFileT $ view withAgdaFileEnvAgdaLib
  localAgdaLib f = WithAgdaFileT . locally withAgdaFileEnvAgdaLib f . unWithAgdaFileT

instance (MonadIO m) => MonadAgdaFile (WithAgdaFileT m) where
  askAgdaFile = WithAgdaFileT $ view withAgdaFileEnvAgdaFile
  localAgdaFile f = WithAgdaFileT . locally withAgdaFileEnvAgdaFile f . unWithAgdaFileT

instance (MonadIO m) => MonadTCEnv (WithAgdaFileT m) where
  askTC = defaultAskTC
  localTC = defaultLocalTC

instance (MonadIO m) => MonadTCState (WithAgdaFileT m) where
  getTC = defaultGetTC
  putTC = defaultPutTC
  modifyTC = defaultModifyTC

instance (MonadIO m) => ReadTCState (WithAgdaFileT m) where
  getTCState = defaultGetTC
  locallyTCState = defaultLocallyTCState

instance (MonadIO m) => HasOptions (WithAgdaFileT m) where
  pragmaOptions = defaultPragmaOptionsImpl
  commandLineOptions = defaultCommandLineOptionsImpl

instance (MonadIO m) => MonadTCM (WithAgdaFileT m) where
  liftTCM = defaultLiftTCM
