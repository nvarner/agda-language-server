{-# LANGUAGE CPP #-}

module Server.Model.AgdaLib
  ( AgdaLib (AgdaLib),
    initAgdaLib,
    agdaLibIncludes,
    agdaLibDependencies,
    agdaLibTcStateRef,
    agdaLibTcEnv,
    isAgdaLibForUri,
    agdaLibFromFs,
    agdaLibToFile,
  )
where

import Agda.Interaction.Library (
    AgdaLibFile (_libIncludes, AgdaLibFile),
    findProjectRoot,
    LibName,
    OptionsPragma (OptionsPragma),
#if MIN_VERSION_Agda(2,8,0)
    getAgdaLibFile,
#else
    getAgdaLibFiles',
#endif
  )
import Agda.Interaction.Library.More (
    tryRunLibM,
#if MIN_VERSION_Agda(2,8,0)
#else
    runLibErrorIO,
#endif
  )
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.IORef (IORef, newIORef)
import Agda.Utils.Lens (Lens', (<&>), (^.), set)
import Agda.Utils.Maybe (listToMaybe)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Server.Model.AgdaFile (AgdaFile)
import Agda.Interaction.Library.Base (libFile, LibName (..), libName, libIncludes, libPragmas)
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidFilePath)
import Agda.Utils.Null (empty)
import Agda.Syntax.Common.Pretty (Pretty, pretty, vcat, prettyAssign, text, pshow, doubleQuotes, (<+>))

data AgdaLibOrigin = FromFile !FilePath | Defaulted deriving (Show)

data AgdaLib = AgdaLib
  { _agdaLibName :: !LibName,
    _agdaLibIncludes :: ![LSP.NormalizedUri],
    _agdaLibOptionsPragma :: !OptionsPragma,
    _agdaLibDependencies :: ![LibName],
    _agdaLibTcStateRef :: !(IORef TCM.TCState),
    _agdaLibTcEnv :: !TCM.TCEnv,
    _agdaLibOrigin :: !AgdaLibOrigin
  }

instance Pretty AgdaLib where
  pretty agdaLib =
    text "AgdaLib"
      <+> doubleQuotes (pretty $ agdaLib ^. agdaLibName)
      <+> pshow (agdaLib ^. agdaLibOrigin)
      <+> text "includes:"
      <+> pretty (LSP.getNormalizedUri <$> (agdaLib ^. agdaLibIncludes))

initAgdaLibWithOrigin :: (MonadIO m) => AgdaLibOrigin -> m AgdaLib
initAgdaLibWithOrigin origin = do
#if MIN_VERSION_Agda(2,8,0)
  let libName = LibName "" []
  tcState <- liftIO TCM.initStateIO
#else
  let libName = ""
  let tcState = TCM.initState
#endif
  tcStateRef <- liftIO $ newIORef tcState
  let tcEnv = TCM.initEnv
  let optionsPragma = OptionsPragma [] empty
  return $ AgdaLib libName [] optionsPragma [] tcStateRef tcEnv origin

initAgdaLib :: (MonadIO m) => m AgdaLib
initAgdaLib = initAgdaLibWithOrigin Defaulted

agdaLibName :: Lens' AgdaLib LibName
agdaLibName f a = f (_agdaLibName a) <&> \x -> a {_agdaLibName = x}

agdaLibIncludes :: Lens' AgdaLib [LSP.NormalizedUri]
agdaLibIncludes f a = f (_agdaLibIncludes a) <&> \x -> a {_agdaLibIncludes = x}

agdaLibOptionsPragma :: Lens' AgdaLib OptionsPragma
agdaLibOptionsPragma f a = f (_agdaLibOptionsPragma a) <&> \x -> a {_agdaLibOptionsPragma = x}

agdaLibDependencies :: Lens' AgdaLib [LibName]
agdaLibDependencies f a = f (_agdaLibDependencies a) <&> \x -> a {_agdaLibDependencies = x}

agdaLibTcStateRef :: Lens' AgdaLib (IORef TCM.TCState)
agdaLibTcStateRef f a = f (_agdaLibTcStateRef a) <&> \x -> a {_agdaLibTcStateRef = x}

agdaLibTcEnv :: Lens' AgdaLib TCM.TCEnv
agdaLibTcEnv f a = f (_agdaLibTcEnv a) <&> \x -> a {_agdaLibTcEnv = x}

agdaLibOrigin :: Lens' AgdaLib AgdaLibOrigin
agdaLibOrigin f a = f (_agdaLibOrigin a) <&> \x -> a {_agdaLibOrigin = x}

isAgdaLibForUri :: AgdaLib -> LSP.NormalizedUri -> Bool
isAgdaLibForUri agdaLib uri = any (`LSP.isUriAncestorOf` uri) (agdaLib ^. agdaLibIncludes)

-- | Get an 'AgdaLib' from @.agda-lib@ files on the filesystem. These files are
-- searched for by traversing parent directories until one is found.
agdaLibFromFs ::
  (MonadIO m) =>
  -- | Directory to start the search from
  FilePath ->
  m (Maybe AgdaLib)
agdaLibFromFs path = do
  root <- tryRunLibM $ findProjectRoot path
  case root of
    Just (Just root) -> do
      libFile <- tryGetLibFileFromRootPath root
      case libFile of
        Just libFile -> Just <$> agdaLibFromFile libFile
        Nothing -> return Nothing
    _noRoot -> return Nothing

-- TODO: this traverses the filesystem
tryGetLibFileFromRootPath :: (MonadIO m) => FilePath -> m (Maybe AgdaLibFile)
#if MIN_VERSION_Agda(2,8,0)
tryGetLibFileFromRootPath root = do
  maybeLibFiles <- tryRunLibM $ getAgdaLibFile root
  case maybeLibFiles of
    Just (libFile:_) -> return $ Just libFile
    _ -> return Nothing
#else
tryGetLibFileFromRootPath root = do
  libFiles <- runLibErrorIO $ getAgdaLibFiles' root
  return $ listToMaybe libFiles
#endif

agdaLibFromFile :: (MonadIO m) => AgdaLibFile -> m AgdaLib
agdaLibFromFile agdaLibFile = do
  let includes = LSP.toNormalizedUri . LSP.filePathToUri <$> agdaLibFile ^. libIncludes
  initAgdaLibWithOrigin (FromFile $ agdaLibFile ^. libFile)
    <&> set agdaLibName (agdaLibFile ^. libName)
    <&> set agdaLibIncludes includes
    <&> set agdaLibOptionsPragma (agdaLibFile ^. libPragmas)

-- | If the given `AgdaLib` came from a file, turn it back into one. Since
-- `AgdaLibFile`s are relative to an Agda file on the filesystem, the first
-- parameter is for the URI for the Agda file
agdaLibToFile :: LSP.NormalizedUri -> AgdaLib -> Maybe AgdaLibFile
agdaLibToFile relativeToUri agdaLib = case agdaLib ^. agdaLibOrigin of
  Defaulted -> Nothing
  FromFile filePath ->
    let includePaths = uriToPossiblyInvalidFilePath <$> agdaLib ^. agdaLibIncludes
        uri = LSP.toNormalizedUri $ LSP.filePathToUri filePath
        above = LSP.uriHeightAbove uri relativeToUri
    in Just $ AgdaLibFile (agdaLib ^. agdaLibName) filePath above includePaths [] (agdaLib ^. agdaLibOptionsPragma)
