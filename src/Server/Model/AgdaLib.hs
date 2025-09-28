{-# LANGUAGE CPP #-}

module Server.Model.AgdaLib
  ( AgdaLib (AgdaLib),
    initAgdaLib,
    agdaLibIncludes,
    agdaLibTcStateRef,
    agdaLibTcEnv,
    isAgdaLibForUri,
    agdaLibFromFs,
  )
where

import Agda.Interaction.Library (
    AgdaLibFile (_libIncludes),
    findProjectRoot,
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
import Agda.Utils.Lens (Lens', (<&>), (^.))
import Agda.Utils.Maybe (listToMaybe)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Server.Model.AgdaFile (AgdaFile)

data AgdaLib = AgdaLib
  { _agdaLibIncludes :: ![LSP.NormalizedUri],
    _agdaLibTcStateRef :: !(IORef TCM.TCState),
    _agdaLibTcEnv :: !TCM.TCEnv
  }

initAgdaLibWithIncludes :: (MonadIO m) => [LSP.NormalizedUri] -> m AgdaLib
initAgdaLibWithIncludes includes = do
#if MIN_VERSION_Agda(2,8,0)
  tcState <- liftIO TCM.initStateIO
#else
  let tcState = TCM.initState
#endif
  tcStateRef <- liftIO $ newIORef tcState
  let tcEnv = TCM.initEnv
  return $ AgdaLib includes tcStateRef tcEnv

initAgdaLib :: (MonadIO m) => m AgdaLib
initAgdaLib = initAgdaLibWithIncludes []

agdaLibIncludes :: Lens' AgdaLib [LSP.NormalizedUri]
agdaLibIncludes f a = f (_agdaLibIncludes a) <&> \x -> a {_agdaLibIncludes = x}

agdaLibTcStateRef :: Lens' AgdaLib (IORef TCM.TCState)
agdaLibTcStateRef f a = f (_agdaLibTcStateRef a) <&> \x -> a {_agdaLibTcStateRef = x}

agdaLibTcEnv :: Lens' AgdaLib TCM.TCEnv
agdaLibTcEnv f a = f (_agdaLibTcEnv a) <&> \x -> a {_agdaLibTcEnv = x}

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
agdaLibFromFile libFile = do
  let includes = LSP.toNormalizedUri . LSP.filePathToUri <$> _libIncludes libFile
  initAgdaLibWithIncludes includes
