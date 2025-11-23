module Server.AgdaLibResolver
  ( byFileId,
    byLibName,
    installedLibraries,
  )
where

import Agda.Interaction.Library (AgdaLibFile, LibName)
import Agda.Interaction.Library.Base (libName)
import Agda.Interaction.Library.More (defaultLibraryFileIds)
import Agda.Interaction.Library.Parse.More (parseLibFile, runP)
import Agda.Setup (getAgdaAppDir)
import Agda.Utils.Either (maybeRight)
import Agda.Utils.Lens ((^.))
import Agda.Utils.List (nubOn)
import Agda.Utils.Maybe (caseMaybeM, listToMaybe)
import Agda.Utils.Monad (mapMaybeM)
import Agda.Utils.Singleton (Singleton (singleton))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (Foldable (toList), find)
import Monad (ServerM, askFilesystemProvider, askModel, modifyModel)
import Server.Filesystem (FileId (LocalFilePath))
import qualified Server.Filesystem as FS
import qualified Server.Model as Model
import Server.Model.AgdaLib (AgdaLib, agdaLibFromFile, agdaLibName)
import Server.Model.InstalledLibsFile (InstalledLibsFile)
import qualified Server.Model.InstalledLibsFile as InstalledLibsFile

byFileId :: FS.FileId -> ServerM (Maybe AgdaLib)
byFileId agdaLibFileId = do
  model <- askModel
  provider <- askFilesystemProvider
  caseMaybeM
    (fileIdToAgdaLibFile provider agdaLibFileId)
    (return Nothing)
    $ \agdaLibFile ->
      case Model.getKnownAgdaLib (agdaLibFile ^. libName) model of
        Just lib -> return $ Just lib
        Nothing -> do
          lib <- agdaLibFromFile agdaLibFile agdaLibFileId
          modifyModel $ Model.withAgdaLib lib
          return $ Just lib

byLibName :: LibName -> [AgdaLib] -> Maybe AgdaLib
byLibName libName = find (\lib -> libName == lib ^. agdaLibName)

fileIdToAgdaLibFile :: (FS.Provider p) => p -> FS.FileId -> ServerM (Maybe AgdaLibFile)
fileIdToAgdaLibFile provider fileId = do
  agdaLibFile <- parseLibFile provider fileId
  case agdaLibFile of
    Nothing -> return Nothing
    Just agdaLibFile -> do
      let (result, _warnings) = runP agdaLibFile
      return $ maybeRight result

installedLibraries :: Maybe FS.FileId -> ServerM [AgdaLib]
installedLibraries overrideLibFile' = do
  provider <- askFilesystemProvider
  let overrideLibFile = FS.toFileId <$> overrideLibFile'
  libsFile <- librariesFile provider overrideLibFile
  let entries = maybe [] (^. InstalledLibsFile.entries) libsFile
  let entries' = nubOn (^. InstalledLibsFile.entryLibFileId) entries
  mapMaybeM
    (\entry -> byFileId (entry ^. InstalledLibsFile.entryLibFileId))
    entries'

librariesFile :: (FS.MonadFilesystem m, FS.Provider p) => p -> Maybe FS.FileId -> m (Maybe InstalledLibsFile)
librariesFile provider overrideLibFile = do
  agdaDir <- liftIO $ LocalFilePath <$> getAgdaAppDir
  defaults <- defaultLibraryFileIds agdaDir
  let candidates = toList $ maybe defaults singleton overrideLibFile
  libsFiles <- mapMaybeM (InstalledLibsFile.fromFileId provider) candidates
  return $ listToMaybe libsFiles
