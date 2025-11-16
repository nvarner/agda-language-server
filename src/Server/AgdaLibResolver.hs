module Server.AgdaLibResolver (findAgdaLib) where

import Agda.Interaction.Library (AgdaLibFile)
import Agda.Interaction.Library.Parse.More (parseLibFile, runP)
import Agda.Utils.Either (maybeRight)
import Agda.Utils.Maybe (caseMaybe, ifJust, listToMaybe)
import qualified Language.LSP.Protocol.Types as LSP
import Monad (ServerM, askFilesystemProvider, askModel, modifyModel)
import qualified Server.Filesystem as FS
import qualified Server.Model as Model
import Server.Model.AgdaLib (AgdaLib, agdaLibFromFile, initAgdaLib)

-- | Find cached 'AgdaLib', or else make one from @.agda-lib@ files on the file
-- system, or else provide a default
findAgdaLib :: LSP.NormalizedUri -> ServerM AgdaLib
findAgdaLib uri = do
  model <- askModel
  case Model.getKnownAgdaLib uri model of
    Just lib -> return lib
    Nothing -> do
      provider <- askFilesystemProvider
      result <- searchFilesystemForAgdaLib provider uri
      lib <- case result of
        Just lib -> return lib
        Nothing -> initAgdaLib
      modifyModel $ Model.withAgdaLib lib
      return lib

searchFilesystemForAgdaLib :: (FS.Provider p, FS.IsFileId f) => p -> f -> ServerM (Maybe AgdaLib)
searchFilesystemForAgdaLib provider agdaIsFileId = do
  let agdaFileId = FS.toFileId agdaIsFileId
  libFileId <- searchForAgdaLibFile provider agdaFileId
  case libFileId of
    Nothing -> return Nothing
    Just fileId -> do
      agdaLibFile <- fileIdToAgdaLibFile provider fileId
      case agdaLibFile of
        Nothing -> return Nothing
        Just agdaLibFile -> do
          agdaLib <- agdaLibFromFile agdaLibFile
          return $ Just agdaLib
  where
    searchForAgdaLibFile :: (FS.Provider p) => p -> FS.FileId -> ServerM (Maybe FS.FileId)
    searchForAgdaLibFile provider childFileId = do
      parentFileId <- FS.fileIdParent childFileId
      caseMaybe parentFileId (return Nothing) $ \parentFileId -> do
        children <- FS.getChildren provider parentFileId
        let candidates = filter (\child -> FS.fileIdExtension child == ".agda-lib") children
        case listToMaybe candidates of
          Nothing -> searchForAgdaLibFile provider parentFileId
          Just candidate -> return $ Just candidate

    fileIdToAgdaLibFile :: (FS.Provider p) => p -> FS.FileId -> ServerM (Maybe AgdaLibFile)
    fileIdToAgdaLibFile provider fileId = do
      agdaLibFile <- parseLibFile provider fileId
      case agdaLibFile of
        Nothing -> return Nothing
        Just agdaLibFile -> do
          let (result, _warnings) = runP agdaLibFile
          return $ maybeRight result
