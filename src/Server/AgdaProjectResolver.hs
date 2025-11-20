module Server.AgdaProjectResolver (findAgdaProject) where

import Monad (ServerM, askFilesystemProvider, askModel, modifyModel)
import Server.AgdaLibResolver (findAgdaLib)
import qualified Server.Filesystem as FS
import qualified Server.Model as Model
import Server.Model.AgdaProject (AgdaProject)
import qualified Server.Model.AgdaProject as AgdaProject

-- | Find cached 'AgdaProject', or else make one from @.agda-lib@ files on the
-- file system, or else provide a default
findAgdaProject :: (FS.IsFileId f) => f -> ServerM AgdaProject
findAgdaProject isFileId = do
  let fileId = FS.toFileId isFileId
  let uri = FS.fileIdToUri fileId
  model <- askModel
  case Model.getKnownAgdaProject uri model of
    Just project -> return project
    Nothing -> do
      lib <- findAgdaLib fileId
      project <- AgdaProject.new lib
      modifyModel $ Model.withAgdaProject project
      return project
