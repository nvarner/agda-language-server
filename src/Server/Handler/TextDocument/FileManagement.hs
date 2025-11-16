module Server.Handler.TextDocument.FileManagement
  ( didOpenHandler,
    didCloseHandler,
    didSaveHandler,
  )
where

import Agda.Interaction.FindFile (SourceFile (SourceFile))
import qualified Agda.Interaction.Imports.More as Imp
import Agda.Interaction.Imports.Virtual (parseVSource, vSrcFromUri)
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.TypeChecking.Monad (MonadTCM (liftTCM))
import Agda.Utils.Lens ((^.))
import Control.Monad.Trans (lift)
import Data.Strict (Strict (toLazy))
import qualified Data.Text as Text
import Indexer (indexFile)
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidAbsolutePath)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Monad (ServerM, modifyModel, modifyVfsIndex)
import qualified Server.Model as Model
import Server.Model.Handler (notificationHandlerWithAgdaLib, takeOverNotificationHandlerWithAgdaLib)
import qualified Server.VfsIndex as VFSIndex
import qualified Server.VfsIndex as VfsIndex

didOpenHandler :: LSP.Handlers ServerM
didOpenHandler = LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $ \notification -> do
  let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
  modifyVfsIndex $ VfsIndex.onOpen uri
  takeOverNotificationHandlerWithAgdaLib notification $ \uri notification -> do
    vfile <- lift $ LSP.getVirtualFile uri
    case vfile of
      Nothing -> return ()
      Just vfile -> do
        vSourceFile <- vSrcFromUri uri vfile
        src <- parseVSource vSourceFile
        lift $ LSP.sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Info $ Text.pack $ prettyShow src
        agdaFile <- indexFile src
        lift $ modifyModel $ Model.setAgdaFile uri agdaFile

didCloseHandler :: LSP.Handlers ServerM
didCloseHandler = LSP.notificationHandler LSP.SMethod_TextDocumentDidClose $ \notification -> do
  let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
  modifyVfsIndex $ VFSIndex.onClose uri
  modifyModel $ Model.deleteAgdaFile $ LSP.toNormalizedUri uri

didSaveHandler :: LSP.Handlers ServerM
didSaveHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidSave $ \uri notification -> do
  vfile <- lift $ LSP.getVirtualFile uri
  case vfile of
    Nothing -> return ()
    Just vfile -> do
      vSourceFile <- vSrcFromUri uri vfile
      src <- parseVSource vSourceFile
      lift $ LSP.sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Info $ Text.pack $ prettyShow src
      agdaFile <- indexFile src
      lift $ modifyModel $ Model.setAgdaFile uri agdaFile
