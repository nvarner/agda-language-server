module Server.Handler.TextDocument.FileManagement
  ( didOpenHandler,
    didCloseHandler,
    didSaveHandler,
  )
where

import Agda.Interaction.FindFile (SourceFile (SourceFile))
import qualified Agda.Interaction.Imports.More as Imp
import Agda.Interaction.Imports.Virtual (parseVSource, vSrcFromUri)
import Agda.TypeChecking.Monad (MonadTCM (liftTCM))
import Agda.Utils.Lens ((^.))
import Control.Monad.Trans (lift)
import Data.Strict (Strict (toLazy))
import Indexer (indexFile)
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidAbsolutePath)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Monad (ServerM, modifyModel)
import qualified Server.Model as Model
import Server.Model.Handler (notificationHandlerWithAgdaLib)

didOpenHandler :: LSP.Handlers ServerM
didOpenHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidOpen $ \uri notification -> do
  vfile <- lift $ LSP.getVirtualFile uri
  case vfile of
    Nothing -> return ()
    Just vfile -> do
      vSourceFile <- vSrcFromUri uri vfile
      src <- liftTCM $ parseVSource vSourceFile
      agdaFile <- indexFile src
      lift $ modifyModel $ Model.setAgdaFile uri agdaFile

didCloseHandler :: LSP.Handlers ServerM
didCloseHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidClose $ \uri notification -> do
  lift $ modifyModel $ Model.deleteAgdaFile uri

didSaveHandler :: LSP.Handlers ServerM
didSaveHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidSave $ \uri notification -> do
  vfile <- lift $ LSP.getVirtualFile uri
  case vfile of
    Nothing -> return ()
    Just vfile -> do
      vSourceFile <- vSrcFromUri uri vfile
      src <- liftTCM $ parseVSource vSourceFile
      agdaFile <- indexFile src
      lift $ modifyModel $ Model.setAgdaFile uri agdaFile
