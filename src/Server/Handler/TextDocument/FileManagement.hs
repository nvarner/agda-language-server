module Server.Handler.TextDocument.FileManagement
  ( didOpenHandler,
    didCloseHandler,
    didSaveHandler,
  )
where

import Agda.Interaction.FindFile (SourceFile (SourceFile))
import qualified Agda.Interaction.Imports.More as Imp
import Agda.TypeChecking.Monad (MonadTCM (liftTCM))
import Agda.Utils.Lens ((^.))
import Control.Monad.Trans (lift)
import Data.Strict (Strict (toLazy))
import Indexer (indexFile)
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidAbsolutePath)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Server as VFS
import qualified Language.LSP.VFS as VFS
import Monad (ServerM, modifyModel)
import qualified Server.Model as Model
import Server.Model.Monad (notificationHandlerWithAgdaLib)

didOpenHandler :: LSP.Handlers ServerM
didOpenHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidOpen $ \notification uri -> do
  sourceFile <- SourceFile <$> uriToPossiblyInvalidAbsolutePath uri
  let sourceText = toLazy $ notification ^. LSP.params . LSP.textDocument . LSP.text
  src <- liftTCM $ Imp.parseVirtualSource sourceFile sourceText
  agdaFile <- indexFile src
  lift $ modifyModel $ Model.setAgdaFile uri agdaFile

didCloseHandler :: LSP.Handlers ServerM
didCloseHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidClose $ \notification uri -> do
  lift $ modifyModel $ Model.deleteAgdaFile uri

didSaveHandler :: LSP.Handlers ServerM
didSaveHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidSave $ \notification uri -> do
  sourceFile <- SourceFile <$> uriToPossiblyInvalidAbsolutePath uri
  virtualFile <- lift $ VFS.getVirtualFile uri
  case virtualFile of
    Nothing -> return ()
    Just virtualFile -> do
      let sourceText = toLazy $ VFS.virtualFileText virtualFile
      src <- liftTCM $ Imp.parseVirtualSource sourceFile sourceText
      agdaFile <- indexFile src
      lift $ modifyModel $ Model.setAgdaFile uri agdaFile
