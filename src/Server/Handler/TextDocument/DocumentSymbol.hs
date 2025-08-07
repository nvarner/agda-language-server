module Server.Handler.TextDocument.DocumentSymbol (documentSymbolHandler) where

import Agda.Utils.Lens ((^.))
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as LSP
import Monad (ServerM)
import Options (Config)
import Server.Handler.Monad (useAgdaFile, withAgdaFile)
import qualified Server.Model as Model
import Server.Model.AgdaFile (agdaFileSymbols)

documentSymbolHandler :: LSP.Handlers (ServerM (LspM Config))
documentSymbolHandler = withAgdaFile LSP.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
  symbols <- useAgdaFile agdaFileSymbols
  return ()
