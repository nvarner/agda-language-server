module Server.Handler.TextDocument.DocumentSymbol (documentSymbolHandler) where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Utils.Maybe (fromMaybe, mapMaybe)
import Control.Monad (forM)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (ServerM)
import Server.Model.AgdaFile (AgdaFile, agdaFileSymbols, defNameRange, symbolByName, symbolsByParent)
import Server.Model.Handler (requestHandlerWithAgdaFile)
import Server.Model.Monad (MonadAgdaFile (askAgdaFile), useAgdaFile)
import Server.Model.Symbol (SymbolInfo (symbolName), lspSymbolKind, symbolShortName, symbolType)

documentSymbolHandler :: LSP.Handlers ServerM
documentSymbolHandler = requestHandlerWithAgdaFile LSP.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
  file <- askAgdaFile
  let symbols = symbolsByParent file
  let topLevelNames = fromMaybe [] $ Map.lookup Nothing symbols
  let topLevelSymbols = mapMaybe (symbolByName file) topLevelNames
  topLevelDocumentSymbols <- lift $ forM topLevelSymbols $ symbolToDocumentSymbol file symbols
  responder $ Right $ LSP.InR $ LSP.InL topLevelDocumentSymbols

symbolToDocumentSymbol :: AgdaFile -> Map (Maybe A.QName) [A.QName] -> SymbolInfo -> ServerM LSP.DocumentSymbol
symbolToDocumentSymbol file symbolsByParent symbol = do
  let name = symbolName symbol
  let range = defNameRange file name
  let childrenNames = fromMaybe [] $ Map.lookup (Just name) symbolsByParent
  let childrenSymbols = mapMaybe (symbolByName file) childrenNames
  childrenDocumentSymbols <-
    forM childrenSymbols $
      symbolToDocumentSymbol file symbolsByParent
  return $
    LSP.DocumentSymbol
      (symbolShortName symbol)
      (Text.pack <$> symbolType symbol)
      (lspSymbolKind symbol)
      Nothing
      Nothing
      range
      range
      (Just childrenDocumentSymbols)
