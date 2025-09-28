module Language.LSP.Protocol.Types.Uri.More
  ( getNormalizedUri,
    isUriAncestorOf,
    uriToPossiblyInvalidAbsolutePath,
  )
where

import Agda.Utils.FileName (AbsolutePath (AbsolutePath), absolute)
import Agda.Utils.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.LSP.Protocol.Types (uriToFilePath)
import qualified Language.LSP.Protocol.Types as LSP

getNormalizedUri :: LSP.NormalizedUri -> Text
getNormalizedUri = LSP.getUri . LSP.fromNormalizedUri

-- | Determine if the first URI is an ancestor of the second.
--
-- This is a heuristic implementation and may need replacement if the heuristic
-- leads to bugs.
isUriAncestorOf :: LSP.NormalizedUri -> LSP.NormalizedUri -> Bool
isUriAncestorOf ancestor descendant =
  getNormalizedUri ancestor `Text.isPrefixOf` getNormalizedUri descendant

uriToPossiblyInvalidAbsolutePath :: (MonadIO m) => LSP.NormalizedUri -> m AbsolutePath
uriToPossiblyInvalidAbsolutePath uri = do
  case LSP.uriToFilePath $ LSP.fromNormalizedUri uri of
    Just path -> liftIO $ absolute path
    Nothing -> return $ uriToInvalidAbsolutePath uri

uriToInvalidAbsolutePath :: LSP.NormalizedUri -> AbsolutePath
uriToInvalidAbsolutePath = AbsolutePath . getNormalizedUri
