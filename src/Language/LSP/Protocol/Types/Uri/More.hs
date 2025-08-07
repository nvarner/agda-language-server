module Language.LSP.Protocol.Types.Uri.More
  ( getNormalizedUri,
    isUriAncestorOf,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
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
