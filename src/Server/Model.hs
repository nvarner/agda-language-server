module Server.Model (Model (Model), empty, getKnownAgdaLib, getAgdaLib, getAgdaFile) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Types as LSP
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib, initAgdaLib, isAgdaLibForUri)

data Model = Model
  { _modelAgdaLibs :: !([AgdaLib]),
    _modelAgdaFiles :: !(Map LSP.NormalizedUri AgdaFile)
  }

empty :: Model
empty = Model [] Map.empty

getKnownAgdaLib :: LSP.NormalizedUri -> Model -> Maybe AgdaLib
getKnownAgdaLib uri = find (`isAgdaLibForUri` uri) . _modelAgdaLibs

getAgdaLib :: (MonadIO m) => LSP.NormalizedUri -> Model -> m AgdaLib
getAgdaLib uri = maybe initAgdaLib return . getKnownAgdaLib uri

getAgdaFile :: LSP.NormalizedUri -> Model -> Maybe AgdaFile
getAgdaFile uri = Map.lookup uri . _modelAgdaFiles
