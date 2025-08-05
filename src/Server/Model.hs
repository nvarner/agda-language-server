module Server.Model (Model, empty) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Types as LSP
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib)

data Model = Model
  { _modelAgdaLibs :: !([AgdaLib]),
    _modelAgdaFiles :: !(Map LSP.Uri AgdaFile)
  }

empty :: Model
empty = Model [] Map.empty
