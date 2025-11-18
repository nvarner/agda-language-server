module Server.Model
  ( Model (Model),
    empty,
    getKnownAgdaLib,
    withAgdaLib,
    getAgdaFile,
    setAgdaFile,
    deleteAgdaFile,
  )
where

import Agda.Utils.Lens (Lens', over)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import Data.Functor ((<&>))
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

agdaLibs :: Lens' Model [AgdaLib]
agdaLibs f a = f (_modelAgdaLibs a) <&> \x -> a {_modelAgdaLibs = x}

agdaFiles :: Lens' Model (Map LSP.NormalizedUri AgdaFile)
agdaFiles f a = f (_modelAgdaFiles a) <&> \x -> a {_modelAgdaFiles = x}

getKnownAgdaLib :: LSP.NormalizedUri -> Model -> Maybe AgdaLib
getKnownAgdaLib uri = find (`isAgdaLibForUri` uri) . _modelAgdaLibs

-- | Add an 'AgdaLib' to the model
withAgdaLib :: AgdaLib -> Model -> Model
withAgdaLib lib model = model {_modelAgdaLibs = lib : _modelAgdaLibs model}

getAgdaFile :: LSP.NormalizedUri -> Model -> Maybe AgdaFile
getAgdaFile uri = Map.lookup uri . _modelAgdaFiles

setAgdaFile :: LSP.NormalizedUri -> AgdaFile -> Model -> Model
setAgdaFile uri agdaFile = over agdaFiles $ Map.insert uri agdaFile

deleteAgdaFile :: LSP.NormalizedUri -> Model -> Model
deleteAgdaFile uri = over agdaFiles $ Map.delete uri
