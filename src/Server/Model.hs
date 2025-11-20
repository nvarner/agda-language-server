module Server.Model
  ( Model (Model),
    empty,
    getKnownAgdaLib,
    getKnownAgdaProject,
    withAgdaLib,
    withAgdaProject,
    getAgdaFile,
    setAgdaFile,
    deleteAgdaFile,
  )
where

import Agda.Utils.Lens (Lens', over, (^.))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Types as LSP
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib, initAgdaLib, isAgdaLibForUri)
import Server.Model.AgdaProject (AgdaProject)
import qualified Server.Model.AgdaProject as AgdaProject

data Model = Model
  { _modelAgdaLibs :: ![AgdaLib],
    _modelAgdaProjects :: ![AgdaProject],
    _modelAgdaFiles :: !(Map LSP.NormalizedUri AgdaFile)
  }

empty :: Model
empty = Model [] [] Map.empty

agdaLibs :: Lens' Model [AgdaLib]
agdaLibs f a = f (_modelAgdaLibs a) <&> \x -> a {_modelAgdaLibs = x}

agdaProjects :: Lens' Model [AgdaProject]
agdaProjects f a = f (_modelAgdaProjects a) <&> \x -> a {_modelAgdaProjects = x}

agdaFiles :: Lens' Model (Map LSP.NormalizedUri AgdaFile)
agdaFiles f a = f (_modelAgdaFiles a) <&> \x -> a {_modelAgdaFiles = x}

getKnownAgdaLib :: LSP.NormalizedUri -> Model -> Maybe AgdaLib
getKnownAgdaLib uri model = find (`isAgdaLibForUri` uri) $ model ^. agdaLibs

getKnownAgdaProject :: LSP.NormalizedUri -> Model -> Maybe AgdaProject
getKnownAgdaProject uri model =
  find ((`isAgdaLibForUri` uri) . (^. AgdaProject.agdaLib)) $ model ^. agdaProjects

-- | Add an 'AgdaLib' to the model
withAgdaLib :: AgdaLib -> Model -> Model
withAgdaLib lib = over agdaLibs (lib :)

-- | Add an 'AgdaProject' to the model
withAgdaProject :: AgdaProject -> Model -> Model
withAgdaProject project = over agdaProjects (project :)

getAgdaFile :: LSP.NormalizedUri -> Model -> Maybe AgdaFile
getAgdaFile uri = Map.lookup uri . _modelAgdaFiles

setAgdaFile :: LSP.NormalizedUri -> AgdaFile -> Model -> Model
setAgdaFile uri agdaFile = over agdaFiles $ Map.insert uri agdaFile

deleteAgdaFile :: LSP.NormalizedUri -> Model -> Model
deleteAgdaFile uri = over agdaFiles $ Map.delete uri
