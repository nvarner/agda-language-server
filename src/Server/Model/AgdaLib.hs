module Server.Model.AgdaLib
  ( AgdaLib (AgdaLib),
    initAgdaLib,
    agdaLibIncludes,
    agdaLibTcStateRef,
    agdaLibTcEnv,
    isAgdaLibForUri,
  )
where

import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.IORef (IORef, newIORef)
import Agda.Utils.Lens (Lens', (<&>), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Server.Model.AgdaFile (AgdaFile)

data AgdaLib = AgdaLib
  { _agdaLibIncludes :: ![LSP.NormalizedUri],
    _agdaLibTcStateRef :: !(IORef TCM.TCState),
    _agdaLibTcEnv :: !TCM.TCEnv
  }

initAgdaLib :: (MonadIO m) => m AgdaLib
initAgdaLib = do
  tcStateRef <- liftIO $ newIORef TCM.initState
  let tcEnv = TCM.initEnv
  return $ AgdaLib [] tcStateRef tcEnv

agdaLibIncludes :: Lens' AgdaLib [LSP.NormalizedUri]
agdaLibIncludes f a = f (_agdaLibIncludes a) <&> \x -> a {_agdaLibIncludes = x}

agdaLibTcStateRef :: Lens' AgdaLib (IORef TCM.TCState)
agdaLibTcStateRef f a = f (_agdaLibTcStateRef a) <&> \x -> a {_agdaLibTcStateRef = x}

agdaLibTcEnv :: Lens' AgdaLib TCM.TCEnv
agdaLibTcEnv f a = f (_agdaLibTcEnv a) <&> \x -> a {_agdaLibTcEnv = x}

isAgdaLibForUri :: AgdaLib -> LSP.NormalizedUri -> Bool
isAgdaLibForUri agdaLib uri = any (`LSP.isUriAncestorOf` uri) (agdaLib ^. agdaLibIncludes)
