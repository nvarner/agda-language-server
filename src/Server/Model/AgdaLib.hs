module Server.Model.AgdaLib
  ( AgdaLib,
    isAgdaLibForUri,
  )
where

import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.IORef (IORef)
import Agda.Utils.Lens (Lens', (<&>), (^.))
import Data.Map (Map)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Server.Model.AgdaFile (AgdaFile)

data AgdaLib = AgdaLib
  { _agdaLibIncludes :: ![LSP.NormalizedUri],
    _agdaLibTcState :: !(IORef TCM.TCState),
    _agdaLibTcEnv :: !TCM.TCEnv
  }

agdaLibIncludes :: Lens' AgdaLib [LSP.NormalizedUri]
agdaLibIncludes f a = f (_agdaLibIncludes a) <&> \x -> a {_agdaLibIncludes = x}

isAgdaLibForUri :: AgdaLib -> LSP.NormalizedUri -> Bool
isAgdaLibForUri agdaLib uri = any (`LSP.isUriAncestorOf` uri) (agdaLib ^. agdaLibIncludes)
