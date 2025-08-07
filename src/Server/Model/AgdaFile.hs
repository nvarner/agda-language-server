module Server.Model.AgdaFile
  ( AgdaFile,
    emptyAgdaFile,
    agdaFileSymbols,
  )
where

import qualified Agda.Syntax.Abstract as A
import Agda.Utils.Lens (Lens', (<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Server.Model.Symbol (Ref, SymbolInfo)

data AgdaFile = AgdaFile
  { _agdaFileSymbols :: !(Map A.QName SymbolInfo),
    _agdaFileRefs :: !(Map A.QName [Ref])
  }

emptyAgdaFile :: AgdaFile
emptyAgdaFile = AgdaFile Map.empty Map.empty

agdaFileSymbols :: Lens' AgdaFile (Map A.QName SymbolInfo)
agdaFileSymbols f a = f (_agdaFileSymbols a) <&> \x -> a {_agdaFileSymbols = x}
