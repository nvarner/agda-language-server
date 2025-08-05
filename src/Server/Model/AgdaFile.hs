module Server.Model.AgdaFile (AgdaFile) where

import qualified Agda.Syntax.Abstract as A
import Data.Map (Map)
import Server.Model.Symbol (Ref, SymbolInfo)

data AgdaFile = AgdaFile
  { _agdaFileSymbols :: !(Map A.QName SymbolInfo),
    _agdaFileRefs :: !(Map A.QName [Ref])
  }
