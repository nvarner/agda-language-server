module Server.Model.AgdaFile
  ( AgdaFile,
    emptyAgdaFile,
    agdaFileSymbols,
    agdaFileRefs,
    insertSymbolInfo,
    insertRef,
  )
where

import qualified Agda.Syntax.Abstract as A
import Agda.Utils.Lens (Lens', over, (<&>))
import Control.Monad (forM)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (Endo, appEndo))
import Server.Model.Symbol (Ref, SymbolInfo)

data AgdaFile = AgdaFile
  { _agdaFileSymbols :: !(Map A.QName SymbolInfo),
    _agdaFileRefs :: !(Map A.QName [Ref])
  }

emptyAgdaFile :: AgdaFile
emptyAgdaFile = AgdaFile Map.empty Map.empty

agdaFileSymbols :: Lens' AgdaFile (Map A.QName SymbolInfo)
agdaFileSymbols f a = f (_agdaFileSymbols a) <&> \x -> a {_agdaFileSymbols = x}

agdaFileRefs :: Lens' AgdaFile (Map A.QName [Ref])
agdaFileRefs f a = f (_agdaFileRefs a) <&> \x -> a {_agdaFileRefs = x}

insertSymbolInfo ::
  (SymbolInfo -> SymbolInfo -> SymbolInfo) ->
  A.QName ->
  SymbolInfo ->
  AgdaFile ->
  AgdaFile
insertSymbolInfo update name symbolInfo = over agdaFileSymbols $ Map.insertWith update name symbolInfo

insertRef :: A.AmbiguousQName -> Ref -> AgdaFile -> AgdaFile
insertRef ambiguousName ref =
  over agdaFileRefs $
    appEndo $
      foldMap (\name -> Endo $ Map.insertWith (<>) name [ref]) (A.unAmbQ ambiguousName)

-- Map.insertWith (<>) name [ref]
