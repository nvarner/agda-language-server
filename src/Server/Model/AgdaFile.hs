module Server.Model.AgdaFile
  ( AgdaFile,
    emptyAgdaFile,
    agdaFileSymbols,
    agdaFileRefs,
    insertSymbolInfo,
    insertRef,
    mergeSymbols,
  )
where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Pretty, pretty, prettyAssign, prettyMap, text, vcat)
import Agda.Utils.Lens (Lens', over, (<&>), (^.))
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (Endo, appEndo))
import Server.Model.Symbol (Ref, SymbolInfo)

data AgdaFile = AgdaFile
  { _agdaFileSymbols :: !(Map A.QName SymbolInfo),
    _agdaFileRefs :: !(Map A.QName [Ref])
  }

instance Pretty AgdaFile where
  pretty agdaFile =
    vcat
      [ prettyAssign (text "symbols", prettyMap $ Map.toList $ agdaFile ^. agdaFileSymbols),
        prettyAssign (text "refs", prettyMap $ Map.toList $ agdaFile ^. agdaFileRefs)
      ]

emptyAgdaFile :: AgdaFile
emptyAgdaFile = AgdaFile Map.empty Map.empty

agdaFileSymbols :: Lens' AgdaFile (Map A.QName SymbolInfo)
agdaFileSymbols f a = f (_agdaFileSymbols a) <&> \x -> a {_agdaFileSymbols = x}

agdaFileRefs :: Lens' AgdaFile (Map A.QName [Ref])
agdaFileRefs f a = f (_agdaFileRefs a) <&> \x -> a {_agdaFileRefs = x}

insertSymbolInfo ::
  A.QName ->
  SymbolInfo ->
  AgdaFile ->
  AgdaFile
insertSymbolInfo name symbolInfo =
  over agdaFileSymbols $ Map.insertWith (<>) name symbolInfo

insertRef :: A.AmbiguousQName -> Ref -> AgdaFile -> AgdaFile
insertRef ambiguousName ref =
  over agdaFileRefs $
    appEndo $
      foldMap (\name -> Endo $ Map.insertWith (<>) name [ref]) (A.unAmbQ ambiguousName)

mergeSymbols :: A.QName -> A.QName -> AgdaFile -> AgdaFile
mergeSymbols old new file =
  file
    & over
      agdaFileSymbols
      ( \symbols ->
          let (oldSymbolInfo, symbols') =
                Map.updateLookupWithKey (\_ _ -> Nothing) old symbols
           in Map.alter (<> oldSymbolInfo) new symbols'
      )
    & over
      agdaFileRefs
      ( \refs ->
          let (oldRefs, refs') =
                Map.updateLookupWithKey (\_ _ -> Nothing) old refs
           in Map.alter (<> oldRefs) new refs'
      )
