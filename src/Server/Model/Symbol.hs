module Server.Model.Symbol (SymbolInfo, Ref) where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Position (Position, PositionWithoutFile)
import qualified Language.LSP.Protocol.Types as LSP

data SymbolKind
  = Con
  | CoCon
  | Field
  | PatternSyn
  | GeneralizeVar
  | Macro
  | Data
  | Record
  | Fun
  | Axiom
  | Prim
  | Module
  | Param
  | Local
  | Unknown
  deriving (Show)

data SymbolInfo = SymbolInfo
  { _symbolKind :: !SymbolKind,
    _symbolType :: !(Maybe String),
    _symbolParent :: !(Maybe A.QName)
  }

data Ref = Ref
  { _refSymbol :: !A.QName,
    _refRange :: !LSP.Range
  }
