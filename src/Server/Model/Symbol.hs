module Server.Model.Symbol
  ( SymbolKind (..),
    SymbolInfo (..),
    RefKind (..),
    Ref (..),
  )
where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Doc, Pretty, comma, parensNonEmpty, pretty, pshow, text, (<+>))
import Control.Applicative ((<|>))
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.More ()

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
  deriving (Show, Eq)

instance Pretty SymbolKind where
  pretty = pshow

instance Semigroup SymbolKind where
  Unknown <> k = k
  k <> _k = k

data SymbolInfo = SymbolInfo
  { symbolKind :: !SymbolKind,
    symbolType :: !(Maybe String),
    symbolParent :: !(Maybe A.QName)
  }

instance Pretty SymbolInfo where
  pretty symbolInfo =
    pretty (symbolKind symbolInfo)
      <+> parensNonEmpty (pretty $ symbolType symbolInfo)

instance Semigroup SymbolInfo where
  new <> old =
    SymbolInfo
      (symbolKind old <> symbolKind new)
      (symbolType old <|> symbolType new)
      (symbolParent old <|> symbolParent new)

data RefKind
  = -- | The symbol is being declared. There should be at most one declaration
    -- for any given symbol (in correct Agda code). Roughly speaking, this is
    -- the "single most important defining reference"
    --
    -- For example, a function's name in its signature
    Decl
  | -- | The symbol is being defined, but is not being declared in the sense
    -- that @Decl@ would apply. Typically, this means the definition may be
    -- split into several parts, and this is one of the "less important" parts
    --
    -- For example, a function's name in the LHS of a clause in its definition
    Def
  | -- | The symbol is (expected to be) already defined and is being used
    --
    -- For example, a function's name in function application
    Usage
  | -- | The symbol is being imported here
    Import
  deriving (Show, Eq)

instance Pretty RefKind where
  pretty = pshow

data Ref = Ref
  { refKind :: !RefKind,
    refRange :: !LSP.Range,
    refIsAmbiguous :: !Bool
  }

prettyAmbiguity :: Ref -> Doc
prettyAmbiguity ref =
  if refIsAmbiguous ref
    then text "ambiguous"
    else text "unambiguous"

instance Pretty Ref where
  pretty ref =
    ((prettyAmbiguity ref <+> pretty (refKind ref)) <> comma)
      <+> pretty (refRange ref)
