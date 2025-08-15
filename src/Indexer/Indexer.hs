{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Indexer.Indexer (abstractToIndex) where

import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Common as C
import Agda.Syntax.Common.Pretty (render)
import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Info as Info
import qualified Agda.Syntax.Internal as I
import qualified Agda.Syntax.Literal as Lit
import qualified Agda.Syntax.Scope.Base as Scope
import Agda.Syntax.Translation.ConcreteToAbstract (TopLevelInfo (TopLevelInfo))
import qualified Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.Functor ((<&>))
import Agda.Utils.List1 (List1)
import qualified Agda.Utils.List1 as List1
import Agda.Utils.Maybe (whenJust)
import Agda.Utils.Monad (when)
import Data.Foldable (forM_, traverse_)
import qualified Data.Set as Set
import Data.Void (absurd)
import Indexer.Monad
  ( AmbiguousNameLike (..),
    HasParamNames (..),
    IndexerM,
    NameLike (..),
    NoType (NoType),
    SymbolKindLike (..),
    TypeLike (..),
    UnknownType (UnknownType),
    execIndexerM,
    tellDecl,
    tellDef,
    tellImport,
    tellNamedArgUsage,
    tellParamNames,
    tellUsage,
    withParent,
  )
import qualified Language.LSP.Server as LSP
import Monad (ServerM)
import Options (Config)
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.Monad (WithAgdaLibM)
import Server.Model.Symbol (SymbolKind (..))

abstractToIndex :: TopLevelInfo -> WithAgdaLibM AgdaFile
abstractToIndex (TopLevelInfo decls _scope) = execIndexerM $ index decls

class Indexable a where
  index :: a -> IndexerM ()
  default index :: (Foldable f, Indexable b, a ~ f b) => a -> IndexerM ()
  index = traverse_ index

instance Indexable A.Declaration where
  index = \case
    A.Axiom kindOfName defInfo _argInfo _polarities name type' -> do
      -- TODO: what does the `ArgInfo` mean?
      -- Includes postulates, function declarations
      tellDecl name kindOfName type'
      index defInfo
      index type'
    A.Generalize _generalizeVars defInfo _argInfo name type' -> do
      -- TODO: what does the `ArgInfo` mean?
      tellDecl name GeneralizeVar type'
      index defInfo
      index type'
    A.Field defInfo name type' -> do
      tellDecl name Field type'
      index defInfo
      index type'
    A.Primitive defInfo name type' -> do
      tellDecl name Prim type'
      index defInfo
      index type'
    A.Mutual _mutualInfo decls ->
      index decls
    A.Section _range _erased moduleName genTel decls -> do
      tellDecl moduleName Module NoType
      tellParamNames moduleName genTel
      index genTel
      withParent moduleName $ do
        index decls
    A.Apply _moduleInfo _erased moduleName moduleApp _scopeCopyInfo importDirective -> do
      tellUsage moduleName
      index moduleApp
      index importDirective
    A.Import _moduleInfo moduleName importDirective -> do
      tellUsage moduleName
      index importDirective
    A.Pragma _range pragma ->
      index pragma
    A.Open _moduleInfo moduleName importDirective -> do
      tellUsage moduleName
      index importDirective
    A.FunDef defInfo name clauses -> do
      -- tellDef name Fun UnknownType
      -- index defInfo
      withParent name $ do
        index clauses
    A.DataSig defInfo _erased name genTel type' -> do
      tellDecl name Data type'
      index defInfo
      index genTel
      index type'
    A.DataDef defInfo name _univCheck dataDefParams constructors -> do
      tellDef name Data UnknownType
      index defInfo
      index dataDefParams
      index constructors
    A.RecSig defInfo _erased name genTel type' -> do
      tellDecl name Record type'
      index defInfo
      index genTel
      index type'
    A.RecDef defInfo name _univCheck recDirectives dataDefParams _type' decls -> do
      -- The type associated with a `RecDef` is a Pi type including the record's
      -- fields, which is not what we want. The `RecSig` does have the type we
      -- want, so we use that instead.
      tellDef name Record UnknownType
      index defInfo
      index dataDefParams
      withParent name $ do
        index recDirectives
        index decls
    A.PatternSynDef name bindings pat -> do
      tellDecl name PatternSyn UnknownType
      forM_ bindings $ \(C.WithHiding _hiding binding) ->
        tellDef binding Param UnknownType
      let pat' :: A.Pattern = fmap absurd pat
      index pat'
    A.UnquoteDecl _mutualInfo defInfos names expr -> do
      forM_ names $ \name ->
        tellDef name Unknown UnknownType
      index defInfos
      index expr
    A.UnquoteDef defInfos names expr -> do
      forM_ names $ \name ->
        tellDef name Unknown UnknownType
      index defInfos
      index expr
    A.UnquoteData defInfos name _univCheck conDefInfos conNames expr -> do
      tellDef name Data UnknownType
      forM_ conNames $ \conName ->
        tellDef conName Con UnknownType
      index defInfos
      index conDefInfos
      index expr
    A.ScopedDecl _scope decls ->
      index decls
    A.UnfoldingDecl _range names ->
      forM_ names $ \name ->
        tellUsage name

instance Indexable A.Expr where
  index = \case
    A.Var name ->
      tellUsage name
    A.Def' name _suffix ->
      tellUsage name
    A.Proj _origin ambiguousName ->
      tellUsage ambiguousName
    A.Con ambiguousName ->
      tellUsage ambiguousName
    A.PatternSyn ambiguousName ->
      tellUsage ambiguousName
    A.Macro name ->
      tellUsage name
    A.Lit _exprInfo lit ->
      index lit
    A.QuestionMark _metaInfo _interactionId ->
      return ()
    A.Underscore _metaInfo ->
      return ()
    A.Dot _exprInfo expr' ->
      index expr'
    A.App _appInfo exprF exprArg -> do
      index exprF
      case funNameFromExpr exprF of
        Just name -> indexNamedArgs name [exprArg]
        Nothing ->
          when (C.argInfoOrigin (C.argInfo exprArg) == C.UserWritten) $ do
            index $ C.namedThing $ C.unArg exprArg
    A.WithApp _appInfo exprF exprArgs -> do
      index exprF
      index exprArgs
    A.Lam _exprInfo binding body -> do
      index binding
      index body
    A.AbsurdLam _exprInfo _hiding ->
      return ()
    A.ExtendedLam _exprInfo defInfo _erased _generatedFn clauses -> do
      index defInfo
      index clauses
    A.Pi _exprInfo tel type' -> do
      index tel
      index type'
    A.Generalized _generalizeVars type' -> do
      index type'
    A.Fun _exprInfo dom codom -> do
      index dom
      index codom
    A.Let _exprInfo bindings body -> do
      index bindings
      index body
    A.Rec _exprInfo recAssigns ->
      index recAssigns
    A.RecUpdate _exprInfo exprRec assigns -> do
      index exprRec
      index assigns
    A.ScopedExpr _scope expr' ->
      index expr'
    A.Quote _exprInfo -> return ()
    A.QuoteTerm _exprInfo -> return ()
    A.Unquote _exprInfo -> return ()
    A.DontCare expr' -> index expr'

funNameFromExpr :: A.Expr -> Maybe A.AmbiguousQName
funNameFromExpr = \case
  A.Var name -> Just $ A.unambiguous $ A.qualify_ name
  A.Def' name _suffix -> Just $ A.unambiguous name
  A.Proj _origin name -> Just name
  A.Con name -> Just name
  A.PatternSyn name -> Just name
  A.Dot _exprInfo expr -> funNameFromExpr expr
  A.App _appInfo exprF _exprArgs -> funNameFromExpr exprF
  A.WithApp _appInfo exprF _exprArgs -> funNameFromExpr exprF
  A.ScopedExpr _scopeInfo expr -> funNameFromExpr expr
  A.DontCare expr -> funNameFromExpr expr
  _noFunName -> Nothing

instance (Indexable a) => Indexable (A.Pattern' a) where
  index = \case
    A.VarP binding -> do
      tellDef binding Local UnknownType
    A.ConP _conPatInfo ambiguousName naps -> do
      tellUsage ambiguousName
      indexNamedArgs ambiguousName naps
    A.ProjP _patInfo _projOrigin ambiguousName ->
      tellUsage ambiguousName
    A.DefP _patInfo ambiguousName naps -> do
      tellUsage ambiguousName
      indexNamedArgs ambiguousName naps
    A.WildP _patInfo -> return ()
    A.AsP _patInfo binding pat' -> do
      tellDef binding Local UnknownType
      index pat'
    A.DotP _patInfo expr ->
      index expr
    A.AbsurdP _patInfo -> return ()
    A.LitP _patInfo lit ->
      index lit
    A.PatternSynP _patInfo ambiguousName naps -> do
      tellUsage ambiguousName
      indexNamedArgs ambiguousName naps
    A.EqualP _patInfo exprPairs ->
      forM_ exprPairs $ \(lhs, rhs) -> do
        index lhs
        index rhs
    A.WithP _patInfo pat' ->
      index pat'
    A.RecP _conPatInfo fieldAssignments ->
      index fieldAssignments
    A.AnnP _patInfo type' pat' -> do
      index type'
      index pat'

instance (Indexable a) => Indexable (Maybe a)

instance (Indexable a) => Indexable [a]

instance (Indexable a) => Indexable (List1 a)

instance (Indexable a) => Indexable (C.Arg a) where
  index (C.Arg argInfo x) =
    when (C.argInfoOrigin argInfo == C.UserWritten) $
      index x

instance Indexable A.TacticAttribute

instance Indexable A.DefInfo where
  index = index . Info.defTactic

indexNamedArgs :: (AmbiguousNameLike n, Indexable a) => n -> [C.NamedArg a] -> IndexerM ()
indexNamedArgs headNameLike args = do
  let headName = toAmbiguousQName headNameLike
  forM_ args $ \(C.Arg argInfo (C.Named maybeName x)) ->
    when (C.argInfoOrigin argInfo == C.UserWritten) $ do
      whenJust maybeName $ tellNamedArgUsage headName
      index x

indexWithExpr :: A.WithExpr -> IndexerM ()
indexWithExpr (C.Named maybeName (C.Arg argInfo expr)) =
  when (C.argInfoOrigin argInfo == C.UserWritten) $ do
    whenJust maybeName $ \(A.BindName name) ->
      tellDef name Param UnknownType
    index expr

instance Indexable Lit.Literal where
  index = \case
    Lit.LitQName name -> tellUsage name
    _otherLit -> return ()

instance (Indexable a) => Indexable (C.FieldAssignment' a) where
  index (C.FieldAssignment fieldCName expr) = do
    scope <- TCM.getScope
    let fieldNames = Scope.anameName <$> Scope.scopeLookup (C.QName fieldCName) scope
    List1.ifNull fieldNames (return ()) $ \fieldNames1 -> do
      let fieldName = A.AmbQ fieldNames1
      tellUsage fieldName
    index expr

instance Indexable A.RecordAssign where
  index = \case
    Left assign ->
      index assign
    Right moduleName ->
      tellUsage moduleName

instance Indexable A.WhereDeclarations where
  index whereDecls = case whereDecls of
    A.WhereDecls (Just moduleName) _ decl -> do
      tellDecl moduleName Module NoType
      withParent moduleName $
        index decl
    A.WhereDecls Nothing _ decl ->
      index decl

instance (Indexable a) => Indexable (A.LHSCore' a) where
  index core = case core of
    A.LHSHead name pats -> do
      tellDef name Param UnknownType
      indexNamedArgs name pats
    A.LHSProj destructor focus pats -> do
      tellUsage destructor
      -- TODO: what does the named arg in `focus` mean?
      indexNamedArgs destructor [focus]
      indexNamedArgs destructor pats
    A.LHSWith lhsHead withPatterns pats -> do
      index lhsHead
      index withPatterns
      -- TODO: what do the named args mean?
      forM_ pats $ \(C.Arg argInfo (C.Named _name pat)) ->
        when (C.argInfoOrigin argInfo == C.UserWritten) $
          index pat

instance Indexable A.LHS where
  index (A.LHS lhsInfo core) = case Info.lhsEllipsis lhsInfo of
    C.ExpandedEllipsis _range _withArgs -> return ()
    C.NoEllipsis -> index core

instance Indexable A.RewriteEqn where
  index eqn = case eqn of
    C.Rewrite exprs ->
      forM_ exprs $ \(_name, expr) ->
        index expr
    C.Invert _generatedFn bindings ->
      forM_ bindings $ \(C.Named maybeName (pat, expr)) -> do
        whenJust maybeName $ \bindName ->
          tellDef bindName Param UnknownType
        index pat
        index expr
    C.LeftLet bindings ->
      forM_ bindings $ \(pat, expr) -> do
        index pat
        index expr

instance Indexable A.RHS where
  index rhs = case rhs of
    A.RHS expr _concrete ->
      index expr
    A.AbsurdRHS ->
      return ()
    A.WithRHS _generatedFn withExprs clauses -> do
      forM_ withExprs indexWithExpr
      index clauses
    A.RewriteRHS rewriteExprs _strippedPats rewriteRhs whereDecls -> do
      index rewriteExprs
      index rewriteRhs
      index whereDecls

instance (Indexable a) => Indexable (A.Clause' a) where
  index (A.Clause lhs _strippedPats rhs whereDecls _catchall) = do
    index lhs
    index rhs
    index whereDecls

instance Indexable A.ModuleApplication where
  index = \case
    A.SectionApp tele moduleName args -> do
      index tele
      tellUsage moduleName
      indexNamedArgs moduleName args
    A.RecordModuleInstance moduleName ->
      tellUsage moduleName

-- Since `HidingDirective' a b` is just `[ImportedName' a b]`, it's much more
-- explicit to give it a special function than try to use instance resolution.
indexHidingDirective :: C.HidingDirective' A.QName A.ModuleName -> IndexerM ()
indexHidingDirective = traverse_ tellUsage

instance Indexable A.Renaming where
  index (C.Renaming fromName toName _toFixity _toRange) = do
    tellUsage fromName
    let toNameKind = case toName of
          C.ImportedModule _moduleName -> Module
          C.ImportedName _name -> Unknown
    tellImport fromName
    -- TODO: better handling of renamed imports
    tellDef toName toNameKind UnknownType

instance Indexable (C.Using' A.QName A.ModuleName) where
  index using = case using of
    C.UseEverything -> return ()
    C.Using importedNames -> traverse_ tellImport importedNames

instance Indexable A.ImportDirective where
  index (C.ImportDirective _range using hiding renaming _publicRange) = do
    index using
    indexHidingDirective hiding
    index renaming

instance Indexable A.LetBinding where
  index = \case
    A.LetBind _letInfo _argInfo boundName type' expr -> do
      -- TODO: what does the `ArgInfo` mean?
      tellDef boundName Local type'
      index type'
      index expr
    A.LetPatBind _letInfo pat expr -> do
      index pat
      index expr
    A.LetApply _moduleInfo _erased moduleName moduleApp _scopeCopyInfo importDirective -> do
      tellUsage moduleName
      index moduleApp
      index importDirective
    A.LetOpen _moduleInfo moduleName importDirective -> do
      tellUsage moduleName
      index importDirective
    A.LetDeclaredVariable boundName ->
      tellDef boundName Local UnknownType

indexNamedArgBinder ::
  (TypeLike t, HasParamNames t) =>
  C.NamedArg A.Binder -> t -> IndexerM ()
indexNamedArgBinder
  (C.Arg argInfo (C.Named _maybeArgName (A.Binder pat name)))
  typeLike =
    when (C.argInfoOrigin argInfo == C.UserWritten) $ do
      tellDef name Param typeLike
      index pat

instance Indexable A.TypedBinding where
  index = \case
    A.TBind _range typedBindInfo binders type' -> do
      index $ A.tbTacticAttr typedBindInfo
      forM_ binders $ \binder ->
        indexNamedArgBinder binder type'
      index type'
    A.TLet _range letBindings -> index letBindings

instance Indexable A.LamBinding where
  index lamBinding = case lamBinding of
    A.DomainFree tacticAttr binder -> do
      index tacticAttr
      indexNamedArgBinder binder UnknownType
    A.DomainFull binding ->
      index binding

instance Indexable A.GeneralizeTelescope where
  index (A.GeneralizeTel _generalizeVars tel) = index tel

instance Indexable A.DataDefParams where
  index (A.DataDefParams _generalizeParams params) = index params

instance Indexable A.RecordDirectives where
  index = \case
    (C.RecordDirectives inductive _hasEta _patRange (Just conName)) ->
      tellDef conName (constructorSymbolKind inductive) UnknownType
    _noUserConstructor -> return ()
    where
      constructorSymbolKind :: Maybe (C.Ranged C.Induction) -> SymbolKind
      constructorSymbolKind (Just (C.Ranged _range C.CoInductive)) = CoCon
      constructorSymbolKind _inductive = Con

instance Indexable A.Pragma where
  index = \case
    A.OptionsPragma _options ->
      return ()
    A.BuiltinPragma _rstring resolvedName ->
      whenJust (resolvedNameToAmbiguousQName resolvedName) $ \name ->
        tellUsage name
    A.BuiltinNoDefPragma _rstring kindOfName name -> do
      tellDef name kindOfName UnknownType
    A.RewritePragma _range ruleNames ->
      forM_ ruleNames $ \ruleName ->
        tellUsage ruleName
    A.CompilePragma _backendName name _compileAs ->
      tellUsage name
    A.StaticPragma name ->
      tellUsage name
    A.EtaPragma name ->
      tellUsage name
    A.InjectivePragma name ->
      tellUsage name
    A.InjectiveForInferencePragma name ->
      tellUsage name
    A.InlinePragma _shouldInline name ->
      tellUsage name
    A.NotProjectionLikePragma name ->
      tellUsage name
    A.OverlapPragma name _overlapMode ->
      tellUsage name
    A.DisplayPragma name args displayExpr -> do
      tellUsage name
      indexNamedArgs name args
      index displayExpr

--------------------------------------------------------------------------------

instance NameLike A.QName where
  toQName = id

instance NameLike A.ModuleName where
  toQName = A.mnameToQName

instance NameLike A.Name where
  toQName = A.qualify_

instance NameLike A.BindName where
  toQName = toQName . A.unBind

instance (NameLike n, NameLike m) => NameLike (C.ImportedName' n m) where
  toQName (C.ImportedModule moduleName) = toQName moduleName
  toQName (C.ImportedName name) = toQName name

instance AmbiguousNameLike A.AmbiguousQName where
  toAmbiguousQName = id

instance AmbiguousNameLike A.QName where
  toAmbiguousQName = A.unambiguous

instance AmbiguousNameLike A.ModuleName where
  toAmbiguousQName = toAmbiguousQName . A.mnameToQName

instance AmbiguousNameLike A.Name where
  toAmbiguousQName = toAmbiguousQName . A.qualify_

instance AmbiguousNameLike A.BindName where
  toAmbiguousQName = toAmbiguousQName . A.unBind

instance AmbiguousNameLike Scope.AbstractName where
  toAmbiguousQName = toAmbiguousQName . Scope.anameName

instance (AmbiguousNameLike n, AmbiguousNameLike m) => AmbiguousNameLike (C.ImportedName' n m) where
  toAmbiguousQName (C.ImportedModule moduleName) = toAmbiguousQName moduleName
  toAmbiguousQName (C.ImportedName name) = toAmbiguousQName name

resolvedNameToAmbiguousQName :: Scope.ResolvedName -> Maybe A.AmbiguousQName
resolvedNameToAmbiguousQName = \case
  Scope.VarName name _bindingSource -> Just $ toAmbiguousQName name
  Scope.DefinedName _access name _suffix -> Just $ toAmbiguousQName name
  Scope.FieldName name -> Just $ toAmbiguousQName name
  Scope.ConstructorName _inductive name -> Just $ toAmbiguousQName name
  Scope.PatternSynResName name -> Just $ toAmbiguousQName name
  Scope.UnknownName -> Nothing

instance SymbolKindLike SymbolKind where
  toSymbolKind = id

instance SymbolKindLike Scope.KindOfName where
  toSymbolKind = \case
    Scope.ConName -> Con
    Scope.CoConName -> CoCon
    Scope.FldName -> Field
    Scope.PatternSynName -> PatternSyn
    Scope.GeneralizeName -> GeneralizeVar
    Scope.DisallowedGeneralizeName -> GeneralizeVar
    Scope.MacroName -> Macro
    Scope.QuotableName -> Unknown
    Scope.DataName -> Data
    Scope.RecName -> Record
    Scope.FunName -> Fun
    Scope.AxiomName -> Axiom
    Scope.PrimName -> Prim
    Scope.OtherDefName -> Unknown

instance SymbolKindLike TCM.Defn where
  toSymbolKind = \case
    TCM.AxiomDefn _axiomData -> Axiom
    TCM.DataOrRecSigDefn _dataOrRecSigData -> Unknown
    TCM.GeneralizableVar -> GeneralizeVar
    TCM.AbstractDefn defn -> toSymbolKind defn
    TCM.FunctionDefn _functionData -> Fun
    TCM.DatatypeDefn _datatypeData -> Data
    TCM.RecordDefn _recordData -> Record
    TCM.ConstructorDefn _constructorData -> Con
    TCM.PrimitiveDefn _primitiveData -> Prim
    TCM.PrimitiveSortDefn _primitiveSortData -> Prim

instance (TypeLike t) => TypeLike (C.Arg t) where
  toTypeString = toTypeString . C.unArg

instance TypeLike A.Type where
  toTypeString = fmap (Just . render) . TCM.liftTCM . prettyTCM

instance TypeLike I.Type where
  toTypeString = fmap (Just . render) . TCM.liftTCM . prettyTCM

instance (HasParamNames p) => HasParamNames (C.Arg p) where
  getParamNames = getParamNames . C.unArg

instance (HasParamNames p) => HasParamNames [p]

instance (HasParamNames p) => HasParamNames (List1 p)

instance HasParamNames A.Type where
  getParamNames = \case
    A.Pi _exprInfo tel codom ->
      getParamNames tel <> getParamNames codom
    A.Fun _exprInfo _dom codom -> getParamNames codom
    A.Generalized varNames codom ->
      Set.toList varNames <> getParamNames codom
    A.ScopedExpr _scopeInfo expr -> getParamNames expr
    _noMoreParams -> []

instance HasParamNames A.TypedBinding where
  getParamNames = \case
    A.TBind _range _typedBindingInfo binders' _type ->
      List1.toList binders'
        <&> C.namedThing . C.unArg
        <&> (A.qualify_ . A.unBind . A.binderName)
    A.TLet _range _letBindings -> []

instance HasParamNames A.GeneralizeTelescope where
  getParamNames (A.GeneralizeTel _vars tel) = getParamNames tel
