module Test.Indexer.Invariants (tests) where

import Agda.Interaction.FindFile (SourceFile (SourceFile), srcFilePath)
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Options (defaultOptions)
import Agda.Syntax.Abstract.More ()
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Translation.ConcreteToAbstract (TopLevelInfo (topLevelDecls))
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.FileName (absolute)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Indexer (indexFile, withAstFor)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (runServerT)
import Server.Model.Monad (runWithAgdaLib)
import System.FilePath (takeBaseName, (</>))
import Test.Indexer.NoDuplicateDecl (testNoDuplicateDecl)
import Test.Indexer.NoMissing (testNoMissing)
import Test.Indexer.NoOverlap (testNoOverlap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import TestData (AgdaFileDetails (AgdaFileDetails))
import qualified TestData

tests :: IO TestTree
tests = do
  inPaths <- findByExtension [".agda"] "test/data/Indexer"
  files <- forM inPaths $ \inPath -> do
    TestData.AgdaFileDetails testName file interface <- TestData.agdaFileDetails inPath
    return (testName, file, interface)

  return $
    testGroup
      "Invariants"
      [ testGroup "No reference overlap" ((\(name, file, _interface) -> testNoOverlap name file) <$> files),
        testGroup "No missing references" ((\(name, file, interface) -> testNoMissing name file interface) <$> files),
        testGroup "No duplicate declarations" ((\(name, file, _interface) -> testNoDuplicateDecl name file) <$> files)
      ]
