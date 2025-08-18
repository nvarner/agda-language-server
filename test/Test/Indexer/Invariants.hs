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
import Server.Model.Monad (withAgdaLibFor)
import System.FilePath (takeBaseName, (</>))
import Test.Indexer.NoDuplicateDecl (testNoDuplicateDecl)
import Test.Indexer.NoMissing (testNoMissing)
import Test.Indexer.NoOverlap (testNoOverlap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import qualified TestData

tests :: IO TestTree
tests = do
  inPaths <- findByExtension [".agda"] "test/data/Indexer"
  files <- forM inPaths $ \inPath -> do
    let testName = takeBaseName inPath
        uri = LSP.filePathToUri inPath
    model <- TestData.getModel

    (file, interface) <- LSP.runLspT undefined $ do
      env <- TestData.getServerEnv model
      runServerT env $ do
        interface <- withAgdaLibFor uri $ do
          TCM.liftTCM $ TCM.setCommandLineOptions defaultOptions
          absInPath <- liftIO $ absolute inPath
          let srcFile = SourceFile absInPath
          src <- TCM.liftTCM $ Imp.parseSource srcFile

          TCM.modifyTCLens TCM.stModuleToSource $ Map.insert (Imp.srcModuleName src) (srcFilePath $ Imp.srcOrigin src)
          checkResult <- TCM.liftTCM $ Imp.typeCheckMain Imp.TypeCheck src
          return $ Imp.crInterface checkResult

        ast <- withAgdaLibFor uri $ do
          TCM.liftTCM $ TCM.setCommandLineOptions defaultOptions
          absInPath <- liftIO $ absolute inPath
          let srcFile = SourceFile absInPath
          src <- TCM.liftTCM $ Imp.parseSource srcFile

          withAstFor src return

        -- Write the AST to a file for debugging purposes
        liftIO $ writeFile ("test/data/AST" </> testName) $ prettyShow $ topLevelDecls ast

        withAgdaLibFor uri $ do
          TCM.liftTCM $ TCM.setCommandLineOptions defaultOptions
          absInPath <- liftIO $ absolute inPath
          let srcFile = SourceFile absInPath
          src <- TCM.liftTCM $ Imp.parseSource srcFile

          agdaFile <- indexFile src
          return (agdaFile, interface)

    return (testName, file, interface)

  return $
    testGroup
      "Invariants"
      [ testGroup "No reference overlap" ((\(name, file, _interface) -> testNoOverlap name file) <$> files),
        testGroup "No missing references" ((\(name, file, interface) -> testNoMissing name file interface) <$> files),
        testGroup "No duplicate declarations" ((\(name, file, _interface) -> testNoDuplicateDecl name file) <$> files)
      ]
