module Test.Indexer.Invariants (tests) where

import Agda.Interaction.FindFile (SourceFile (SourceFile), srcFilePath)
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Options (defaultOptions)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.FileName (absolute)
import Agda.Utils.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Indexer (indexFile)
import Indexer.Indexer (abstractToIndex)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (runServerM)
import Server.Model.AgdaFile (AgdaFile, agdaFileRefs)
import Server.Model.Monad (withAgdaLibFor)
import System.FilePath (takeBaseName)
import Test.Indexer.NoMissing (testNoMissing)
import Test.Indexer.NoOverlap (testNoOverlap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.HUnit (testCase)
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
      runServerM env $ do
        interface <- withAgdaLibFor uri $ do
          TCM.liftTCM $ TCM.setCommandLineOptions defaultOptions
          absInPath <- liftIO $ absolute inPath
          let srcFile = SourceFile absInPath
          src <- TCM.liftTCM $ Imp.parseSource srcFile

          TCM.modifyTCLens TCM.stModuleToSource $ Map.insert (Imp.srcModuleName src) (srcFilePath $ Imp.srcOrigin src)
          checkResult <- TCM.liftTCM $ Imp.typeCheckMain Imp.TypeCheck src
          return $ Imp.crInterface checkResult

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
        testGroup "No missing references" ((\(name, file, interface) -> testNoMissing name file interface) <$> files)
      ]
