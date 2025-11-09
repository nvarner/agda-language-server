module Test.Indexer.Reload (tests) where

import Indexer (indexFile)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (runServerT)
import Server.Model.Monad (runWithAgdaLib)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified TestData

tests :: TestTree
tests =
  testGroup "Reload" $
    [ testCase "Reload single file" $ testReloadFile "test/data/A.agda"
    ]

testReloadFile :: FilePath -> IO ()
testReloadFile path = do
  let uri = LSP.filePathToUri path
  model <- TestData.getModel

  LSP.runLspT undefined $ do
    env <- TestData.getServerEnv model
    runServerT env $ do
      runWithAgdaLib uri $ do
        src <- TestData.parseSourceFromPath path
        _ <- indexFile src
        _ <- indexFile src
        return ()
