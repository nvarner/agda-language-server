module Test.AgdaLibResolution (tests) where

import Agda.Syntax.Common.Pretty (prettyShow)
import Control.Monad.IO.Class (liftIO)
import Indexer (indexFile, usingSrcAsCurrent)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (runServerT)
import Server.AgdaLibResolver (findAgdaLib)
import Server.Model.Monad (MonadAgdaLib (askAgdaLib), runWithAgdaLib)
import System.Directory (makeAbsolute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import qualified TestData

natPath, constPath :: FilePath
natPath = "test/data/libs/no-deps/src/Data/Nat.agda"
constPath = "test/data/libs/no-deps/src/Constants.agda"

tests :: TestTree
tests =
  testGroup
    "Agda lib resolution"
    [ testCase "Module without imports in lib without dependencies" $ do
        model <- TestData.getModel

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            runWithAgdaLib (LSP.filePathToUri natPath) $ do
              natSrc <- TestData.parseSourceFromPath natPath
              _ <- indexFile natSrc
              return (),
      testCase "Module with imports in lib without lib dependencies" $ do
        model <- TestData.getModel

        absConstPath <- makeAbsolute constPath

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            lib <- findAgdaLib absConstPath
            liftIO $ assertFailure $ prettyShow lib

            -- runWithAgdaLib (LSP.filePathToUri absConstPath) $ do
            --   lib <- askAgdaLib
            --   _ <- liftIO $ assertFailure $ prettyShow lib
            --   constSrc <- TestData.parseSourceFromPath constPath
            --   _ <- indexFile constSrc
            --   return ()
    ]
