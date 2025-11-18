{-# LANGUAGE CPP #-}

module Test.AgdaLibResolution (tests) where

#if MIN_VERSION_Agda(2,8,0)
import Agda.Interaction.Library (parseLibName)
#endif
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Utils.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Indexer (indexFile, usingSrcAsCurrent)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (runServerT)
import Server.AgdaLibResolver (findAgdaLib)
import qualified Server.Filesystem as FS
import Server.Model.AgdaLib (AgdaLibOrigin (FromFile), agdaLibIncludes, agdaLibName, agdaLibOrigin)
import Server.Model.Monad (MonadAgdaLib, askAgdaLib, runWithAgdaLib, runWithAgdaLibT)
import System.Directory (makeAbsolute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified TestData

natPath, constPath, agdaLibPath, srcPath :: FilePath
natPath = "test/data/libs/no-deps/src/Data/Nat.agda"
constPath = "test/data/libs/no-deps/src/Constants.agda"
agdaLibPath = "test/data/libs/no-deps/no-deps.agda-lib"
srcPath = "test/data/libs/no-deps/src"

tests :: TestTree
tests =
  testGroup
    "Agda lib resolution"
    [ testCase "Explicit" $ do
        model <- TestData.getModel

        absConstPath <- makeAbsolute constPath
        absAgdaLibPath <- makeAbsolute agdaLibPath
        absSrcPath <- makeAbsolute srcPath

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            lib <- findAgdaLib absConstPath

#if MIN_VERSION_Agda(2,8,0)
            liftIO $ lib ^. agdaLibName @?= parseLibName "no-deps"
#else
            liftIO $ lib ^. agdaLibName @?= "no-deps"
#endif
            liftIO $ lib ^. agdaLibOrigin @?= FromFile (FS.LocalFilePath absAgdaLibPath)
            liftIO $ lib ^. agdaLibIncludes @?= [FS.LocalFilePath absSrcPath],
      testCase "Module without imports in lib without dependencies" $ do
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

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            runWithAgdaLib (LSP.filePathToUri constPath) $ do
              constSrc <- TestData.parseSourceFromPath constPath
              _ <- indexFile constSrc
              return ()
    ]
