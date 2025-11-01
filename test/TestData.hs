{-# LANGUAGE DataKinds #-}

module TestData
  ( documentSymbolMessage,
    getModel,
    fileUri1,
    fileUri2,
    fileUri3,
    fakeUri,
    getServerEnv,
    AgdaFileDetails (..),
    agdaFileDetails,
  )
where

import Agda.Interaction.FindFile (SourceFile (SourceFile), srcFilePath)
import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.Options
import Agda.Syntax.Abstract.More ()
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Translation.ConcreteToAbstract (topLevelDecls)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.FileName (absolute)
import Agda.Utils.IORef (newIORef)
import Agda.Utils.Lens (set, (<&>))
import Control.Concurrent (newChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Indexer (indexFile, withAstFor)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (Env (Env), runServerT)
import Options (defaultOptions, initConfig)
import qualified Server.CommandController as CommandController
import Server.Model (Model (Model))
import Server.Model.AgdaFile (AgdaFile, emptyAgdaFile)
import Server.Model.AgdaLib (agdaLibIncludes, initAgdaLib)
import Server.Model.Monad (runWithAgdaLib)
import qualified Server.ResponseController as ResponseController
import System.FilePath (takeBaseName, (</>))

data AgdaFileDetails = AgdaFileDetails
  { fileName :: String,
    agdaFile :: AgdaFile,
    interface :: TCM.Interface
  }

agdaFileDetails :: FilePath -> IO AgdaFileDetails
agdaFileDetails inPath = do
  let testName = takeBaseName inPath
      uri = LSP.filePathToUri inPath
  model <- TestData.getModel

  (file, interface) <- LSP.runLspT undefined $ do
    env <- TestData.getServerEnv model
    runServerT env $ do
      interface <- runWithAgdaLib uri $ do
        TCM.liftTCM $ TCM.setCommandLineOptions Agda.Interaction.Options.defaultOptions
        absInPath <- liftIO $ absolute inPath
        let srcFile = SourceFile absInPath
        src <- TCM.liftTCM $ Imp.parseSource srcFile

        TCM.modifyTCLens TCM.stModuleToSource $ Map.insert (Imp.srcModuleName src) (srcFilePath $ Imp.srcOrigin src)
        checkResult <- TCM.liftTCM $ Imp.typeCheckMain Imp.TypeCheck src
        return $ Imp.crInterface checkResult

      ast <- runWithAgdaLib uri $ do
        TCM.liftTCM $ TCM.setCommandLineOptions Agda.Interaction.Options.defaultOptions
        absInPath <- liftIO $ absolute inPath
        let srcFile = SourceFile absInPath
        src <- TCM.liftTCM $ Imp.parseSource srcFile

        withAstFor src return

      runWithAgdaLib uri $ do
        TCM.liftTCM $ TCM.setCommandLineOptions Agda.Interaction.Options.defaultOptions
        absInPath <- liftIO $ absolute inPath
        let srcFile = SourceFile absInPath
        src <- TCM.liftTCM $ Imp.parseSource srcFile

        agdaFile <- indexFile src
        return (agdaFile, interface)

  return $ AgdaFileDetails testName file interface

--------------------------------------------------------------------------------

documentSymbolMessage :: LSP.NormalizedUri -> LSP.TRequestMessage LSP.Method_TextDocumentDocumentSymbol
documentSymbolMessage uri =
  let params =
        LSP.DocumentSymbolParams
          Nothing
          Nothing
          (LSP.TextDocumentIdentifier $ LSP.fromNormalizedUri uri)
   in LSP.TRequestMessage
        "2.0"
        (LSP.IdInt 0)
        LSP.SMethod_TextDocumentDocumentSymbol
        params

--------------------------------------------------------------------------------

fileUri1 :: LSP.NormalizedUri
fileUri1 = LSP.toNormalizedUri $ LSP.Uri "file:///home/user2/project2/A/B/C.agda"

fileUri2 :: LSP.NormalizedUri
fileUri2 = LSP.toNormalizedUri $ LSP.Uri "file:///home/user/project2/X.agda"

fileUri3 :: LSP.NormalizedUri
fileUri3 = LSP.toNormalizedUri $ LSP.Uri "https://example.com/agda/Main.agda"

fakeUri :: LSP.NormalizedUri
fakeUri = LSP.toNormalizedUri $ LSP.Uri "file:///home/user2/project/Test.agda"

getModel :: IO Model
getModel = do
  let includes1 =
        LSP.toNormalizedUri . LSP.Uri
          <$> [ "file:///home/user/project1/",
                "file:///home/user2/project2/",
                "https://example.com/agda/"
              ]
  testLib1 <-
    initAgdaLib
      <&> set agdaLibIncludes includes1

  let includes2 =
        LSP.toNormalizedUri . LSP.Uri
          <$> ["file:///home/user/project2/"]
  testLib2 <-
    initAgdaLib
      <&> set agdaLibIncludes includes2

  let libs = [testLib1, testLib2]

  let testFile1 = emptyAgdaFile
  let testFile2 = emptyAgdaFile
  let testFile3 = emptyAgdaFile

  let files =
        Map.fromList
          [ (fileUri1, testFile1),
            (fileUri2, testFile2),
            (fileUri3, testFile3)
          ]

  return $ Model libs files

--------------------------------------------------------------------------------

getServerEnv :: (MonadIO m) => Model -> m Env
getServerEnv model =
  Env defaultOptions True initConfig
    <$> liftIO newChan
    <*> liftIO CommandController.new
    <*> liftIO newChan
    <*> liftIO ResponseController.new
    <*> liftIO (newIORef model)
