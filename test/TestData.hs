{-# LANGUAGE CPP #-}
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
    parseSourceFromPath,
    parseSourceFromPathAndContents,
  )
where

import Agda.Interaction.FindFile
  ( SourceFile (SourceFile),
#if MIN_VERSION_Agda(2,8,0)
#else
    srcFilePath,
#endif
  )
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
import Indexer (indexFile, withAstFor, usingSrcAsCurrent)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (Env (Env), runServerT, catchTCError)
import Options (defaultOptions, initConfig)
import qualified Server.CommandController as CommandController
import Server.Model (Model (Model))
import Server.Model.AgdaFile (AgdaFile, emptyAgdaFile)
import Server.Model.AgdaLib (agdaLibIncludes, initAgdaLib)
import Server.Model.Monad (runWithAgdaLib, MonadAgdaLib)
import qualified Server.ResponseController as ResponseController
import System.FilePath (takeBaseName, (</>))
import Agda.TypeChecking.Pretty (prettyTCM)
import Data.Text (Text)
import Agda.Interaction.Imports.Virtual (parseSourceFromContents)
import qualified Server.Filesystem as FS
import qualified Server.VfsIndex as VfsIndex

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
      let withSrc f = runWithAgdaLib uri $ do
            TCM.liftTCM $ TCM.setCommandLineOptions Agda.Interaction.Options.defaultOptions
            src <- parseSourceFromPath inPath

            f src

      let onErr = \err -> runWithAgdaLib uri $ do
            t <- TCM.liftTCM $ prettyTCM err
            error $ prettyShow t

      interface <- (withSrc $ \src -> usingSrcAsCurrent src $ do
        checkResult <- TCM.liftTCM $ Imp.typeCheckMain Imp.TypeCheck src
        return $ Imp.crInterface checkResult) `catchTCError` onErr

      file <- withSrc indexFile `catchTCError` onErr

      return (file, interface)

  return $ AgdaFileDetails testName file interface

sourceFileFromPath :: (TCM.MonadTCM m) => FilePath -> m SourceFile
sourceFileFromPath path = do
  absPath <- liftIO $ absolute path
#if MIN_VERSION_Agda(2,8,0)
  TCM.liftTCM $ TCM.srcFromPath absPath
#else
  return $ SourceFile absPath
#endif

parseSourceFromPath :: (TCM.MonadTCM m) => FilePath -> m Imp.Source
parseSourceFromPath path = do
  srcFile <- sourceFileFromPath path
  TCM.liftTCM $ Imp.parseSource srcFile

parseSourceFromPathAndContents ::
  (TCM.MonadTCM m, TCM.MonadTrace m, MonadAgdaLib m) =>
  FilePath ->
  Text ->
    m Imp.Source
parseSourceFromPathAndContents path contents = do
  srcFile <- sourceFileFromPath path
  let uri = LSP.toNormalizedUri $ LSP.filePathToUri path
  parseSourceFromContents uri srcFile contents

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
    <*> (pure $ FS.Layered [FS.Wrap FS.OsFilesystem])
    <*> liftIO (newIORef VfsIndex.empty)
    <*> liftIO (newIORef model)
