{-# LANGUAGE DataKinds #-}

module TestData
  ( documentSymbolMessage,
    getModel,
    fileUri1,
    fileUri2,
    fileUri3,
    fakeUri,
    getServerEnv,
  )
where

import Agda.Utils.IORef (newIORef)
import Agda.Utils.Lens (set, (<&>))
import Control.Concurrent (newChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Monad (Env (Env))
import Options (defaultOptions, initConfig)
import qualified Server.CommandController as CommandController
import Server.Model (Model (Model))
import Server.Model.AgdaFile (emptyAgdaFile)
import Server.Model.AgdaLib (agdaLibIncludes, initAgdaLib)
import qualified Server.ResponseController as ResponseController

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
