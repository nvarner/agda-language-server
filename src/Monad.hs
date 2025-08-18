{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Monad where

import Agda.IR
import Agda.Interaction.Base (IOTCM)
import Agda.TypeChecking.Monad (TCMT)
import Agda.Utils.Lens (Lens', (^.))
import Control.Concurrent
import Control.Monad.Reader
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Maybe (isJust)
import Data.Text
  ( Text,
    pack,
  )
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
  ( LspM,
    MonadLsp,
    getConfig,
  )
import Options
import Server.CommandController (CommandController)
import qualified Server.CommandController as CommandController
import Server.Model (Model)
import qualified Server.Model as Model
import Server.Model.AgdaLib (AgdaLib)
import Server.ResponseController (ResponseController)
import qualified Server.ResponseController as ResponseController

--------------------------------------------------------------------------------

data Env = Env
  { envOptions :: Options,
    envDevMode :: Bool,
    envConfig :: Config,
    envLogChan :: Chan Text,
    envCommandController :: CommandController,
    envResponseChan :: Chan Response,
    envResponseController :: ResponseController,
    envModel :: !(IORef Model)
  }

createInitEnv :: (MonadIO m, MonadLsp Config m) => Options -> m Env
createInitEnv options =
  Env options (isJust (optViaTCP options))
    <$> getConfig
    <*> liftIO newChan
    <*> liftIO CommandController.new
    <*> liftIO newChan
    <*> liftIO ResponseController.new
    <*> liftIO (newIORef Model.empty)

--------------------------------------------------------------------------------

-- | OUR monad
type ServerT m = ReaderT Env m

type ServerM = ServerT (LspM Config)

runServerT :: Env -> ServerT m a -> m a
runServerT = flip runReaderT

--------------------------------------------------------------------------------

writeLog :: (Monad m, MonadIO m) => Text -> ServerT m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

writeLog' :: (Monad m, MonadIO m, Show a) => a -> ServerT m ()
writeLog' x = do
  chan <- asks envLogChan
  liftIO $ writeChan chan $ pack $ show x

askModel :: (MonadIO m) => ServerT m Model
askModel = do
  modelVar <- asks envModel
  liftIO $ readIORef modelVar

-- | Provider
provideCommand :: (Monad m, MonadIO m) => IOTCM -> ServerT m ()
provideCommand iotcm = do
  controller <- asks envCommandController
  liftIO $ CommandController.put controller iotcm

-- | Consumer
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM
consumeCommand env = liftIO $ CommandController.take (envCommandController env)

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerT m ()
waitUntilResponsesSent = do
  controller <- asks envResponseController
  liftIO $ ResponseController.setCheckpointAndWait controller

signalCommandFinish :: (Monad m, MonadIO m) => ServerT m ()
signalCommandFinish = do
  writeLog "[Command] Finished"
  -- send `ResponseEnd`
  env <- ask
  liftIO $ writeChan (envResponseChan env) ResponseEnd
  -- allow the next Command to be consumed
  liftIO $ CommandController.release (envCommandController env)

-- | Sends a Response to the client via "envResponseChan"
sendResponse :: (Monad m, MonadIO m) => Env -> Response -> TCMT m ()
sendResponse env response = liftIO $ writeChan (envResponseChan env) response
