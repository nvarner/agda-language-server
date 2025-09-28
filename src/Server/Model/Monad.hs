{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Server.Model.Monad
  ( MonadAgdaLib (..),
    useAgdaLib,
    notificationHandlerWithAgdaLib,
    MonadAgdaFile (..),
    useAgdaFile,
    requestHandlerWithAgdaFile,
    WithAgdaLibM,
    runWithAgdaLib,
  )
where

import Agda.Interaction.Options (CommandLineOptions (optPragmaOptions), PragmaOptions)
import Agda.TypeChecking.Monad (HasOptions (..), MonadTCEnv (..), MonadTCM (..), MonadTCState (..), PersistentTCState (stPersistentOptions), ReadTCState (..), TCEnv, TCM, TCMT (..), TCState (stPersistentState), modifyTCLens, setTCLens, stPragmaOptions, useTC)
import Agda.Utils.IORef (modifyIORef', readIORef, writeIORef)
import Agda.Utils.Lens (Lens', locally, over, use, view, (<&>), (^.))
import Agda.Utils.Monad (bracket_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as LSP
import Monad (ServerM, ServerT, askModel, findAgdaLib)
import Options (Config)
import qualified Server.Model as Model
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib, agdaLibTcEnv, agdaLibTcStateRef)

--------------------------------------------------------------------------------

class (MonadTCM m, ReadTCState m) => MonadAgdaLib m where
  askAgdaLib :: m AgdaLib
  localAgdaLib :: (AgdaLib -> AgdaLib) -> m a -> m a

useAgdaLib :: (MonadAgdaLib m) => Lens' AgdaLib a -> m a
useAgdaLib lens = do
  agdaLib <- askAgdaLib
  return $ agdaLib ^. lens

class (MonadAgdaLib m) => MonadAgdaFile m where
  askAgdaFile :: m AgdaFile
  localAgdaFile :: (AgdaFile -> AgdaFile) -> m a -> m a

useAgdaFile :: (MonadAgdaFile m) => Lens' AgdaFile a -> m a
useAgdaFile lens = do
  agdaFile <- askAgdaFile
  return $ agdaFile ^. lens

--------------------------------------------------------------------------------

defaultAskTC :: (MonadAgdaLib m) => m TCEnv
defaultAskTC = useAgdaLib agdaLibTcEnv

defaultLocalTC :: (MonadAgdaLib m) => (TCEnv -> TCEnv) -> m a -> m a
defaultLocalTC f = localAgdaLib (over agdaLibTcEnv f)

defaultGetTC :: (MonadAgdaLib m) => m TCState
defaultGetTC = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  liftIO $ readIORef tcStateRef

defaultPutTC :: (MonadAgdaLib m) => TCState -> m ()
defaultPutTC tcState = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  liftIO $ writeIORef tcStateRef tcState

defaultModifyTC :: (MonadAgdaLib m) => (TCState -> TCState) -> m ()
defaultModifyTC f = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  liftIO $ modifyIORef' tcStateRef f

-- Taken from TCMT implementation
defaultLocallyTCState :: (MonadAgdaLib m) => Lens' TCState a -> (a -> a) -> m b -> m b
defaultLocallyTCState lens f = bracket_ (useTC lens <* modifyTCLens lens f) (setTCLens lens)

-- Taken from TCMT implementation
defaultPragmaOptionsImpl :: (MonadAgdaLib m) => m PragmaOptions
defaultPragmaOptionsImpl = useTC stPragmaOptions

-- Taken from TCMT implementation
defaultCommandLineOptionsImpl :: (MonadAgdaLib m) => m CommandLineOptions
defaultCommandLineOptionsImpl = do
  p <- useTC stPragmaOptions
  cl <- stPersistentOptions . stPersistentState <$> getTC
  return $ cl {optPragmaOptions = p}

defaultLiftTCM :: (MonadAgdaLib m) => TCM a -> m a
defaultLiftTCM (TCM f) = do
  tcStateRef <- useAgdaLib agdaLibTcStateRef
  tcEnv <- useAgdaLib agdaLibTcEnv
  liftIO $ f tcStateRef tcEnv

--------------------------------------------------------------------------------

newtype WithAgdaLibT m a = WithAgdaLibT {unWithAgdaLibT :: ReaderT AgdaLib m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runWithAgdaLibT :: AgdaLib -> WithAgdaLibT m a -> m a
runWithAgdaLibT agdaLib = flip runReaderT agdaLib . unWithAgdaLibT

type WithAgdaLibM = WithAgdaLibT ServerM

runWithAgdaLib :: LSP.Uri -> WithAgdaLibM a -> ServerM a
runWithAgdaLib uri x = do
  let normUri = LSP.toNormalizedUri uri
  model <- askModel
  agdaLib <- Model.getAgdaLib normUri model
  runWithAgdaLibT agdaLib x

type NotificationHandlerWithAgdaLib m =
  LSP.TNotificationMessage m -> LSP.NormalizedUri -> WithAgdaLibM ()

notificationHandlerWithAgdaLib ::
  forall (m :: LSP.Method LSP.ClientToServer LSP.Notification) textdoc.
  (LSP.HasTextDocument (LSP.MessageParams m) textdoc, LSP.HasUri textdoc LSP.Uri) =>
  LSP.SMethod m ->
  NotificationHandlerWithAgdaLib m ->
  LSP.Handlers ServerM
notificationHandlerWithAgdaLib m handler = LSP.notificationHandler m $ \notification -> do
  let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
      normUri = LSP.toNormalizedUri uri

  agdaLib <- findAgdaLib normUri
  runWithAgdaLibT agdaLib $ handler notification normUri

instance (MonadIO m) => MonadAgdaLib (WithAgdaLibT m) where
  askAgdaLib = WithAgdaLibT ask
  localAgdaLib f = WithAgdaLibT . local f . unWithAgdaLibT

instance (MonadIO m) => MonadTCEnv (WithAgdaLibT m) where
  askTC = defaultAskTC
  localTC = defaultLocalTC

instance (MonadIO m) => MonadTCState (WithAgdaLibT m) where
  getTC = defaultGetTC
  putTC = defaultPutTC
  modifyTC = defaultModifyTC

instance (MonadIO m) => ReadTCState (WithAgdaLibT m) where
  getTCState = defaultGetTC
  locallyTCState = defaultLocallyTCState

instance (MonadIO m) => HasOptions (WithAgdaLibT m) where
  pragmaOptions = defaultPragmaOptionsImpl
  commandLineOptions = defaultCommandLineOptionsImpl

instance (MonadIO m) => MonadTCM (WithAgdaLibT m) where
  liftTCM = defaultLiftTCM

--------------------------------------------------------------------------------

data WithAgdaFileEnv = WithAgdaFileEnv
  { _withAgdaFileEnvAgdaLib :: !AgdaLib,
    _withAgdaFileEnvAgdaFile :: !AgdaFile
  }

withAgdaFileEnvAgdaLib :: Lens' WithAgdaFileEnv AgdaLib
withAgdaFileEnvAgdaLib f a = f (_withAgdaFileEnvAgdaLib a) <&> \x -> a {_withAgdaFileEnvAgdaLib = x}

withAgdaFileEnvAgdaFile :: Lens' WithAgdaFileEnv AgdaFile
withAgdaFileEnvAgdaFile f a = f (_withAgdaFileEnvAgdaFile a) <&> \x -> a {_withAgdaFileEnvAgdaFile = x}

newtype WithAgdaFileT m a = WithAgdaFileT
  {unWithAgdaFileT :: ReaderT WithAgdaFileEnv m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runWithAgdaFileT :: AgdaLib -> AgdaFile -> WithAgdaFileT m a -> m a
runWithAgdaFileT agdaLib agdaFile =
  let env = WithAgdaFileEnv agdaLib agdaFile
   in flip runReaderT env . unWithAgdaFileT

type WithAgdaFileM = WithAgdaFileT ServerM

type RequestHandlerWithAgdaFile m =
  LSP.TRequestMessage m ->
  (Either (LSP.TResponseError m) (LSP.MessageResult m) -> WithAgdaFileM ()) ->
  WithAgdaFileM ()

requestHandlerWithAgdaFile ::
  forall (m :: LSP.Method LSP.ClientToServer LSP.Request).
  (LSP.HasTextDocument (LSP.MessageParams m) LSP.TextDocumentIdentifier) =>
  LSP.SMethod m ->
  RequestHandlerWithAgdaFile m ->
  LSP.Handlers ServerM
requestHandlerWithAgdaFile m handler = LSP.requestHandler m $ \req responder -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
      normUri = LSP.toNormalizedUri uri

  model <- askModel
  case Model.getAgdaFile normUri model of
    Nothing -> do
      let message = "Request for unknown Agda file at URI: " <> LSP.getUri uri
      responder $ Left $ LSP.TResponseError (LSP.InR LSP.ErrorCodes_InvalidParams) message Nothing
    Just agdaFile -> do
      agdaLib <- Model.getAgdaLib normUri model
      let responder' = lift . responder
      runWithAgdaFileT agdaLib agdaFile $ handler req responder'

instance (MonadIO m) => MonadAgdaLib (WithAgdaFileT m) where
  askAgdaLib = WithAgdaFileT $ view withAgdaFileEnvAgdaLib
  localAgdaLib f = WithAgdaFileT . locally withAgdaFileEnvAgdaLib f . unWithAgdaFileT

instance (MonadIO m) => MonadAgdaFile (WithAgdaFileT m) where
  askAgdaFile = WithAgdaFileT $ view withAgdaFileEnvAgdaFile
  localAgdaFile f = WithAgdaFileT . locally withAgdaFileEnvAgdaFile f . unWithAgdaFileT

instance (MonadIO m) => MonadTCEnv (WithAgdaFileT m) where
  askTC = defaultAskTC
  localTC = defaultLocalTC

instance (MonadIO m) => MonadTCState (WithAgdaFileT m) where
  getTC = defaultGetTC
  putTC = defaultPutTC
  modifyTC = defaultModifyTC

instance (MonadIO m) => ReadTCState (WithAgdaFileT m) where
  getTCState = defaultGetTC
  locallyTCState = defaultLocallyTCState

instance (MonadIO m) => HasOptions (WithAgdaFileT m) where
  pragmaOptions = defaultPragmaOptionsImpl
  commandLineOptions = defaultCommandLineOptionsImpl

instance (MonadIO m) => MonadTCM (WithAgdaFileT m) where
  liftTCM = defaultLiftTCM
