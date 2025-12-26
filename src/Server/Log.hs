{-# LANGUAGE FlexibleContexts #-}

module Server.Log
  ( infoT,
    infoP,
    errorT,
    errorP,
    errorTCM,
  )
where

import Agda.Syntax.Common.Pretty (Pretty, prettyShow)
import Agda.TypeChecking.Monad (MonadTCM, liftTCM)
import Agda.TypeChecking.Pretty (PrettyTCM, prettyTCM)
import Agda.Utils.Monad (ifM, (<=<))
import Colog.Core (Severity (Error, Info), WithSeverity (WithSeverity), (<&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Logging as LSP
import Language.LSP.Server (MonadLsp)
import Monad (MonadMockLsp (shouldMockLsp))
import Options (Config)

console :: (MonadLsp Config m, MonadMockLsp m) => Severity -> Text -> m ()
console severity text =
  ifM shouldMockLsp (pure ()) $
    (LSP.logToLogMessage <& WithSeverity text severity)

popup :: (MonadLsp Config m, MonadMockLsp m) => Severity -> Text -> m ()
popup severity text =
  ifM shouldMockLsp (pure ()) $
    LSP.logToShowMessage <& WithSeverity text severity

infoT :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
infoT text = console Info text

infoP :: (MonadLsp Config m, MonadMockLsp m, Pretty p) => p -> m ()
infoP = infoT . Text.pack . prettyShow

errorT :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
errorT text = console Error text >> popup Error text

errorP :: (MonadLsp Config m, MonadMockLsp m, Pretty p) => p -> m ()
errorP = errorT . Text.pack . prettyShow

errorTCM :: (MonadLsp Config m, MonadMockLsp m, MonadTCM m, PrettyTCM p) => p -> m ()
errorTCM = errorP <=< liftTCM . prettyTCM
