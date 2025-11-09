{-# LANGUAGE CPP #-}

module Agda.Interaction.Library.More
  ( tryRunLibM,
#if MIN_VERSION_Agda(2,8,0)
#else
    runLibErrorIO,
#endif
  )
where

import Agda.Interaction.Library (LibM, AgdaLibFile)
import Agda.Interaction.Library.Base (LibErrorIO, libName, libFile, libIncludes)
import Agda.Utils.Either (maybeRight)
import Agda.Utils.Null (Null (empty))
import Control.Category ((>>>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Lazy (evalStateT)
import Control.Monad.Writer.Lazy (runWriterT)
import Agda.Syntax.Common.Pretty (Pretty, pretty, text, (<+>))
import Agda.Utils.Lens ((^.))

#if MIN_VERSION_Agda(2,8,0)
-- Unneeded in 2.8.0 due to API changes
#else
runLibErrorIO :: (MonadIO m) => LibErrorIO a -> m a
runLibErrorIO =
  runWriterT
    >>> flip evalStateT empty
    >>> fmap fst
    >>> liftIO
#endif

tryRunLibM :: (MonadIO m) => LibM a -> m (Maybe a)
tryRunLibM =
  runExceptT
    >>> runWriterT
    >>> flip evalStateT empty
    >>> fmap (fst >>> maybeRight)
    >>> liftIO

instance Pretty AgdaLibFile where
  pretty agdaLibFile =
    text "AgdaLibFile"
      <+> (pretty $ agdaLibFile ^. libName)
      <+> (pretty $ agdaLibFile ^. libFile)
      <+> (pretty $ agdaLibFile ^. libIncludes)
