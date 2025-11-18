module Agda.TypeChecking.Monad.Options.More (setCommandLineOptionsByLib) where

import Agda.Interaction.Options (CommandLineOptions (..))
import qualified Agda.Interaction.Options.Lenses as Lens
import Agda.TypeChecking.Monad (MonadTCM)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Monad.Benchmark (updateBenchmarkingStatus)
import Agda.Utils.FileName (AbsolutePath, absolute)
import Agda.Utils.Lens ((^.))
import qualified Agda.Utils.List1 as List1
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidFilePath)
import qualified Server.Filesystem as FS
import Server.Model.AgdaLib (AgdaLib, agdaLibDependencies, agdaLibIncludes)
import Server.Model.Monad (MonadAgdaLib (askAgdaLib))
import System.Directory (getCurrentDirectory)

setCommandLineOptionsByLib ::
  (MonadTCM m, MonadAgdaLib m) =>
  CommandLineOptions ->
  m ()
setCommandLineOptionsByLib opts = do
  root <- liftIO (absolute =<< getCurrentDirectory)
  setCommandLineOptionsByLib' root opts

setCommandLineOptionsByLib' ::
  (MonadTCM m, MonadAgdaLib m) =>
  AbsolutePath ->
  CommandLineOptions ->
  m ()
setCommandLineOptionsByLib' root opts = do
  incs <- case optAbsoluteIncludePaths opts of
    [] -> do
      opts' <- setLibraryPathsByLib opts
      let incs = optIncludePaths opts'
      TCM.liftTCM $ TCM.setIncludeDirs incs root
      List1.toList <$> TCM.getIncludeDirs
    incs -> return incs
  TCM.modifyTC $ Lens.setCommandLineOptions opts {optAbsoluteIncludePaths = incs}
  TCM.liftTCM $ TCM.setPragmaOptions (optPragmaOptions opts)
  TCM.liftTCM updateBenchmarkingStatus

setLibraryPathsByLib ::
  (MonadTCM m, MonadAgdaLib m) =>
  CommandLineOptions ->
  m CommandLineOptions
setLibraryPathsByLib o = do
  agdaLib <- askAgdaLib
  return $ addDefaultLibrariesByLib agdaLib o

-- TODO: resolve dependency libs; see setLibraryIncludes in Agda

addDefaultLibrariesByLib :: AgdaLib -> CommandLineOptions -> CommandLineOptions
addDefaultLibrariesByLib agdaLib o
  | not (null $ optLibraries o) || not (optUseLibs o) = o
  | otherwise = do
      let libs = agdaLib ^. agdaLibDependencies
      let incs = uriToPossiblyInvalidFilePath . FS.fileIdToUri <$> agdaLib ^. agdaLibIncludes
      o {optIncludePaths = incs ++ optIncludePaths o, optLibraries = libs}
