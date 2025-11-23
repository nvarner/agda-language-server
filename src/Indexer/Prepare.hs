module Indexer.Prepare (setCommandLineOptionsByLib) where

import Agda.Interaction.Library (LibName)
import Agda.Interaction.Library.Base (libNameForCurrentDir)
import Agda.Interaction.Options (CommandLineOptions (..))
import qualified Agda.Interaction.Options.Lenses as Lens
import qualified Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Monad.Benchmark (updateBenchmarkingStatus)
import Agda.Utils.FileName (AbsolutePath, absolute)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Lens ((^.))
import qualified Agda.Utils.List1 as List1
import Agda.Utils.Maybe (ifJustM, maybeToList)
import Agda.Utils.Monad (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Server.AgdaLibResolver as AgdaLibResolver
import qualified Server.Filesystem as FS
import Server.Model.AgdaLib (AgdaLib, agdaLibDependencies, agdaLibIncludes, agdaLibName)
import Server.Model.Monad (MonadAgdaProject, WithAgdaProjectM, askAgdaLib)
import System.Directory (getCurrentDirectory)

setCommandLineOptionsByLib :: CommandLineOptions -> WithAgdaProjectM ()
setCommandLineOptionsByLib opts = do
  root <- liftIO (absolute =<< getCurrentDirectory)
  setCommandLineOptionsByLib' root opts

setCommandLineOptionsByLib' :: AbsolutePath -> CommandLineOptions -> WithAgdaProjectM ()
setCommandLineOptionsByLib' root opts = do
  incs <- case optAbsoluteIncludePaths opts of
    [] -> do
      opts' <- updateOptionsWithDependencyLibs opts
      let incs = optIncludePaths opts'
      TCM.liftTCM $ TCM.setIncludeDirs incs root
      List1.toList <$> TCM.getIncludeDirs
    incs -> return incs
  TCM.modifyTC $ Lens.setCommandLineOptions opts {optAbsoluteIncludePaths = incs}
  TCM.liftTCM $ TCM.setPragmaOptions (optPragmaOptions opts)
  TCM.liftTCM updateBenchmarkingStatus

-- | Determine the libraries we directly depend on
directDependencyLibNames ::
  (MonadAgdaProject m) =>
  -- | Persistent command line options
  CommandLineOptions ->
  m [LibName]
directDependencyLibNames persistentOptions
  | not (null $ optLibraries persistentOptions) = return $ optLibraries persistentOptions
  | not (optUseLibs persistentOptions) = return []
  | otherwise =
      ifJustM
        askAgdaLib
        (\agdaLib -> return $ agdaLib ^. agdaLibDependencies)
        (defaultLibNames persistentOptions)

-- | Determine the libraries we depend on when there is no .agda-lib
-- TODO: read from default file
defaultLibNames ::
  (MonadAgdaProject m) =>
  CommandLineOptions ->
  m [LibName]
defaultLibNames persistentOptions
  | optDefaultLibs persistentOptions = return [libNameForCurrentDir]
  | otherwise = return []

dependencyLibs :: CommandLineOptions -> WithAgdaProjectM [AgdaLib]
dependencyLibs persistentOptions = do
  directDependencies <- directDependencyLibNames persistentOptions
  installed <- lift $ AgdaLibResolver.installedLibraries (FS.LocalFilePath <$> optOverrideLibrariesFile persistentOptions)
  let libs = resolveDeps installed directDependencies [] []
  case libs of
    Nothing -> __IMPOSSIBLE__ -- TODO: very wrong, do real error handling
    Just libs -> do
      projectLib <- askAgdaLib
      return $ maybeToList projectLib <> libs
  where
    resolveDeps :: [AgdaLib] -> [LibName] -> [LibName] -> [AgdaLib] -> Maybe [AgdaLib]
    resolveDeps _installed [] _doneNames doneLibs = Just doneLibs
    resolveDeps installed (next : todo) doneNames doneLibs
      | next `elem` doneNames = resolveDeps installed todo doneNames doneLibs
      | otherwise = do
          lib <- AgdaLibResolver.byLibName next installed
          let newDeps = lib ^. agdaLibDependencies
          resolveDeps installed (newDeps <> todo) (next : doneNames) (lib : doneLibs)

updateOptionsWithDependencyLibs :: CommandLineOptions -> WithAgdaProjectM CommandLineOptions
updateOptionsWithDependencyLibs o = do
  libs <- dependencyLibs o
  let includes = concatMap (^. agdaLibIncludes) libs
  let libNames = fmap (^. agdaLibName) libs
  let includePaths = FS.fileIdToPossiblyInvalidFilePath <$> includes
  return $
    o
      { optIncludePaths = includePaths <> optIncludePaths o,
        optLibraries = libNames
      }
