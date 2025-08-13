module Indexer (indexFile) where

import Agda.Interaction.FindFile (SourceFile (SourceFile), srcFilePath)
import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.Imports.More as Imp
import Agda.Interaction.Library (getPrimitiveLibDir)
import Agda.Interaction.Options (defaultOptions, optLoadPrimitives)
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Translation.ConcreteToAbstract (ToAbstract (toAbstract), TopLevel (TopLevel))
import Agda.Syntax.Translation.ReflectedToAbstract (toAbstract_)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.FileName (AbsolutePath, filePath, mkAbsolute)
import Agda.Utils.Monad (bracket_, unlessM, when)
import qualified Agda.Utils.Trie as Trie
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Strict as Strict
import Indexer.Indexer (abstractToIndex)
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.Monad (MonadAgdaLib, WithAgdaLibM)
import System.FilePath ((</>))

indexFile ::
  Imp.Source ->
  WithAgdaLibM AgdaFile
indexFile src = do
  currentOptions <- TCM.useTC TCM.stPragmaOptions

  TCM.liftTCM $
    TCM.setCurrentRange (C.modPragmas . Imp.srcModule $ src) $
      -- Now reset the options
      TCM.setCommandLineOptions . TCM.stPersistentOptions . TCM.stPersistentState =<< TCM.getTC

  TCM.modifyTCLens TCM.stModuleToSource $ Map.insert (Imp.srcModuleName src) (srcFilePath $ Imp.srcOrigin src)

  TCM.localTC (\e -> e {TCM.envCurrentPath = Just (srcFilePath $ Imp.srcOrigin src)}) $ do
    let topLevel =
          TopLevel
            (srcFilePath $ Imp.srcOrigin src)
            (Imp.srcModuleName src)
            (C.modDecls $ Imp.srcModule src)
    ast <- TCM.liftTCM $ toAbstract topLevel
    abstractToIndex ast

-- let options = defaultOptions

-- TCM.liftTCM TCM.resetState

-- TCM.liftTCM $ TCM.setCommandLineOptions' rootPath options
-- TCM.liftTCM $ Imp.setOptionsFromSourcePragmas True src
-- loadPrims <- optLoadPrimitives <$> TCM.pragmaOptions

-- when loadPrims $ do
--   libdirPrim <- liftIO getPrimitiveLibDir

--   -- Turn off import-chasing messages.
--   -- We have to modify the persistent verbosity setting, since
--   -- getInterface resets the current verbosity settings to the persistent ones.

--   bracket_ (TCM.getsTC TCM.getPersistentVerbosity) TCM.putPersistentVerbosity $ do
--     TCM.modifyPersistentVerbosity
--       (Strict.Just . Trie.insert [] 0 . Strict.fromMaybe Trie.empty)
--     -- set root verbosity to 0

--     -- We don't want to generate highlighting information for Agda.Primitive.
--     TCM.liftTCM $
--       TCM.withHighlightingLevel TCM.None $
--         forM_ (Set.map (libdirPrim </>) TCM.primitiveModules) $ \f -> do
--           primSource <- Imp.parseSource (SourceFile $ mkAbsolute f)
--           Imp.checkModuleName' (Imp.srcModuleName primSource) (Imp.srcOrigin primSource)
--           void $ Imp.getNonMainInterface (Imp.srcModuleName primSource) (Just primSource)

-- TCM.liftTCM $ Imp.checkModuleName' (Imp.srcModuleName src) (Imp.srcOrigin src)

-- addImportCycleCheck (Imp.srcModuleName src) $
--   TCM.localTC (\e -> e {TCM.envCurrentPath = Just $ TCM.srcFileId srcFile}) $ do
--     let topLevel =
--           C.TopLevel
--             srcFile
--             (Imp.srcModuleName src)
--             (C.modDecls $ Imp.srcModule src)
--     ast <- TCM.liftTCM $ C.toAbstract topLevel

--     deps <- TCM.useTC TCM.stImportedModulesTransitive
--     moduleToSourceId <- TCM.useTC TCM.stModuleToSourceId
--     forM_ deps $ maybeUpdateCacheForDepFile moduleToSourceId

--     cache <- getCache
--     let entries = LSP.fromNormalizedUri . fst <$> Cache.getAllEntries cache
--     alwaysReportSDoc "lsp.cache" 20 $ return (text "cache entries:" <+> pretty entries)

--     let ds = C.topLevelDecls ast
--     let scope = C.topLevelScope ast

--     TCM.liftTCM TCM.activateLoadedFileCache

--     TCM.liftTCM TCM.cachingStarts
--     opts <- TCM.liftTCM $ TCM.useTC TCM.stPragmaOptions
--     me <- TCM.liftTCM TCM.readFromCachedLog
--     case me of
--       Just (TCM.Pragmas opts', _)
--         | opts == opts' ->
--             return ()
--       _ -> TCM.liftTCM TCM.cleanCachedLog
--     TCM.liftTCM $ TCM.writeToCurrentLog $ TCM.Pragmas opts

--     TCM.liftTCM $
--       mapM_ TC.checkDeclCached ds `TCM.finally_` TCM.cacheCurrentLog

--     TCM.liftTCM TCM.unfreezeMetas

--     TCM.liftTCM $ TCM.setScope scope

--     TCM.liftTCM $
--       TCM.stCurrentModule
--         `TCM.setTCLens'` Just
--           ( C.topLevelModuleName ast,
--             Imp.srcModuleName src
--           )

--     file <- Index.abstractToIndex $ C.topLevelDecls ast

--     -- return (file, moduleToSourceId, deps)
--     return file
