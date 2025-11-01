{-# LANGUAGE CPP #-}

module Agda.Interaction.Imports.More
  ( setOptionsFromSourcePragmas,
    checkModuleName',
    runPMDropWarnings,
    moduleName,
    runPM,
    beginningOfFile,
  )
where

import Agda.Interaction.FindFile (
    SourceFile (SourceFile),
    checkModuleName,
#if MIN_VERSION_Agda(2,8,0)
    rootNameModule,
#else
    moduleName,
#endif
  )
import Agda.Interaction.Imports (Source (..))
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Library (OptionsPragma (..), _libPragmas)
import Agda.Syntax.Common (TopLevelModuleName')
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Parser (
    moduleParser,
    parseFile,
#if MIN_VERSION_Agda(2,8,0)
    parse,
    moduleNameParser,
#else
    PM,
    runPMIO,
#endif
  )
import Agda.Syntax.Position
  ( Range,
    Range' (Range),
    RangeFile,
    getRange,
    intervalToRange,
    mkRangeFile,
    posToRange,
    posToRange',
    startPos,
#if MIN_VERSION_Agda(2,8,0)
    beginningOfFile,
    rangeFromAbsolutePath,
#endif
  )
import Agda.Syntax.TopLevelModuleName (
    TopLevelModuleName,
    RawTopLevelModuleName (..),
#if MIN_VERSION_Agda(2,8,0)
    rawTopLevelModuleNameForModule,
#endif
  )
import Agda.TypeChecking.Monad
  ( Interface,
    TCM,
    checkAndSetOptionsFromPragma,
    setCurrentRange,
    setOptionsFromPragma,
    setTCLens,
    stPragmaOptions,
    useTC,
#if MIN_VERSION_Agda(2,8,0)
    runPM,
    runPMDropWarnings,
#endif
  )
import qualified Agda.TypeChecking.Monad as TCM
import qualified Agda.TypeChecking.Monad.Benchmark as Bench
#if MIN_VERSION_Agda(2,8,0)
#else
import Agda.TypeChecking.Warnings (runPM)
#endif
import Agda.Utils.FileName (AbsolutePath)
import Agda.Utils.Monad (bracket_)
#if MIN_VERSION_Agda(2,8,0)
import qualified Data.Text as T
#endif
import qualified Data.Text.Lazy as TL
import Control.Monad.Error.Class (
#if MIN_VERSION_Agda(2,8,0)
    catchError,
#else
    throwError,
#endif
  )
#if MIN_VERSION_Agda(2,8,0)
import Agda.Utils.Singleton (singleton)
#else
import Agda.Syntax.Common.Pretty (pretty)
#endif

srcFilePath :: SourceFile -> TCM AbsolutePath
#if MIN_VERSION_Agda(2,8,0)
srcFilePath = TCM.srcFilePath
#else
srcFilePath (SourceFile f) = return f
#endif

#if MIN_VERSION_Agda(2,8,0)
-- beginningOfFile was generalized in Agda 2.8.0 to support the features we
-- need, so we just import it
#else
beginningOfFile :: RangeFile -> Range
beginningOfFile rf = posToRange (startPos $ Just rf) (startPos $ Just rf)
#endif

#if MIN_VERSION_Agda(2,8,0)
-- runPMDropWarnings was introduced in Agda 2.8.0, so we just import it
#else
runPMDropWarnings :: PM a -> TCM a
runPMDropWarnings m = do
  (res, _ws) <- runPMIO m
  case res of
    Left  e -> throwError $ TCM.Exception (getRange e) (pretty e)
    Right a -> return a
#endif

-- Unexported Agda functions

srcDefaultPragmas :: Imp.Source -> [OptionsPragma]
srcDefaultPragmas src = map _libPragmas (Imp.srcProjectLibs src)

srcFilePragmas :: Imp.Source -> [OptionsPragma]
srcFilePragmas src = pragmas
  where
    cpragmas = C.modPragmas (Imp.srcModule src)
    pragmas =
      [ OptionsPragma
          { pragmaStrings = opts,
            pragmaRange = r
          }
      | C.OptionsPragma r opts <- cpragmas
      ]

-- | Set options from a 'Source' pragma, using the source
--   ranges of the pragmas for error reporting. Flag to check consistency.
setOptionsFromSourcePragmas :: Bool -> Imp.Source -> TCM ()
setOptionsFromSourcePragmas checkOpts src = do
  mapM_ setOpts (srcDefaultPragmas src)
  mapM_ setOpts (srcFilePragmas src)
  where
    setOpts
      | checkOpts = checkAndSetOptionsFromPragma
      | otherwise = setOptionsFromPragma

-- Andreas, 2016-07-11, issue 2092
-- The error range should be set to the file with the wrong module name
-- not the importing one (which would be the default).
checkModuleName' :: TopLevelModuleName' Range -> SourceFile -> TCM ()
checkModuleName' m f =
  setCurrentRange m $ checkModuleName m f Nothing

#if MIN_VERSION_Agda(2,8,0)
-- moduleName was exported until 2.8.0

-- | Computes the module name of the top-level module in the given file.
--
-- If no top-level module name is given, then an attempt is made to
-- use the file name as a module name.

moduleName ::
     AbsolutePath
     -- ^ The path to the file.
  -> C.Module
     -- ^ The parsed module.
  -> TCM TopLevelModuleName
moduleName file parsedModule = Bench.billTo [Bench.ModuleName] $ do
  let defaultName = rootNameModule file
      raw = rawTopLevelModuleNameForModule parsedModule
  TCM.topLevelModuleName =<< if C.isNoName raw
    then setCurrentRange (rangeFromAbsolutePath file) $ do
      m <- runPM (fst <$> parse moduleNameParser defaultName)
             `catchError` \_ ->
           TCM.typeError $ TCM.InvalidFileName file TCM.DoesNotCorrespondToValidModuleName
      case m of
        C.Qual{} ->
          TCM.typeError $ TCM.InvalidFileName file $
            TCM.RootNameModuleNotAQualifiedModuleName $ T.pack defaultName
        C.QName{} ->
          return $ RawTopLevelModuleName
            { rawModuleNameRange = getRange m
            , rawModuleNameParts = singleton (T.pack defaultName)
            , rawModuleNameInferred = True
                -- Andreas, 2025-06-21, issue #7953:
                -- Remember we made up this module name to improve errors.
            }
    else return raw
#endif
