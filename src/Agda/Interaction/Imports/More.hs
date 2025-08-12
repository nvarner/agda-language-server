-- | This module reexports unexported functions
module Agda.Interaction.Imports.More
  ( setOptionsFromSourcePragmas,
    checkModuleName',
  )
where

import Agda.Interaction.FindFile (SourceFile, checkModuleName)
import Agda.Interaction.Imports (Source)
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Library (OptionsPragma (..), _libPragmas)
import Agda.Syntax.Common (TopLevelModuleName')
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Position (Range)
import Agda.Syntax.TopLevelModuleName (TopLevelModuleName)
import Agda.TypeChecking.Monad (Interface, TCM, checkAndSetOptionsFromPragma, setCurrentRange, setOptionsFromPragma, setTCLens, stPragmaOptions, useTC)
import Agda.Utils.Monad (bracket_)

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
