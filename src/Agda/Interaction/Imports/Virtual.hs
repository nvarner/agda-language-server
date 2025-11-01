{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Agda.Interaction.Imports.Virtual
  ( VSourceFile (..),
    vSrcFileId,
    vSrcFromUri,
    parseVSource,
  )
where

#if MIN_VERSION_Agda(2,8,0)
#else
import Agda.Interaction.FindFile (SourceFile (SourceFile))
#endif
import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.Imports.More as Imp
import Agda.Syntax.Parser (moduleParser, parseFile)
import Agda.Syntax.Position (mkRangeFile)
import qualified Agda.TypeChecking.Monad as TCM
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import qualified Data.Strict as Strict
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidAbsolutePath)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS

data VSourceFile = VSourceFile
  { vSrcFileSrcFile :: TCM.SourceFile,
    vSrcUri :: LSP.NormalizedUri,
    vSrcVFile :: VFS.VirtualFile
  }

vSrcFilePath :: (TCM.MonadFileId m) => VSourceFile -> m TCM.AbsolutePath
vSrcFilePath = TCM.srcFilePath . vSrcFileSrcFile

vSrcFileId :: VSourceFile -> TCM.FileId
vSrcFileId = TCM.srcFileId . vSrcFileSrcFile

#if MIN_VERSION_Agda(2,8,0)
vSrcFromUri ::
  (TCM.MonadFileId m, MonadIO m) =>
  LSP.NormalizedUri ->
  VFS.VirtualFile ->
  m VSourceFile
vSrcFromUri normUri file = do
  absPath <- uriToPossiblyInvalidAbsolutePath normUri
  src <- TCM.srcFromPath absPath
  return $ VSourceFile src normUri file
#else
vSrcFromUri ::
  (MonadIO m) =>
  LSP.NormalizedUri ->
  VFS.VirtualFile ->
  m VSourceFile
vSrcFromUri normUri file = do
  absPath <- uriToPossiblyInvalidAbsolutePath normUri
  let src = SourceFile absPath
  return $ VSourceFile src normUri file
#endif

-- | Based on @parseSource@
parseVSource :: (TCM.MonadTCM m) => VSourceFile -> m Imp.Source
parseVSource vSrcFile = TCM.liftTCM $ do
  let sourceFile = vSrcFileSrcFile vSrcFile
  f <- vSrcFilePath vSrcFile

  let rf0 = mkRangeFile f Nothing
  TCM.setCurrentRange (Imp.beginningOfFile rf0) $ do
    let sourceStrict = VFS.virtualFileText $ vSrcVFile vSrcFile
    let source = Strict.toLazy sourceStrict
    let txt = Text.unpack sourceStrict

    parsedModName0 <-
      Imp.moduleName f . fst . fst =<< do
        Imp.runPMDropWarnings $ parseFile moduleParser rf0 txt

    let rf = mkRangeFile f $ Just parsedModName0
    ((parsedMod, attrs), fileType) <- Imp.runPM $ parseFile moduleParser rf txt
    parsedModName <- Imp.moduleName f parsedMod

    -- TODO: handle libs properly
    let libs = []

    return
      Imp.Source
        { Imp.srcText = source,
          Imp.srcFileType = fileType,
          Imp.srcOrigin = sourceFile,
          Imp.srcModule = parsedMod,
          Imp.srcModuleName = parsedModName,
          Imp.srcProjectLibs = libs,
          Imp.srcAttributes = attrs
        }
