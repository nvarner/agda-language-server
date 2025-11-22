module Server.Model.AgdaLib
  ( AgdaLibOrigin (..),
    AgdaLib (AgdaLib),
    initAgdaLib,
    agdaLibName,
    agdaLibIncludes,
    agdaLibDependencies,
    agdaLibOrigin,
    isAgdaLibForUri,
    agdaLibFromFile,
    agdaLibToFile,
  )
where

import Agda.Interaction.Library
  ( AgdaLibFile (AgdaLibFile),
    LibName,
    OptionsPragma (OptionsPragma),
  )
import Agda.Interaction.Library.Base (libIncludes, libName, libPragmas)
import Agda.Syntax.Common.Pretty (Pretty, doubleQuotes, pretty, pshow, text, (<+>))
import Agda.Utils.Lens (Lens', set, (<&>), (^.))
import Agda.Utils.Null (empty)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidFilePath)
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import qualified Server.Filesystem as FS

data AgdaLibOrigin = FromFile !FS.FileId | Defaulted
  deriving (Show, Eq)

data AgdaLib = AgdaLib
  { _agdaLibName :: !LibName,
    _agdaLibIncludes :: ![FS.FileId],
    _agdaLibOptionsPragma :: !OptionsPragma,
    _agdaLibDependencies :: ![LibName],
    _agdaLibOrigin :: !AgdaLibOrigin
  }

instance Pretty AgdaLib where
  pretty agdaLib =
    text "AgdaLib"
      <+> doubleQuotes (pretty $ agdaLib ^. agdaLibName)
      <+> pshow (agdaLib ^. agdaLibOrigin)
      <+> text "includes:"
      <+> pretty (agdaLib ^. agdaLibIncludes)

initAgdaLibWithOrigin :: AgdaLibOrigin -> AgdaLib
initAgdaLibWithOrigin origin =
  let optionsPragma = OptionsPragma [] empty
   in AgdaLib empty [] optionsPragma [] origin

initAgdaLib :: AgdaLib
initAgdaLib = initAgdaLibWithOrigin Defaulted

agdaLibName :: Lens' AgdaLib LibName
agdaLibName f a = f (_agdaLibName a) <&> \x -> a {_agdaLibName = x}

agdaLibIncludes :: Lens' AgdaLib [FS.FileId]
agdaLibIncludes f a = f (_agdaLibIncludes a) <&> \x -> a {_agdaLibIncludes = x}

agdaLibOptionsPragma :: Lens' AgdaLib OptionsPragma
agdaLibOptionsPragma f a = f (_agdaLibOptionsPragma a) <&> \x -> a {_agdaLibOptionsPragma = x}

agdaLibDependencies :: Lens' AgdaLib [LibName]
agdaLibDependencies f a = f (_agdaLibDependencies a) <&> \x -> a {_agdaLibDependencies = x}

agdaLibOrigin :: Lens' AgdaLib AgdaLibOrigin
agdaLibOrigin f a = f (_agdaLibOrigin a) <&> \x -> a {_agdaLibOrigin = x}

isAgdaLibForUri :: AgdaLib -> LSP.NormalizedUri -> Bool
isAgdaLibForUri agdaLib uri = any (\include -> FS.fileIdToUri include `LSP.isUriAncestorOf` uri) (agdaLib ^. agdaLibIncludes)

-- | Given an 'AgdaLibFile' and the URI of that file, create the
-- corresponding 'AgdaLib'
agdaLibFromFile :: (MonadIO m, FS.IsFileId f) => AgdaLibFile -> f -> m AgdaLib
agdaLibFromFile agdaLibFile agdaLibIsFileId = do
  let agdaLibFileId = FS.toFileId agdaLibIsFileId
  agdaLibParent <- FS.fileIdParent agdaLibFileId
  let includeToAbsolute = case agdaLibParent of
        Nothing -> return . FS.LocalFilePath
        Just parent -> \include -> FS.LocalFilePath include `FS.fileIdRelativeTo` parent
  includes <- forM (agdaLibFile ^. libIncludes) includeToAbsolute
  return (initAgdaLibWithOrigin (FromFile agdaLibFileId))
    <&> set agdaLibName (agdaLibFile ^. libName)
    <&> set agdaLibIncludes includes
    <&> set agdaLibOptionsPragma (agdaLibFile ^. libPragmas)

-- | If the given `AgdaLib` came from a file, turn it back into one. Since
-- `AgdaLibFile`s are relative to an Agda file on the filesystem, the first
-- parameter is for the URI for the Agda file
agdaLibToFile :: LSP.NormalizedUri -> AgdaLib -> Maybe AgdaLibFile
agdaLibToFile relativeToUri agdaLib = case agdaLib ^. agdaLibOrigin of
  Defaulted -> Nothing
  FromFile fileId ->
    let includePaths = uriToPossiblyInvalidFilePath . FS.fileIdToUri <$> agdaLib ^. agdaLibIncludes
        uri = FS.fileIdToUri fileId
        above = LSP.uriHeightAbove uri relativeToUri
        filePath = LSP.uriToPossiblyInvalidFilePath uri
     in Just $ AgdaLibFile (agdaLib ^. agdaLibName) filePath above includePaths [] (agdaLib ^. agdaLibOptionsPragma)
