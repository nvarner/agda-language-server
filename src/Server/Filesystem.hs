{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Filesystem
  ( FileId,
    fileIdToUri,
    fileIdToPossiblyInvalidAbsolutePath,
    fileIdParent,
    fileIdExtension,
    IsFileId (..),
    MonadFilesystem (..),
    Provider,
    getFileContents,
    getChildren,
    LspVirtualFilesystem (..),
    OsFilesystem (..),
    ProviderWrapper (..),
    Layered (..),
  )
where

import Agda.Utils.Either (maybeRight)
import Agda.Utils.FileName (AbsolutePath, absolute, sameFile)
import Agda.Utils.IO (catchIO)
import Agda.Utils.IO.UTF8 (ReadException, readTextFile)
import Agda.Utils.List (nubM)
import Agda.Utils.Monad (foldM, forM)
import Control.Exception (try, tryJust)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Strict as Strict
import Data.Text (Text)
import qualified Data.Text as Text
import Language.LSP.Protocol.Types (NormalizedUri)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Options (Config)
import Server.VfsIndex (VfsIndex, getEntry, getEntryChildren)
import System.Directory (canonicalizePath, doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Error (isPermissionError)

data FileId
  = Uri !LSP.NormalizedUri
  | LocalFilePath !FilePath

referToSameFile :: FileId -> FileId -> IO Bool
referToSameFile a b =
  case (tryFileIdToFilePath a, tryFileIdToFilePath b) of
    (Just a, Just b) -> do
      absA <- absolute a
      absB <- absolute b
      sameFile absA absB
    (Just _, Nothing) -> return False
    (Nothing, Just _) -> return False
    (Nothing, Nothing) -> do
      let uriA = fileIdToUri a
      let uriB = fileIdToUri b
      return $ uriA == uriB

fileIdToUri :: FileId -> LSP.NormalizedUri
fileIdToUri = \case
  Uri uri -> uri
  LocalFilePath filePath -> LSP.toNormalizedUri $ LSP.filePathToUri filePath

tryFileIdToFilePath :: FileId -> Maybe FilePath
tryFileIdToFilePath = \case
  Uri uri -> LSP.uriToFilePath $ LSP.fromNormalizedUri uri
  LocalFilePath filePath -> Just filePath

fileIdToPossiblyInvalidAbsolutePath :: (MonadIO m) => FileId -> m AbsolutePath
fileIdToPossiblyInvalidAbsolutePath (Uri uri) = LSP.uriToPossiblyInvalidAbsolutePath uri
fileIdToPossiblyInvalidAbsolutePath (LocalFilePath path) = liftIO $ absolute path

fileIdParent :: (MonadIO m) => FileId -> m (Maybe FileId)
fileIdParent (Uri uri) = return $ Uri <$> LSP.uriParent uri
fileIdParent (LocalFilePath path) = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then liftIO $ dirParent path
    else return $ fileParent path
  where
    -- Taken from Agda's findProjectConfig
    dirParent :: FilePath -> IO (Maybe FileId)
    dirParent dirPath = do
      up <- canonicalizePath $ path </> ".."
      if up == path then return Nothing else return $ Just $ LocalFilePath up

    fileParent :: FilePath -> Maybe FileId
    fileParent filePath = Just $ LocalFilePath $ takeDirectory filePath

fileIdExtension :: FileId -> Text
fileIdExtension (Uri uri) = LSP.uriExtension uri
fileIdExtension (LocalFilePath path) = Text.pack $ takeExtension path

class IsFileId a where
  toFileId :: a -> FileId

instance IsFileId FileId where
  toFileId = id

instance IsFileId LSP.Uri where
  toFileId = Uri . LSP.toNormalizedUri

instance IsFileId LSP.NormalizedUri where
  toFileId = Uri

instance IsFileId FilePath where
  toFileId = LocalFilePath

data File = File {fileContents :: !Text}

-- | A monad where a filesystem provider can run. This is a bit of a hack. Its
-- capabilities are really just chosen to support the providers we need. We may
-- as well use 'ServerM', except that we would get circular dependencies when we
-- include a provider in its environment.
class (MonadLsp Config m) => MonadFilesystem m where
  askVfsIndex :: m VfsIndex

class Provider a where
  getFileImpl :: (MonadFilesystem m) => a -> FileId -> m (Maybe File)
  getChildrenImpl :: (MonadFilesystem m) => a -> FileId -> m [FileId]

getFile :: (MonadFilesystem m, Provider a, IsFileId f) => a -> f -> m (Maybe File)
getFile provider fileId = getFileImpl provider (toFileId fileId)

getFileContents :: (MonadFilesystem m, Provider a, IsFileId f) => a -> f -> m (Maybe Text)
getFileContents provider fileId = do
  file <- getFile provider fileId
  return $ fileContents <$> file

getChildren :: (MonadFilesystem m, Provider a, IsFileId f) => a -> f -> m [FileId]
getChildren provider fileId = getChildrenImpl provider (toFileId fileId)

data LspVirtualFilesystem = LspVirtualFilesystem

instance Provider LspVirtualFilesystem where
  getFileImpl _provider fileId = do
    let uri = fileIdToUri fileId
    vfile <- LSP.getVirtualFile uri
    case vfile of
      Nothing -> return Nothing
      Just vfile -> do
        let contents = VFS.virtualFileText vfile
        return $ Just $ File contents

  getChildrenImpl _provider parent = do
    let uri = fileIdToUri parent
    vfsIndex <- askVfsIndex
    let children = concat $ getEntryChildren <$> getEntry uri vfsIndex
    return $ Uri <$> children

data OsFilesystem = OsFilesystem

instance Provider OsFilesystem where
  getFileImpl _provider fileId =
    case tryFileIdToFilePath fileId of
      Nothing -> return Nothing
      Just filePath -> do
        let result :: IO (Either E.IOException File)
            result = try $ do
              contents <- Strict.toStrict <$> readTextFile filePath
              return $ File contents
        liftIO $ maybeRight <$> result

  getChildrenImpl _provider parent =
    case tryFileIdToFilePath parent of
      Nothing -> return []
      Just parent ->
        liftIO
          $ (flip catchIO)
            (\e -> if isPermissionError e then return [] else E.throwIO e)
          $ do
            relativeChildren <- liftIO $ getDirectoryContents parent
            let absoluteChildren = fmap (\child -> parent </> child) relativeChildren
            return $ LocalFilePath <$> absoluteChildren

-- TODO: Proper WASM support probably means custom extensions of LSP for
-- filesystem access. When/if these are implemented, they should get a provider
-- here. Include the provider in the default provider instance when running WASM
-- with whatever client the custom extension is for, and hopefully it should all
-- Just Work.

data ProviderWrapper = forall a. (Provider a) => Wrap a

instance Provider ProviderWrapper where
  getFileImpl (Wrap provider) = getFileImpl provider

  getChildrenImpl (Wrap provider) = getChildrenImpl provider

data Layered = Layered {layeredProviders :: ![ProviderWrapper]}

instance Provider Layered where
  getFileImpl (Layered providers) fileId = do
    let results = flip getFileImpl fileId <$> providers
    foldM
      ( \file candidate -> case file of
          Just _ -> return file
          Nothing -> candidate
      )
      Nothing
      results

  getChildrenImpl (Layered providers) fileId = do
    allChildren <- forM providers $ \provider -> getChildrenImpl provider fileId
    liftIO $ nubM referToSameFile (concat allChildren)
