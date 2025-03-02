{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.FileSystem where

import Error.UserError
import MyPrelude
import System.Directory qualified as Directory
import System.Directory.Tree

data FileSystem m r where
  ReadDirectory :: FilePath -> FileSystem m (DirTree ByteString)
  WriteDirectory :: FilePath -> DirTree ByteString -> FileSystem m (DirTree ())
  CanonicalizePath ::
    -- | filepath to convert
    FilePath ->
    FileSystem m (Either IOError FilePath)

makeSem ''FileSystem

runFileSystemIO ::
  (Member (Embed IO) effs, Member (Error UserError) effs) =>
  Sem (FileSystem : effs) ~> Sem effs
runFileSystemIO = interpret $ \case
  ReadDirectory fp ->
    liftIO $ dirTree <$> readDirectoryWithL readFile fp
  WriteDirectory fp dt ->
    liftIO $ dirTree <$> writeDirectoryWith writeFile (fp :/ dt)
  CanonicalizePath fp ->
    liftIO . try . Directory.canonicalizePath $ fp
