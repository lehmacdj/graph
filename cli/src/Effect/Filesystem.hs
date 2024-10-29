{-# LANGUAGE TemplateHaskell #-}

module Effect.Filesystem where

import Effect.UserError
import MyPrelude
import System.Directory.Tree

-- | TODO: add effects for more things, make sure we can implement fileystem import
data FileSystem m r where
  ReadFileE :: FilePath -> FileSystem m ByteString
  WriteFileE :: FilePath -> ByteString -> FileSystem m ()

makeSem ''FileSystem

data FileSystemTree m r where
  ReadDirectory :: FilePath -> FileSystemTree m (DirTree ByteString)
  WriteDirectory :: FilePath -> DirTree ByteString -> FileSystemTree m (DirTree ())

makeSem ''FileSystemTree

-- runFileSystemIO
--   :: (Member (Embed IO) effs, Member Throw effs)
--   => Sem (FileSystem : effs) ~> Sem effs
-- runFileSystemIO = interpret $ \case
--   ReadFile

runFileSystemTreeIO ::
  (Member (Embed IO) effs, Member (Error UserError) effs) =>
  Sem (FileSystemTree : effs) ~> Sem effs
runFileSystemTreeIO = interpret $ \case
  ReadDirectory fp ->
    liftIO $ dirTree <$> readDirectoryWithL readFile fp
  WriteDirectory fp dt ->
    liftIO $ dirTree <$> writeDirectoryWith writeFile (fp :/ dt)
