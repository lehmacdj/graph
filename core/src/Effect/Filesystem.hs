{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Filesystem where

import Control.Lens
import MyPrelude
import System.Directory.Tree
import UserError

-- | TODO: add effects for more things, make sure we can implement fileystem import
data FileSystem m r where
  ReadFileE :: FilePath -> FileSystem m ByteString
  WriteFileE :: FilePath -> ByteString -> FileSystem m ()

makeSem ''FileSystem

data FileSystemTree m r where
  ReadDirectory :: FilePath -> FileSystemTree m (DirTree LByteString)

makeSem ''FileSystemTree

-- runFileSystemIO
--   :: (Member (Embed IO) effs, Member Throw effs)
--   => Sem (FileSystem : effs) ~> Sem effs
-- runFileSystemIO = interpret $ \case
--   ReadFile

runFileSystemTreeIO ::
  (Member (Embed IO) effs, Member ThrowUserError effs) =>
  Sem (FileSystemTree : effs) ~> Sem effs
runFileSystemTreeIO = interpret $ \case
  ReadDirectory fp ->
    liftIO $
      dirTree <$> readDirectoryWithL (fmap (view lazy) . readFile) fp
