{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Filesystem where

import ClassyPrelude

import Control.Monad.Freer.TH
import Control.Monad.Freer

import Control.Lens

import Effect.Throw

import System.Directory.Tree

data FileSystem r where
  ReadFileE :: FilePath -> FileSystem ByteString
  WriteFileE :: FilePath -> ByteString -> FileSystem ()
  -- TODO: add effects for more things, make sure we can implement fileystem import
makeEffect ''FileSystem

data FileSystemTree r where
  ReadDirectory :: FilePath -> FileSystemTree (DirTree LByteString)
makeEffect ''FileSystemTree

-- runFileSystemIO
--   :: (MonadIO m, LastMember m effs, Member Throw effs)
--   => Eff (FileSystem : effs) ~> Eff effs
-- runFileSystemIO = interpret $ \case
--   ReadFile

runFileSystemTreeIO
  :: (MonadIO m, LastMember m effs, Member Throw effs)
  => Eff (FileSystemTree : effs) ~> Eff effs
runFileSystemTreeIO = interpret $ \case
  ReadDirectory fp -> liftIO $
    dirTree <$> readDirectoryWithL (fmap (view lazy) . readFile) fp
