{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Filesystem where

import ClassyPrelude

import Control.Monad.Freer.TH

import System.Directory.Tree

data FileSystem r where
  ReadFile :: FilePath -> FileSystem ByteString
  WriteFile :: FilePath -> ByteString -> FileSystem ()
  -- TODO: add effects for more things, make sure we can implement fileystem import
makeEffect ''FileSystem

data FileSystemTree r where
  ReadDirectory :: FilePath -> FileSystemTree (DirTree LByteString)
makeEffect ''FileSystemTree
