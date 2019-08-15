{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Filesystem where

import ClassyPrelude

import Control.Monad.Freer.TH

data Filesystem r where
  ReadFile :: FilePath -> Filesystem ByteString
  WriteFile :: FilePath -> ByteString -> Filesystem ()
  -- TODO: add effects for more things, make sure we can implement fileystem import
makeEffect ''Filesystem
