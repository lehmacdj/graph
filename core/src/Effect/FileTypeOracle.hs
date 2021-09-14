{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for testing the filetype of a data file. Underlyingly uses file(1)
-- shell command.
module Effect.FileTypeOracle
  ( runFileTypeOracle,
    getExtension,
    FileTypeOracle (..),
  )
where

import MyPrelude
import System.Process.Typed

data FileTypeOracle m r where
  GetExtension :: FilePath -> FileTypeOracle m Text

makeSem ''FileTypeOracle

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
runFileTypeOracle :: Member (Embed IO) r => Sem (FileTypeOracle : r) a -> Sem r a
runFileTypeOracle = interpret \case
  GetExtension fp ->
    fmap (decodeUtf8 . toStrict . fst)
      . readProcess_
      $ proc "file" ["--extension", "-b", fp]
