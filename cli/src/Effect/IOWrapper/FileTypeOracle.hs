{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for testing the filetype of a data file. Underlyingly uses file(1)
-- shell command.
module Effect.IOWrapper.FileTypeOracle
  ( runFileTypeOracle,
    getExtension,
    FileTypeOracle (..),
  )
where

import qualified Data.Text as T
import MyPrelude
import System.Process.Typed

data FileTypeOracle m r where
  GetExtension :: FilePath -> FileTypeOracle m Text

makeSem ''FileTypeOracle

-- | Run FreshNID as a computation with a state representing the next value to
-- use for a Fresh NID.
-- | TODO: make this handle files that don't exist and errors better; maybe
-- return empty string for files that "file" can't determine the extension for?
-- Maybe need to alter type signature of effect to handle missing files better
-- though
runFileTypeOracle :: Member (Embed IO) r => Sem (FileTypeOracle : r) a -> Sem r a
runFileTypeOracle = interpret \case
  GetExtension fp ->
    fmap
      ( (\x -> if x == "???" then "data" else x)
          . T.strip
          . decodeUtf8
          . toStrict
          . fst
      )
      . readProcess_
      $ proc "file" ["--extension", "-b", fp]
