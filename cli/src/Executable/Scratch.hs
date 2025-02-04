{-# LANGUAGE CPP #-}

-- | This module is a scratchpad for testing out new features or debugging
module Executable.Scratch where

import Foreign
import Foreign.C
import MyPrelude
import System.MacOS.NSFileCoordinator

#ifdef darwin_HOST_OS

main :: IO ()
main = do
  fileCoordinator <- nsFileCoordinator_init
  url <- withCString "/Users/devin/iCloud Drive/test.json" $ \cannonicalPath ->
    nsURL_initFileURL cannonicalPath (fromBool True)
  m_NSFileCoordinator_coordinateReadingItem
    fileCoordinator
    url
    k_NSFileCoordinatorReadingWithoutChanges
    nullPtr
    (\newURL -> do
      putStrLn "got access to read file"
      pure ())

#else

main :: IO ()
main = putStrLn "Not on macOS"

#endif
