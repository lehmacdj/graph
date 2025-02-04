{-# LANGUAGE CPP #-}

-- | This module is a scratchpad for testing out new features or debugging
module Executable.Scratch where

import Foreign
import Foreign.C
import MyPrelude
import System.MacOS.NSFileCoordinator.RawBindings

#ifdef darwin_HOST_OS

main :: IO ()
main = do
  putStrLn "Testing NSFileCoordinator FFI functions..."

  -- Initialize coordinator
  fileCoordinator <- nsFileCoordinator_init

  -- Create test URLs
  url1 <- withCString "/Users/devin/iCloud Drive/test.json" $ \path ->
    nsURL_initFileURL path (fromBool False)
  url2 <- withCString "/Users/devin/iCloud Drive/foo.json" $ \path ->
    nsURL_initFileURL path (fromBool False)

  -- Test reading
  putStrLn "\nTesting coordinateReadingItem..."
  m_NSFileCoordinator_coordinateReadingItem
    fileCoordinator
    url1
    k_NSFileCoordinatorReadingWithoutChanges
    nullPtr
    (\newURL -> putStrLn "  Got read access to file 1")

  -- Test writing
  putStrLn "\nTesting coordinateWritingItem..."
  m_NSFileCoordinator_coordinateWritingItem
    fileCoordinator
    url1
    k_NSFileCoordinatorWritingContentIndependentMetadataOnly
    nullPtr
    (\newURL -> putStrLn "  Got write access to file 1")

  -- Test reading and writing
  putStrLn "\nTesting coordinateReadingAndWritingItem..."
  m_NSFileCoordinator_coordinateReadingAndWritingItem
    fileCoordinator
    url1
    k_NSFileCoordinatorReadingWithoutChanges
    url2
    k_NSFileCoordinatorWritingContentIndependentMetadataOnly
    nullPtr
    (\readURL writeURL -> putStrLn "  Got read access to file 1 and write access to file 2")

  -- Test writing multiple
  putStrLn "\nTesting coordinateWritingItems..."
  m_NSFileCoordinator_coordinateWritingItems
    fileCoordinator
    url1
    k_NSFileCoordinatorWritingContentIndependentMetadataOnly
    url2
    k_NSFileCoordinatorWritingContentIndependentMetadataOnly
    nullPtr
    (\newURL1 newURL2 -> putStrLn "  Got write access to both files")

  putStrLn "\nAll tests complete!"

#else

main :: IO ()
main = putStrLn "Not on macOS"

#endif
