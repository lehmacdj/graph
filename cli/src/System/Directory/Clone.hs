{-# LANGUAGE CPP #-}

-- | Bindings for APFS clone facilities that fallback to an ordinary copy when
-- that is unavailable.
module System.Directory.Clone where

import MyPrelude

#if darwin_HOST_OS
import Data.ByteString (useAsCString)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
#else
import System.Directory (copyFile)
#endif

cloneFile :: (HasCallStack) => FilePath -> FilePath -> IO ()

#ifdef darwin_HOST_OS
-- In principle we could test if clone was available on the macOS system for
-- the specified path before calling c_cloneFile using code similar to the following:
-- https://github.com/lehmacdj/c-experiments/blob/bda851532909de0972e53998d73a8ed9270dcfdc/c-api-testing/darwin_test_if_clone_exists.c
-- However, we don't do that and just assume that on macOS clone is supported
-- for a couple of reasons
foreign import ccall "<sys/clonefile.h> clonefile"
  c_cloneFile :: CString -> CString -> CInt -> IO ()

-- We use this really convoluted way of converting a String (FilePath) to a
-- CString to ensure that we encode the string using utf8 so that we can
-- support file names containing unicode characters better (maybe). The
-- multiple copying of the string is probably a fine price to pay
cloneFile src dst =
  let asCString s = useAsCString (encodeUtf8 (pack s))
   in asCString src \src' ->
        asCString dst \dst' ->
          c_cloneFile src' dst' 0
#else
cloneFile = copyFile
#endif
