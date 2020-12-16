-- | Simple implementation of iterm2 image protocol for displaying images.
module System.IO.Term.Image where

import Data.ByteString.Builder
import MyPrelude hiding (putStrLn)
import System.IO
import System.IO.Term.BinarySerialize
import System.IO.Term.Image.Payload

printImage :: LByteString -> IO ()
printImage d = do
  hPutBuilder stdout . intoBuilder $ payload
  putStrLn ""
  where
    payload = Payload (PayloadArgs (Just "unnamed.jpg") Nothing Auto Auto True True) d
