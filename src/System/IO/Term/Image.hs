{-| Simple implementation of iterm2 image protocol for displaying images.
 -}
module System.IO.Term.Image where

import System.IO.Term.BinarySerialize
import System.IO.Term.Image.Payload
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as B

import System.IO

printImage :: ByteString -> IO ()
printImage d = do
  hPutBuilder stdout . toBuilder $ payload
  putStrLn ""
  where
    payload = Payload (PayloadArgs (Just "unnamed.jpg") Nothing Auto Auto True True) d
