{-| Simple implementation of iterm2 image protocol for displaying images.
 -}
module System.IO.Term.Image where

import System.IO.Term.BinarySerialize
import System.IO.Term.Image.Payload
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder

import System.IO

printImage :: ByteString -> IO ()
printImage d = do
  hPutBuilder stdout . intoBuilder $ payload
  putStrLn ""
  where
    payload = Payload (PayloadArgs (Just "unnamed.jpg") Nothing Auto Auto True True) d