{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Data structure representing the information needed for the serialization
    of images for the iTerm2 terminal emulator.
    https://iterm2.com/documentation-images.html
 -}
module System.IO.Term.Image.Payload where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Base64.Lazy
import Data.String (fromString)
import System.IO (FilePath)
import Control.Lens
import Control.Monad.State

import System.IO.Term.BinarySerialize
import Util

data Dimension
  = Cells Int -- ^ character cells the image should take up
  | Pixels Int -- ^ pixels to display the image as
  | Percent Int -- ^ percent of terminal session to use
  | Auto -- ^ automatic size based on file
  deriving (Show, Eq, Ord)

instance BinarySerialize Dimension where
  toBuilder (Cells x) = fromString (show x)
  toBuilder (Pixels x) = fromString (show x) <> "px"
  toBuilder (Percent x) = fromString (show x) <> "%"
  toBuilder Auto = "auto"

data PayloadArgs
  = PayloadArgs
  { _payloadArgsFilename :: Maybe FilePath -- ^ needs to be base 64 encoded
  , _payloadArgsSize :: Maybe Int -- ^ size in bytes
  , _payloadArgsWidth :: Dimension
  , _payloadArgsHeight :: Dimension
  , _payloadArgsPreserveAspectRatio :: Bool -- ^ default true, {1, 0} encoding
  , _payloadArgsInline :: Bool -- ^ should the image be inlined, default true {1, 0}
  }
  deriving (Show, Eq, Ord)
makeLenses ''PayloadArgs

instance BinarySerialize PayloadArgs where
  toBuilder a = (`execState` "") $ do
    withJust (view payloadArgsFilename a) (\x ->
      modify (<> ("name=" <> fnEncode x <> ";")))
    withJust (view payloadArgsSize a) (\x ->
      modify (<> ("size=" <> fromString (show x) <> ";")))
    modify (<> ("width=" <> toBuilder (view payloadArgsWidth a) <> ";"))
    modify (<> ("height=" <> toBuilder (view payloadArgsHeight a) <> ";"))
    modify (<> ("preserveAspectRatio=" <> bEncode (view payloadArgsPreserveAspectRatio a) <> ";"))
    modify (<> ("inline=" <> bEncode (view payloadArgsInline a)))
    where
      fnEncode = lazyByteString . encode . fromString
      bEncode True = "1"
      bEncode False = "0"

data Payload
  = Payload
  { _payloadArgs :: PayloadArgs
  , _payloadData :: ByteString -- ^ needs to be base 64 encoded
  } deriving (Show, Eq, Ord)
makeLenses ''Payload

instance BinarySerialize Payload where
  toBuilder p =
    charUtf8 '\x1b' <> "]" <> "1337" <> ";"
    <> "File=[" <> toBuilder (view payloadArgs p) <> "]"
    <> ":" <> lazyByteString (encode (view payloadData p)) <> charUtf8 '\x07'
