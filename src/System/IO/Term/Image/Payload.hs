{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Data structure representing the information needed for the serialization
    of images for the iTerm2 terminal emulator.
    https://iterm2.com/documentation-images.html
 -}
module System.IO.Term.Image.Payload where

import MyPrelude

import Data.ByteString.Builder
import Data.ByteString.Base64.Lazy
import Data.String (fromString)
import System.IO (FilePath)
import Control.Lens
import Control.Monad.State

import System.IO.Term.BinarySerialize

data Dimension
  = Cells Int -- ^ character cells the image should take up
  | Pixels Int -- ^ pixels to display the image as
  | Percent Int -- ^ percent of terminal session to use
  | Auto -- ^ automatic size based on file
  deriving (Show, Eq, Ord)

instance BinarySerialize Dimension where
  intoBuilder (Cells x) = fromString (show x)
  intoBuilder (Pixels x) = fromString (show x) <> "px"
  intoBuilder (Percent x) = fromString (show x) <> "%"
  intoBuilder Auto = "auto"

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
  intoBuilder a = (`execState` "") $ do
    withJust (view payloadArgsFilename a) (\x ->
      modify (<> ("name=" <> fnEncode x <> ";")))
    withJust (view payloadArgsSize a) (\x ->
      modify (<> ("size=" <> fromString (show x) <> ";")))
    modify (<> ("width=" <> intoBuilder (view payloadArgsWidth a) <> ";"))
    modify (<> ("height=" <> intoBuilder (view payloadArgsHeight a) <> ";"))
    modify (<> ("preserveAspectRatio=" <> bEncode (view payloadArgsPreserveAspectRatio a) <> ";"))
    modify (<> ("inline=" <> bEncode (view payloadArgsInline a)))
    where
      fnEncode = lazyByteString . encode . fromString
      bEncode True = "1"
      bEncode False = "0"

data Payload
  = Payload
  { _payloadArgs :: PayloadArgs
  , _payloadData :: LByteString -- ^ needs to be base 64 encoded
  } deriving (Show, Eq, Ord)
makeLenses ''Payload

instance BinarySerialize Payload where
  intoBuilder p =
    charUtf8 '\x1b' <> "]" <> "1337" <> ";"
    <> "File=[" <> intoBuilder (view payloadArgs p) <> "]"
    <> ":" <> lazyByteString (encode (view payloadData p)) <> charUtf8 '\x07'
