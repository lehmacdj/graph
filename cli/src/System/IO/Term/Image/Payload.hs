-- | Data structure representing the information needed for the serialization
--    of images for the iTerm2 terminal emulator.
--    https://iterm2.com/documentation-images.html
module System.IO.Term.Image.Payload where

import Control.Monad.State
import Data.ByteString.Base64.Lazy
import Data.ByteString.Builder
import MyPrelude
import System.IO.Term.BinarySerialize

data Dimension
  = -- | character cells the image should take up
    Cells Int
  | -- | pixels to display the image as
    Pixels Int
  | -- | percent of terminal session to use
    Percent Int
  | -- | automatic size based on file
    Auto
  deriving (Show, Eq, Ord)

instance BinarySerialize Dimension where
  intoBuilder (Cells x) = fromString (show x)
  intoBuilder (Pixels x) = fromString (show x) <> "px"
  intoBuilder (Percent x) = fromString (show x) <> "%"
  intoBuilder Auto = "auto"

data PayloadArgs = PayloadArgs
  { -- | needs to be base 64 encoded
    payloadArgsFilename :: Maybe FilePath,
    -- | size in bytes
    payloadArgsSize :: Maybe Int,
    payloadArgsWidth :: Dimension,
    payloadArgsHeight :: Dimension,
    -- | default true, {1, 0} encoding
    payloadArgsPreserveAspectRatio :: Bool,
    -- | should the image be inlined, default true {1, 0}
    payloadArgsInline :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance BinarySerialize PayloadArgs where
  intoBuilder a = (`execState` "") $ do
    withJust
      a.payloadArgsFilename
      ( \x ->
          modify (<> ("name=" <> fnEncode x <> ";"))
      )
    withJust
      a.payloadArgsSize
      ( \x ->
          modify (<> ("size=" <> fromString (show x) <> ";"))
      )
    modify (<> ("width=" <> intoBuilder a.payloadArgsWidth <> ";"))
    modify (<> ("height=" <> intoBuilder a.payloadArgsHeight <> ";"))
    modify (<> ("preserveAspectRatio=" <> bEncode a.payloadArgsPreserveAspectRatio <> ";"))
    modify (<> ("inline=" <> bEncode a.payloadArgsInline))
    where
      fnEncode = lazyByteString . encode . fromString
      bEncode True = "1"
      bEncode False = "0"

data Payload = Payload
  { payloadArgs :: PayloadArgs,
    -- | needs to be base 64 encoded
    payloadData :: LByteString
  }
  deriving (Show, Eq, Ord, Generic)

instance BinarySerialize Payload where
  intoBuilder p =
    charUtf8 '\x1b'
      <> "]"
      <> "1337"
      <> ";"
      <> "File=["
      <> intoBuilder p.payloadArgs
      <> "]"
      <> ":"
      <> lazyByteString (encode p.payloadData)
      <> charUtf8 '\x07'
