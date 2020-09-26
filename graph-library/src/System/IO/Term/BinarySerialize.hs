module System.IO.Term.BinarySerialize where

import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy (ByteString)

class BinarySerialize a where
  intoBuilder :: a -> Builder
  intoBytes :: a -> ByteString
  intoBytes = toLazyByteString . intoBuilder
  {-# MINIMAL intoBuilder #-}
