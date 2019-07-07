module System.IO.Term.BinarySerialize where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)

class BinarySerialize a where
  toBuilder :: a -> Builder
  toBytes :: a -> ByteString
  toBytes = toLazyByteString . toBuilder
  {-# MINIMAL toBuilder #-}
