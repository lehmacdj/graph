module System.IO.Term.BinarySerialize where

import MyPrelude

class BinarySerialize a where
  intoBuilder :: a -> ByteStringBuilder
  intoBytes :: a -> LByteString
  intoBytes = builderToLazy . intoBuilder
  {-# MINIMAL intoBuilder #-}
