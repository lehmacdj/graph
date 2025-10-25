module Models.MimeType where

import MyPrelude

data MimeType = MimeType
  { type' :: Text,
    subtype :: Text
  }
  deriving (Eq, Ord, Generic)

instance Show MimeType where
  show (MimeType t st) = unpack t ++ "/" ++ unpack st
