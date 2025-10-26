module Models.FileTypeInfo where

import Models.MimeType
import MyPrelude

data FileTypeInfo = FileTypeInfo
  { mimeType :: MimeType,
    extension :: Text
  }
  deriving (Eq, Show, Ord, Generic)
