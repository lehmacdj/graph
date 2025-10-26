module Models.MimeType where

import Data.Text qualified as T
import MyPrelude

data MimeType = MimeType
  { type' :: Text,
    subtype :: Text
  }
  deriving (Eq, Ord, Generic)

instance Show MimeType where
  show (MimeType t st) = unpack t ++ "/" ++ unpack st

-- | Parse a mimetype string into MimeType
parseMimeType :: Text -> Maybe MimeType
parseMimeType mt =
  case T.split (== '/') mt of
    [type', subtype] -> Just MimeType {..}
    _ -> Nothing
