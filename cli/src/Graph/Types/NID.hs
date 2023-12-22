{-# LANGUAGE DeriveGeneric #-}

module Graph.Types.NID
  ( NID,
    nilNID,
  )
where

import Control.DeepSeq
import Control.Monad.Fail (fail)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import GHC.Generics
import MyPrelude
import System.Random
import System.Random.Stateful (Uniform (..), uniformRM)
import Text.Read

-- | The number of letter digits in a NID
-- * My notes use 10 which is probably sufficient unless I start generating nodes programmatically
-- * YouTube uses 12 which is probably enough if I use a DB that can detect collisions and fail non-silently
-- * 32 is enough for all intents and purposes including distributed universal paperclip building AI
nidDigits :: Int
nidDigits = 10 -- the same number as my wiki notes

-- | NIDs base 62 strings of some length. Currently this number is at 10, but
-- I'm somewhat stupidly making the implementation generic in case I want to
-- reduce collision chances in the future (though that would most likely require
-- renumbering all NIDs which could get expensive in a sufficiently large graph)
--
-- We avoid using + and / because they are special characters in URLs and _ and
-- - because they mess up double click to select word in some browsers/terminals
-- thus we get 62 characters to work with
--
-- We represent these IDs using a string because it's not worth deserializing
-- them to a more compact format. We verify that they are valid before
-- converting them to this newtype however.
newtype NID = NID {textRep :: Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (NFData)

instance Uniform NID where
  uniformM g = NID . pack <$> replicateM nidDigits (uniformRM ('0', 'z') g)

-- | zero NID which represents the origin
nilNID :: NID
nilNID = NID (replicate nidDigits '0')

instance Show NID where
  show = unpack . textRep

instance Read NID where
  readsPrec _ x
    | all isBase62Char x && length x == nidDigits = [(NID (pack x), "")]
    | otherwise = []
    where
      isBase62Char c = isDigit c || isAsciiUpper c || isAsciiLower c

instance ToJSON NID where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NID

instance ToJSONKey NID where
  toJSONKey = toJSONKeyText tshow

instance FromJSONKey NID where
  fromJSONKey = FromJSONKeyTextParser p
    where
      p t = case readMay (unpack t) of
        Nothing -> fail "failed to parse NID"
        Just x -> pure x
