{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module Graph.Types.NID
  ( NID,
    nilNID,
    smallNID,
    unsafeNID,
    nidDigits,
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
nidDigits = 32 -- there isn't a great reason to keep this small as long as we implement completion for partial NIDs

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
newtype NID = UnsafeNID {textRep :: Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (NFData)

instance Uniform NID where
  uniformM g = UnsafeNID . pack <$> replicateM nidDigits (uniformRM ('0', 'z') g)

-- | zero NID which represents the origin
nilNID :: NID
nilNID = UnsafeNID (replicate nidDigits '0')

-- | Note: this is for testing only, it's not guaranteed to be unique
-- the approach used for converting the Int into a NID is naive leads to the
-- same input for different inputs. This is safe for ints < 62^10
smallNID :: Int -> NID
smallNID = UnsafeNID . pack . reverse . go nidDigits . (`rem` (62 ^ nidDigits))
  where
    go :: Int -> Int -> String
    go digitsLeft n
      | digitsLeft == 0 = []
      | otherwise =
        base62Chars `indexEx` (n `rem` 62) :
        go (digitsLeft - 1) (n `quot` 62)
    base62Chars :: Vector Char
    base62Chars = ['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z']

instance Show NID where
  show = unpack . textRep

instance Read NID where
  readsPrec _ x
    | all isBase62Char x && length x == nidDigits = [(UnsafeNID (pack x), "")]
    | otherwise = []
    where
      isBase62Char c = isDigit c || isAsciiUpper c || isAsciiLower c

instance ToJSON NID where
  toEncoding = toEncoding . (id :: Text -> Text) . textRep

instance FromJSON NID where
  parseJSON = withText "NID" $ \t ->
    readMay (unpack t) & \case
      Nothing -> fail "failed to parse NID"
      Just x -> pure x

instance ToJSONKey NID where
  toJSONKey = toJSONKeyText tshow

instance FromJSONKey NID where
  fromJSONKey = FromJSONKeyTextParser p
    where
      p t = case readMay (unpack t) of
        Nothing -> fail "failed to parse NID"
        Just x -> pure x

-- | Unsafe method for constructing an NID from a string.
-- While checked (crashes if not nidDigits long & uses characters outside of
-- A-Z a-z 0-9) this should still be avoided except for 'SpecialNodes' because
-- it violates random generation of ids (which is important for avoiding
-- collisions)
unsafeNID :: HasCallStack => Text -> NID
unsafeNID = fromMaybe (error "doesn't meet precondition") . readMay
