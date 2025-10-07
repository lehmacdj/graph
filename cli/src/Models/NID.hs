module Models.NID
  ( NID,
    nilNID,
    smallNID,
    unsafeNID,
    nidDigits,
    nidRepresentation,
    parseNID,
    spec_parseNID,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as A
import GHC.Generics
import Models.Common
import MyPrelude
import System.Random
import System.Random.Stateful (Uniform (..), uniformRM)
import Test.Hspec (Spec, it, shouldBe, shouldSatisfy)
import Text.Read
import Utils.Base62 (base62Chars, isBase62Char)

-- | The number of letter digits in a NID
-- * My notes use 10 which is probably as long as there are <1 million nodes
--   generated
-- * YouTube uses 12 which is probably enough if I use a DB that can detect
--   collisions and fail non-silently
-- * 32 is enough for all intents and purposes including distributed universal
--   paperclip producing AI nanofactories
nidDigits :: Int
nidDigits = 12

-- | NIDs base 62 strings of some length. Currently this number is at 10, but
-- I'm somewhat stupidly making the implementation generic in case I want to
-- reduce collision chances in the future (though that would most likely require
-- renumbering all NIDs which could get expensive in a sufficiently large graph)
--
-- We avoid using + and / because they are special characters in URLs and _ and
-- - because they mess up double click to select word in some browsers/terminals
-- thus we get 62 characters to work with
--
-- We represent these IDs using a string. This makes comparing them potentially
-- quite slow because we use them as keys in `Map`. Perhaps worth revisiting
-- or using a HashMap instead.
newtype NID = UnsafeNID {representation :: Text}
  deriving (Eq, Ord, Generic, Lift)
  deriving newtype (NFData, Hashable)

instance Uniform NID where
  uniformM g =
    UnsafeNID
      . pack
      <$> replicateM nidDigits ((base62Chars `indexEx`) <$> uniformRM (0, 61) g)

-- | zero NID which represents the origin
nilNID :: NID
nilNID = UnsafeNID (replicate nidDigits '0')

nidRepresentation :: NID -> Text
nidRepresentation (UnsafeNID t) = t

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
          base62Chars `indexEx` (n `rem` 62)
            : go (digitsLeft - 1) (n `quot` 62)

instance CompactNodeShow NID where
  type Augmentation NID = Void
  minimumNidLength _ =
    max 1 . (nidDigits -) . length . takeWhile (== '0') . (.representation)
  nshowSettings CompactNodeShowSettings {..} nid =
    (if showNidAtSign then "@" else "")
      ++ drop (max (nidDigits - nidLength) 0) nid.representation

-- | Attoparsec parser for NID
parseNID :: Parser NID
parseNID = do
  nidStr <- A.count nidDigits (A.satisfy isBase62Char)
  pure $ UnsafeNID (pack nidStr)

spec_parseNID :: Spec
spec_parseNID = do
  let testParseIsUnsafeNID s =
        it ("parses " <> show s) $
          A.parseOnly (parseNID <* A.endOfInput) s
            `shouldBe` Right (UnsafeNID s)
  testParseIsUnsafeNID "0123456789ab"
  testParseIsUnsafeNID "000000000000"
  let testParseFails s =
        it ("fails to parse " <> show s) $
          A.parseOnly (parseNID <* A.endOfInput) s
            `shouldSatisfy` isLeft
  testParseFails "" -- too short
  testParseFails "0123456789a" -- too short
  testParseFails "0123456789abc" -- too long
  testParseFails "0123456789a!" -- invalid char
  testParseFails "01234-6789ab" -- invalid char
  testParseFails "01234_6789ab" -- invalid char
  testParseFails "01234/6789ab" -- invalid char
  testParseFails "01234+6789ab" -- invalid char
  testParseFails "01234 6789ab" -- invalid char

instance Show NID where
  show = unpack . nshow

instance Read NID where
  readsPrec _ x =
    case A.parseOnly (parseNID <* A.endOfInput) (pack x) of
      Right nid -> [(nid, "")]
      Left _ -> []

instance ToJSON NID where
  toEncoding = toEncoding . (.representation)

instance FromJSON NID where
  parseJSON = withText "NID" $ \t ->
    case A.parseOnly (parseNID <* A.endOfInput) t of
      Left err -> fail $ "failed to parse NID: " <> err
      Right nid -> pure nid

instance ToJSONKey NID where
  toJSONKey = toJSONKeyText (.representation)

instance FromJSONKey NID where
  fromJSONKey = FromJSONKeyTextParser p
    where
      p t = case A.parseOnly (parseNID <* A.endOfInput) t of
        Left err -> fail $ "failed to parse NID: " <> err
        Right nid -> pure nid

-- | Unsafe method for constructing an NID from a string.
-- While checked (crashes if not nidDigits long & uses characters outside of
-- A-Z a-z 0-9) this should still be avoided except for 'SystemNodes' because
-- it violates random generation of ids (which is important for avoiding
-- collisions)
unsafeNID :: (HasCallStack) => Text -> NID
unsafeNID t =
  fromMaybe (error $ show t <> " doesn't meet precondition") $
    readMay t
