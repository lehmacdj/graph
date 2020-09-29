{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Graph.Types.NID
  ( NID (..),
    smallNID,
    nilNID,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Base64.URL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text.Lazy as T
import GHC.Generics
import MyPrelude
import System.Random
import Text.Read

-- | NIDs are composed of 3 Word64s. We don't use a standard format like UUID
-- because I find the chance of collision of those unacceptable for eventual
-- applications here. This makes it sufficiently small without extending the
-- length of the string too much further.
--
-- NIDs are encoded as 32 length base 64 (2^6) strings.
-- For maximum URL compatibility we use _ and - in the base 64 encoding instead
-- of the usual + and /, there is no == at the end either.
data NID = NID
  { upper :: !Word64,
    middle :: !Word64,
    lower :: !Word64
  }
  deriving (Eq, Ord, Generic, NFData)

instance Random NID where
  random g0 =
    let (u, g1) = random g0
        (m, g2) = random g1
        (l, g3) = random g2
     in (NID u m l, g3)
  randomR (NID u1 m1 l1, NID u2 m2 l2) g0 =
    let (u, g1) = randomR (u1, u2) g0
        (m, g2) = randomR (m1, m2) g1
        (l, g3) = randomR (l1, l2) g2
     in (NID u m l, g3)

-- | Creates an NID with all but lower bits 0, for use in testing
smallNID :: Word64 -> NID
smallNID l = NID 0 0 l

-- | zero NID which represents the origin
nilNID :: NID
nilNID = NID 0 0 0

-- | separates a Word64 into 8 Word8's, the generated list always has length 8;
-- bits are generated from LSB to MSB
explodeWord64 :: Word64 -> [Word8]
explodeWord64 = reverse . go []
  where
    go acc n
      | length acc == 8 = acc
      | otherwise =
        let (rest, rem') = n `divMod` ((2 :: Word64) ^ (8 :: Word64))
         in go (fromIntegral rem' : acc) rest

-- | creates a Word64 from 8 Word8's. Returns Nothing if there are not exactly
-- 8 Word8s.
-- Bits are interpreted from LSB to MSB
mkWord64 :: [Word8] -> Maybe Word64
mkWord64 ws@[_, _, _, _, _, _, _, _] =
  Just . sum $ zipWith shift (fmap fromIntegral ws) [0, 8 .. 56]
mkWord64 _ = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs < n = [xs]
  | otherwise =
    let (chunk, rest) = splitAt n xs
     in chunk : chunksOf n rest

instance Show NID where
  show (NID u m l) =
    T.unpack . encodeBase64Unpadded . BS.pack $
      explodeWord64 u <> explodeWord64 m <> explodeWord64 l

instance Read NID where
  readsPrec _ x = case decodeBase64Unpadded (BSC.pack x) of
    Left _ -> []
    Right parsed -> case mapM mkWord64 (chunksOf 8 (BS.unpack parsed)) of
      Just [u, m, l] -> [(NID u m l, "")]
      _ -> []

instance ToJSON NID where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NID

instance ToJSONKey NID where
  toJSONKey = ToJSONKeyText f g
    where
      f = pack . show
      g = text . pack . show

instance FromJSONKey NID where
  fromJSONKey = FromJSONKeyTextParser p
    where
      p t = case readMay (unpack t) of
        Nothing -> fail "failed to parse NID"
        Just x -> pure x
