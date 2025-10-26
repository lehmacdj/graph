{-# LANGUAGE UndecidableInstances #-}

module MyPrelude.JSON
  ( module X,
    FastGenericEncoding (..),
    AesonValue,
    encodeJSON,
    eitherDecodeLazy,
  )
where

import ClassyPrelude
import Data.Aeson
import Data.Aeson as X (FromJSON (..), ToJSON (..), eitherDecodeStrict)
import GHC.Generics

type AesonValue = Value

newtype FastGenericEncoding a = GenericEncodingAeson
  {underlying :: a}
  deriving (Generic)

instance
  (Generic a, GToJSON' Encoding Zero (Rep a), GToJSON' Value Zero (Rep a)) =>
  ToJSON (FastGenericEncoding a)
  where
  toJSON = genericToJSON defaultOptions . (.underlying)
  toEncoding = genericToEncoding defaultOptions . (.underlying)

instance
  (Generic a, GFromJSON Zero (Rep a)) =>
  FromJSON (FastGenericEncoding a)
  where
  parseJSON = fmap GenericEncodingAeson . genericParseJSON defaultOptions

encodeJSON :: (ToJSON a) => a -> LByteString
encodeJSON = encode

eitherDecodeLazy :: (FromJSON a) => LByteString -> Either String a
eitherDecodeLazy = eitherDecode
