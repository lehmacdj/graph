{-# LANGUAGE UndecidableInstances #-}

module MyPrelude.JSON where

import ClassyPrelude
import Data.Aeson
import GHC.Generics

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
