{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Web where

import ClassyPrelude

import Control.Monad.Freer.TH

data Web r where
  GetHttp :: String -> Web LByteString
makeEffect ''Web
