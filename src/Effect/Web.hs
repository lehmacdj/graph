{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Web where

import ClassyPrelude

import Control.Monad.Freer.TH

-- import Network.HTTP.Conduit (HttpException, simpleHttp)
-- import Control.Exception (try)

data Web r where
  GetHttp :: String -> Web LByteString
makeEffect ''Web
