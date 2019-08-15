{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Console where

import ClassyPrelude

import Control.Monad.Freer.TH

data Console a where
  Echo :: String -> Console () -- ^ print a string followed by a newline
  DisplayImage :: LByteString -> Console () -- ^ print a LBS as an image
makeEffect ''Console
