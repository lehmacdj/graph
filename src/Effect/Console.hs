{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Console where

import ClassyPrelude

import Control.Monad.Freer.TH

data Console a where
  Echo :: String -> Console ()
  PrintImage :: LByteString -> Console ()
makeEffect ''Console
