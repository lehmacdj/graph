{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Load where

import ClassyPrelude
import Control.Monad.Freer.TH

data Load r where
  SetLoaded :: String -> Load ()

makeEffect ''Load
