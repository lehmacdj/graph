{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effect.Load where

import MyPrelude

data Load m r where
  SetLoaded :: String -> Load m ()

makeSem ''Load
