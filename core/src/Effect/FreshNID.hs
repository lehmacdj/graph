{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Effect for generating unique identifiers. To be implemented by random
-- generation of UUIDs or sequencial indicies for example.
module Effect.FreshNID where

import Graph (NID)
import MyPrelude

data FreshNID m r where
  FreshNID :: FreshNID m NID

makeSem ''FreshNID
