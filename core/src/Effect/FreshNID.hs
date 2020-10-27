{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Effect for generating unique identifiers. To be implemented by random
-- generation of UUIDs or sequencial indicies for example.
module Effect.FreshNID where

import Control.Monad.Freer.TH
import Graph (NID)
import MyPrelude

data FreshNID r where
  FreshNID :: FreshNID NID

makeEffect ''FreshNID
