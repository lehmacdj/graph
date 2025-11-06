-- | External testing utilities that only depend on ClassyPrelude and testing libraries.
-- This module can be imported by MyPrelude modules without creating cyclic dependencies.
module Utils.Testing.External
  ( module X,
  )
where

import Test.Hspec as X (Spec, describe, it)
import Test.Hspec.Expectations as X
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X hiding (Fixed (..), label)
