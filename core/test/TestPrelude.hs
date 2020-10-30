module TestPrelude
  ( module MyPrelude,
    module Test.Tasty,
    module Test.Tasty.HUnit,
  )
where

import MyPrelude hiding (assert)
import Test.Tasty
import Test.Tasty.HUnit
