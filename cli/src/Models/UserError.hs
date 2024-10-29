{-# LANGUAGE ViewPatterns #-}

module Models.UserError
  ( module Models.UserError,
    left,
    (#),
  )
where

import Control.Arrow (left)
import Control.Lens
import Models.Graph (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)

data UserError
  = OtherError String
  | -- | Thrown when aborting an action (e.g. via a y/n prompt)
    OperationCancelled
  | OtherException SomeException
  | IOFail IOError
  | MissingNode NID
  | -- | report that the thing that has a given
    -- representation wasn't a singleton
    NotSingleton String
  | WebError HttpException
  | -- | deserialization fails
    AesonDeserialize String
  | Multiple (NonEmpty UserError)
  deriving (Generic)

instance Show UserError where
  show (OtherError s) = s
  show (OtherException e) = show e
  show OperationCancelled = "cancelled!"
  show (IOFail e) = show e
  show (MissingNode nid) = "node " ++ show nid ++ " is missing"
  show (NotSingleton xs) = xs ++ " was expected to be a singleton but wasn't"
  show (WebError e) = show e
  show (AesonDeserialize e) = "failed to deserialize JSON: " ++ e
  show (Multiple errs) = unlines $ map show errs

-- | order preserving union of the errors in the different error types
-- for use with Validation
instance Semigroup UserError where
  Multiple e1s <> Multiple e2s = Multiple (e1s <> e2s)
  e1 <> Multiple e2s = Multiple (singleton e1 <> e2s)
  Multiple e1s <> e2 = Multiple (e1s <> singleton e2)
  e1 <> e2 = Multiple (singleton e1 <> singleton e2)
