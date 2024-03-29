{-# LANGUAGE ViewPatterns #-}

module UserError
  ( module UserError,
    left,
    (#),
    module Polysemy.Error,
  )
where

import Control.Arrow (left)
import Control.Lens
import Graph (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import Polysemy.Error hiding (fromException, try)

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

throwString :: Member (Error UserError) effs => String -> Sem effs a
throwString = throw . OtherError

throwIfNothing ::
  Member (Error UserError) effs =>
  UserError ->
  Maybe a ->
  Sem effs a
throwIfNothing _ (Just x) = pure x
throwIfNothing err Nothing = throw err
{-# DEPRECATED throwIfNothing "use note from Polysemy.Error instead" #-}

throwLeft ::
  Member (Error UserError) effs =>
  Either UserError a ->
  Sem effs a
throwLeft (Right x) = pure x
throwLeft (Left err) = throw err

-- | capture errors and convert them to an error
fromExceptionToUserError ::
  Members [Error UserError, Embed IO] r => IO a -> Sem r a
fromExceptionToUserError = fromExceptionVia mapExceptionToUserError
  where
    mapExceptionToUserError = \case
      (fromException @IOError -> Just e) -> IOFail e
      (fromException @HttpException -> Just e) -> WebError e
      e -> OtherException e

printErrors ::
  Member (Embed IO) effs =>
  Sem (Error UserError ': effs) () ->
  Sem effs ()
printErrors = (`handleError` liftIO . eprint)

errorToLeft ::
  Show e => Sem (Error e : effs) a -> Sem effs (Either String a)
errorToLeft = (`handleError` pure . Left . show) . fmap Right

errorToNothing ::
  Sem (Error e : effs) a -> Sem effs (Maybe a)
errorToNothing = (`handleError` const (pure Nothing)) . fmap Just

-- * More granular errors for refining kinds of exceptions that stuff can throw

class ToUserError e where
  toUserError :: e -> UserError

subsumeUserError ::
  forall e r a.
  (Member (Error UserError) r, ToUserError e) =>
  Sem (Error e : r) a ->
  Sem r a
subsumeUserError = (`handleError` (throw . toUserError))

newtype Missing = Missing {unMissing :: NID}
  deriving (Show, Eq, Ord)

instance ToUserError Missing where
  toUserError (Missing nid) = MissingNode nid

throwMissing :: Member (Error Missing) effs => NID -> Sem effs a
throwMissing = throw . Missing
