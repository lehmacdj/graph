{-# LANGUAGE ViewPatterns #-}

module Effect.UserError
  ( module Effect.UserError,
    module Models.UserError,
    module Polysemy.Error,
  )
where

import Models.NID
import Models.UserError
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import Polysemy.Error hiding (fromException, try)

throwString :: Member (Error UserError) effs => String -> Sem effs a
throwString = throw . OtherError

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

instance ToUserError IOException where
  toUserError = IOFail

throwMissing :: Member (Error Missing) effs => NID -> Sem effs a
throwMissing = throw . Missing
