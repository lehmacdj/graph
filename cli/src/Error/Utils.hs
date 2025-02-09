module Error.Utils
  ( module Error.Utils,
    module Error.UserError,
    module Polysemy.Error,
  )
where

import Models.NID
import Error.UserError
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import Polysemy.Error hiding (fromException, try)
import System.MacOS.NSFileCoordinator (NSErrorException)

throwString :: Member (Error UserError) effs => String -> Sem effs a
throwString = throw . OtherError

throwLeft ::
  (Member (Error UserError) effs, ToUserError err) =>
  Either err a ->
  Sem effs a
throwLeft (Right x) = pure x
throwLeft (Left err) = throw $ toUserError err

-- | capture errors and convert them to an error
embedCatchingErrors ::
  Members [Error UserError, Embed IO] r => IO a -> Sem r a
embedCatchingErrors = fromExceptionVia mapExceptionToUserError
  where
    mapExceptionToUserError = \case
      (fromException @IOError -> Just e) -> IOFail e
      (fromException @HttpException -> Just e) -> WebError e
      (fromException @NSErrorException -> Just e) -> FileCoordinationError e
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

data Missing = Missing
  { nid :: NID,
    reason :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance ToUserError Missing where
  toUserError (Missing nid reason) = MissingNode nid reason

instance ToUserError IOException where
  toUserError = IOFail

instance ToUserError UserError where
  toUserError = id

instance ToUserError SomeException where
  toUserError = OtherException

instance ToUserError HttpException where
  toUserError = WebError

instance ToUserError String where
  toUserError = OtherError

throwMissing :: Member (Error Missing) effs => NID -> Sem effs a
throwMissing nid = throw $ Missing nid Nothing

class TextRepresentableError a where
  textRepresentation :: a -> Text
  default textRepresentation :: Exception a => a -> Text
  textRepresentation = pack . displayException

instance TextRepresentableError String where
  textRepresentation = pack

instance TextRepresentableError Text where
  textRepresentation = id

instance TextRepresentableError SomeException

instance TextRepresentableError IOError

throwMissingIfLeft ::
  (Member (Error Missing) effs, TextRepresentableError err) =>
  NID -> Either err a -> Sem effs a
throwMissingIfLeft _ (Right x) = pure x
throwMissingIfLeft nid (Left e) =
  throw $ Missing nid $ Just $ textRepresentation e

throwMissingIfNothing ::
  (Member (Error Missing) effs) =>
  NID -> Maybe a -> Sem effs a
throwMissingIfNothing _ (Just r) = pure r
throwMissingIfNothing nid Nothing = throwMissing nid
