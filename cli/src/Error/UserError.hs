module Error.UserError
  ( module Error.UserError,
    module Polysemy.Error,
    left,
    (#),
  )
where

import Control.Arrow (left)
import Control.Exception qualified as E
import Control.Lens
import Models.NID (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import Polysemy.Error hiding (fromException, try)
import System.MacOS.NSFileCoordinator (NSErrorException)

-- | Blanket error type used by the app. This is called @UserError@ because
-- generally we only want to intercept it when propagating errors to the user.
data UserError
  = OtherError Text
  | OtherException SomeException
  | Multiple (NonEmpty UserError)
  | -- | Thrown when aborting an action (e.g. via a y/n prompt)
    OperationCancelled
  | IOFail IOError
  | -- | A @Missing@ error that was converted into an UserError.
    -- Generally we keep Missing separate because it is more legitimate for
    -- various parts of the application to want to intercept them and handle
    -- then in different ways, while generally UserErrors need to be propogated
    -- all the way up to the user.
    MissingNode NID (Maybe Text)
  | WebError HttpException
  | FailedToDeserializeNode NID String
  | FileCoordinationError NSErrorException
  deriving (Generic)

instance Show UserError where
  show (OtherError s) = unpack s
  show (OtherException e) = show e
  show (Multiple errs) = unlines $ map show errs
  show OperationCancelled = "cancelled!"
  show (IOFail e) = show e
  show (MissingNode nid reason) =
    "node "
      ++ show nid
      ++ " is missing"
      ++ maybe "" ((" for reason " ++) . unpack) reason
  show (WebError e) = show e
  show (FailedToDeserializeNode nid e) =
    "failed to deserialize JSON for " ++ show nid ++ ": " ++ e
  show (FileCoordinationError e) = "error while file coordinating: " ++ show e

-- | order preserving union of the errors in the different error types
-- for use with Validation
instance Semigroup UserError where
  Multiple e1s <> Multiple e2s = Multiple (e1s <> e2s)
  e1 <> Multiple e2s = Multiple (singleton e1 <> e2s)
  Multiple e1s <> e2 = Multiple (e1s <> singleton e2)
  e1 <> e2 = Multiple (singleton e1 <> singleton e2)

instance Exception UserError

throwString :: (Member (Error UserError) effs) => String -> Sem effs a
throwString = throwConvertible

throwText :: (Member (Error UserError) effs) => Text -> Sem effs a
throwText = throwConvertible

throwConvertible ::
  (ToUserError e, Member (Error UserError) effs) =>
  e ->
  Sem effs a
throwConvertible = throw . toUserError

throwLeft ::
  (Member (Error UserError) effs, ToUserError err) =>
  Either err a ->
  Sem effs a
throwLeft (Right x) = pure x
throwLeft (Left err) = throw $ toUserError err

-- | capture errors and convert them to an error
embedCatchingErrors ::
  (Members [Error UserError, Embed IO] r) => IO a -> Sem r a
embedCatchingErrors = fromExceptionVia mapExceptionToUserError
  where
    mapExceptionToUserError = \case
      (E.fromException @IOError -> Just e) -> IOFail e
      (E.fromException @HttpException -> Just e) -> WebError e
      (E.fromException @NSErrorException -> Just e) -> FileCoordinationError e
      e -> OtherException e

printErrors ::
  (Member (Embed IO) effs) =>
  Sem (Error UserError ': effs) () ->
  Sem effs ()
printErrors = (`handleError` liftIO . eprint)

class ToUserError e where
  toUserError :: e -> UserError

subsumeUserError ::
  forall e r a.
  (Member (Error UserError) r, ToUserError e) =>
  Sem (Error e : r) a ->
  Sem r a
subsumeUserError = (`handleError` (throw . toUserError))

instance ToUserError IOException where
  toUserError = IOFail

instance ToUserError UserError where
  toUserError = id

instance ToUserError SomeException where
  toUserError = OtherException

instance ToUserError HttpException where
  toUserError = WebError

instance ToUserError Text where
  toUserError = OtherError

instance ToUserError String where
  toUserError = OtherError . pack
