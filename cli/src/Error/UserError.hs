module Error.UserError
  ( module Error.UserError,
    left,
    (#),
  )
where

import Control.Arrow (left)
import Control.Lens
import Models.NID (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import System.MacOS.NSFileCoordinator (NSErrorException)

-- | Blanket error type used by the app. This is called @UserError@ because
-- generally we only want to intercept it when propagating errors to the user.
data UserError
  = OtherError String
  | -- | Thrown when aborting an action (e.g. via a y/n prompt)
    OperationCancelled
  | OtherException SomeException
  | IOFail IOError
  | -- | A @Missing@ error that was converted into an UserError.
    -- Generally we keep Missing separate because it is more legitimate for
    -- various parts of the application to want to intercept them and handle
    -- then in different ways, while generally UserErrors need to be propogated
    -- all the way up to the user.
    MissingNode NID (Maybe Text)
  | -- | report that the thing that has a given
    -- representation wasn't a singleton
    NotSingleton String
  | WebError HttpException
  | -- | deserialization fails
    AesonDeserialize String
  | FileCoordinationError NSErrorException
  | Multiple (NonEmpty UserError)
  deriving (Generic)

instance Show UserError where
  show (OtherError s) = s
  show (OtherException e) = show e
  show OperationCancelled = "cancelled!"
  show (IOFail e) = show e
  show (MissingNode nid reason) =
    "node " ++ show nid ++ " is missing"
    ++ maybe "" ((" for reason " ++) . unpack) reason
  show (NotSingleton xs) = xs ++ " was expected to be a singleton but wasn't"
  show (WebError e) = show e
  show (AesonDeserialize e) = "failed to deserialize JSON: " ++ e
  show (FileCoordinationError e) = "error while file coordinating: " ++ show e
  show (Multiple errs) = unlines $ map show errs

-- | order preserving union of the errors in the different error types
-- for use with Validation
instance Semigroup UserError where
  Multiple e1s <> Multiple e2s = Multiple (e1s <> e2s)
  e1 <> Multiple e2s = Multiple (singleton e1 <> e2s)
  Multiple e1s <> e2 = Multiple (e1s <> singleton e2)
  e1 <> e2 = Multiple (singleton e1 <> singleton e2)
