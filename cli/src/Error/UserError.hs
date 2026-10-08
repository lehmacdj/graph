module Error.UserError
  ( module Error.UserError,
    left,
    (#),
  )
where

import Control.Exception qualified as E
import Control.Lens
import Models.NID (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import System.FileCoordination (NSErrorException)

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
  | -- | A change was not written because the files of the nodes it changes
    -- contradict what it was computed from
    InconsistentNodeRecords (NonEmpty RecordMismatch)
  deriving stock (Generic)

-- | Which of a node's two lists of edges
data RecordSide = IncomingRecord | OutgoingRecord
  deriving stock (Eq, Ord, Show, Generic)

-- | A difference between the file of a node with a change applied to it and
-- the node that the change was computed to give. In each the first 'NID' is
-- the node whose file it is; an edge is given as the side of that node it is
-- on, its label and its other node.
data RecordMismatch
  = -- | The file has an edge that the change was computed without
    RecordNotComputed NID RecordSide Text NID
  | -- | The change was computed with an edge that the file doesn't have
    RecordNotInFile NID RecordSide Text NID
  | -- | The file says it is the second node
    FileOfOtherNode NID NID
  | -- | The change deletes the node, but applying it to the file doesn't
    NodeNotDeleted NID
  | -- | The change has the node, but applying it to the file gives none
    NodeNotCreated NID
  deriving stock (Eq, Ord, Show, Generic)

describeRecordMismatch :: RecordMismatch -> String
describeRecordMismatch = \case
  RecordNotComputed nid side label other ->
    "the file of "
      ++ show nid
      ++ " has "
      ++ edge side label other
      ++ ", but the other records say there is no such edge"
  RecordNotInFile nid side label other ->
    "the file of "
      ++ show nid
      ++ " doesn't have "
      ++ edge side label other
      ++ ", but the other records say there is such an edge"
  FileOfOtherNode nid claimed ->
    "the file of " ++ show nid ++ " says it is the node " ++ show claimed
  NodeNotDeleted nid ->
    "the node " ++ show nid ++ " would be left over instead of being deleted"
  NodeNotCreated nid ->
    "the node " ++ show nid ++ " would not exist after the change"
  where
    edge side label other = case side of
      IncomingRecord ->
        "an incoming edge " ++ show label ++ " from " ++ show other
      OutgoingRecord ->
        "an outgoing edge " ++ show label ++ " to " ++ show other

instance Show UserError where
  show (OtherError s) = unpack s
  show (OtherException e) = show e
  show (Multiple errs) = intercalate "\n" $ map show errs
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
  show (InconsistentNodeRecords mismatches) =
    intercalate "\n" $
      [ "this change was not written: the graph is damaged where it applies"
          ++ " (inconsistent-node-records)"
      ]
        ++ map (("- " ++) . describeRecordMismatch) (toList mismatches)

-- | order preserving union of the errors in the different error types
-- for use with Validation
instance Semigroup UserError where
  Multiple e1s <> Multiple e2s = Multiple (e1s <> e2s)
  e1 <> Multiple e2s = Multiple (singleton e1 <> e2s)
  Multiple e1s <> e2 = Multiple (e1s <> singleton e2)
  e1 <> e2 = Multiple (singleton e1 <> singleton e2)

instance Exception UserError

throwString :: (Error UserError :> effs) => String -> Eff effs a
throwString = throwConvertible

throwText :: (Error UserError :> effs) => Text -> Eff effs a
throwText = throwConvertible

throwConvertible ::
  (ToUserError e, Error UserError :> effs) =>
  e ->
  Eff effs a
throwConvertible = throwError . toUserError

throwLeft ::
  (Error UserError :> effs, ToUserError err) =>
  Either err a ->
  Eff effs a
throwLeft (Right x) = pure x
throwLeft (Left err) = throwError $ toUserError err

-- | capture errors and convert them to an error
embedCatchingErrors ::
  (Members [Error UserError, IOE] r) => IO a -> Eff r a
embedCatchingErrors = fromExceptionVia mapExceptionToUserError
  where
    mapExceptionToUserError = \case
      (E.fromException @IOError -> Just e) -> IOFail e
      (E.fromException @HttpException -> Just e) -> WebError e
      (E.fromException @NSErrorException -> Just e) -> FileCoordinationError e
      e -> OtherException e

printErrors ::
  (IOE :> effs) =>
  Eff (Error UserError ': effs) () ->
  Eff effs ()
printErrors = (`handleError` liftIO . eprint)

class ToUserError e where
  toUserError :: e -> UserError

subsumeUserError ::
  forall e r a.
  (Error UserError :> r, ToUserError e) =>
  Eff (Error e : r) a ->
  Eff r a
subsumeUserError = (`handleError` (throwError . toUserError))

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
