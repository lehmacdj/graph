module Error.UserError
  ( module Error.UserError,
    RecordSide (..),
    left,
    (#),
  )
where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception qualified as E
import Control.Lens
import Models.Damage
import Models.NID (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import System.Console.Haskeline qualified as Haskeline
import System.FileCoordination (NSErrorException (..))
import Utils.Testing.External

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
  | -- | The file of the node is there but could not be read
    UnreadableNodeFile NID IOError
  | FileCoordinationError NSErrorException
  | -- | A change was not written because the files of the nodes it changes
    -- contradict what it was computed from
    InconsistentNodeRecords (NonEmpty RecordMismatch)
  | -- | A change was not written because the files of the nodes that it read
    -- or changes are damaged
    DamagedGraph (NonEmpty Damage)
  | -- | Only ever a warning: a command that changes nothing read damaged
    -- files and went on
    DamagedGraphWarning (NonEmpty Damage)
  | -- | @at@ ran its command nowhere because targets of its path have no
    -- node file: each such target with the edges that led to it, as the
    -- damage that they are (none for a target that no edge led to)
    TargetsWithoutFile (NonEmpty (NID, [Damage]))
  | -- | fsck found that many pieces of damage (and listed them)
    DamageFound Int
  | -- | fix left that many pieces of damage (and listed them)
    DamageNotRepaired Int
  | -- | The graph's origin and system nodes could not be made ready, which
    -- every command but fsck and fix needs; with the reason
    CannotOpenGraph UserError
  | -- | A change was not written because it can't change all of its node
    -- files: each file to replace that can't be written, with the reason,
    -- and, if files are to be created or deleted, the reason why the graph's
    -- directory can't be written, with those files
    UnwritableChange
      [(NID, IOError)]
      (Maybe (IOError, NonEmpty (FileChange, NID)))
  | -- | Writing a change failed for some of its node files, after the checks:
    -- the files that were written, and the ones that were not with why
    ChangeWrittenInPart [(FileChange, NID)] (NonEmpty (FileChange, NID, UserError))
  | -- | A change was not written because these node files are no longer as
    -- they were when it read them
    ChangedMeanwhile (NonEmpty NID)
  | -- | The data of these nodes could not be written ('ReplacingFile') or
    -- removed ('DeletingFile') after the rest of the change was written;
    -- with why
    DataNotWritten (NonEmpty (FileChange, NID, UserError))
  deriving stock (Generic)

-- | What a change does to a file of the graph
data FileChange = ReplacingFile | CreatingFile | DeletingFile
  deriving stock (Eq, Ord, Show, Generic)

-- | The verb for doing that to a file, as 'fileFailure' takes it
fileChangeVerb :: FileChange -> String
fileChangeVerb = \case
  ReplacingFile -> "write"
  CreatingFile -> "create"
  DeletingFile -> "delete"

-- | The past participle of 'fileChangeVerb'
fileChangeDone :: FileChange -> String
fileChangeDone = \case
  ReplacingFile -> "written"
  CreatingFile -> "created"
  DeletingFile -> "deleted"

-- | Why something could not be done to a file, from the error that doing it
-- gave: in words, with what the system said ('fileFailure'), or as the error
-- says it.
describeFileFailure :: String -> UserError -> String
describeFileFailure doing = \case
  IOFail e -> fileFailure doing e
  e -> show e

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
    "the graph is damaged (unparsable-node-file): "
      ++ describeDamage (UnparsableNodeFile nid e)
      ++ "\n"
      ++ repairHint
  show (UnreadableNodeFile nid e) =
    nodeFileName nid
      ++ " is there but cannot be read (unreadable-node-file): "
      ++ show e
  show (FileCoordinationError e) =
    "the system did not give access to a file of the graph"
      ++ " (file-coordination-error): "
      ++ unpack e.description
      ++ " ("
      ++ unpack e.domain
      ++ " "
      ++ show e.code
      ++ ")"
  show (InconsistentNodeRecords mismatches) =
    intercalate "\n" $
      [ "this change was not written: the graph is damaged where it applies"
          ++ " (inconsistent-node-records)"
      ]
        ++ map (("- " ++) . describeRecordMismatch) (toList mismatches)
  show (DamagedGraph damage) =
    intercalate "\n" $
      [ "this change was not written: the graph is damaged where it applies"
          ++ " (damaged-graph)"
      ]
        ++ map (("- " ++) . describeDamage) (toList damage)
        ++ [repairHint]
  show (DamageFound count) =
    "the graph is damaged (damage-found): fsck found "
      ++ show count
      ++ (if count == 1 then " problem" else " problems")
      ++ ", listed above; fix repairs what it can and says what it can't"
  show (DamageNotRepaired count) =
    "the graph is still damaged (damage-not-repaired): fix did not repair "
      ++ show count
      ++ (if count == 1 then " problem" else " problems")
      ++ ", listed above with the reasons"
  show (CannotOpenGraph reason) =
    ( case reason of
        -- the change that was refused is the one that opening the graph
        -- makes, which whoever ran a command that only reads didn't ask for
        DamagedGraph damage ->
          intercalate "\n" $
            [ "a system node or an edge to one is missing and was not added:"
                ++ " the graph is damaged where it goes (damaged-graph)"
            ]
              ++ map (("- " ++) . describeDamage) (toList damage)
              ++ [repairHint]
        _ -> show reason
    )
      ++ "\nthe graph could not be opened (cannot-open-graph): only fsck and"
      ++ " fix can run until this is repaired"
  show (DamagedGraphWarning damage) =
    intercalate "\n" $
      ["warning: the graph is damaged where this command read it (damaged-graph)"]
        ++ map (("- " ++) . describeDamage) (toList damage)
        ++ [repairHint]
  show (TargetsWithoutFile targets) =
    intercalate "\n" $
      [ "at did nothing: "
          ++ (if length targets == 1 then "a target has" else "targets have")
          ++ " no node file (damaged-graph)"
      ]
        ++ map ("- " ++) (concatMap describeTarget (toList targets))
        ++ [repairHint]
    where
      describeTarget = \case
        (target, []) ->
          [ "there is no "
              ++ nodeFileName target
              ++ ", the file of the target "
              ++ show target
          ]
        (_, damage) -> map describeDamage damage
  show (UnwritableChange files directory) =
    intercalate "\n" $
      [ "this change was not written: files of it can't be written"
          ++ " (unwritable-graph-file)"
      ]
        ++ [ "- " ++ nodeFileName nid ++ " can't be written: " ++ fileFailure "write" e
           | (nid, e) <- files
           ]
        ++ [ "- the graph's directory can't be written, so "
               ++ intercalate
                 " and "
                 [ intercalate ", " names ++ " can't be " ++ fileChangeDone change
                 | change <- [CreatingFile, DeletingFile],
                   let names = [nodeFileName nid | (change', nid) <- toList changes, change' == change],
                   not (null names)
                 ]
               ++ ": "
               ++ fileFailure "write" e
           | (e, changes) <- toList directory
           ]
  show (ChangeWrittenInPart written failed) =
    intercalate "\n" $
      [ if null written
          then "this change was not written: writing its files failed (change-not-written)"
          else
            "this change was written only in part: some of its files could"
              ++ " not be written, so the graph may be damaged where it applies"
              ++ " (change-written-in-part)"
      ]
        ++ [ "- "
               ++ nodeFileName nid
               ++ " could not be "
               ++ fileChangeDone change
               ++ ": "
               ++ describeFileFailure (fileChangeVerb change) e
           | (change, nid, e) <- toList failed
           ]
        ++ [ "- written as the change has them: "
               ++ intercalate ", " (map writtenFile written)
           | not (null written)
           ]
        ++ [repairHint | not (null written)]
    where
      writtenFile (change, nid) =
        nodeFileName nid ++ case change of
          ReplacingFile -> ""
          CreatingFile -> " (created)"
          DeletingFile -> " (deleted)"
  show (ChangedMeanwhile nids) =
    "this change was not written: "
      ++ intercalate " and " (map nodeFileName (toList nids))
      ++ (if length nids == 1 then " is" else " are")
      ++ " no longer as the change read "
      ++ (if length nids == 1 then "it" else "them")
      ++ ": something else, another program or a sync, changed "
      ++ (if length nids == 1 then "it" else "them")
      ++ " meanwhile (changed-meanwhile). Nothing of the change was written;"
      ++ " run the command again"
  show (DataNotWritten failed) =
    intercalate "\n" $
      [ "the data of "
          ++ (if length failed == 1 then "a node" else "some nodes")
          ++ " could not be changed, though the rest of the change was written"
          ++ " (data-not-written)"
      ]
        ++ [ "- the data of "
               ++ show nid
               ++ " could not be "
               ++ (if change == DeletingFile then "removed" else "written")
               ++ ": "
               ++ describeFileFailure
                 (if change == DeletingFile then "remove" else "write")
                 e
           | (change, nid, e) <- toList failed
           ]

-- | The damage that an error says a command was refused for. A command line
-- doesn't warn of that damage as well ('Error.Warn.warningOfDamageReadPast').
damageRefusedBy :: UserError -> Set Damage
damageRefusedBy = \case
  DamagedGraph damage -> setFromList (toList damage)
  TargetsWithoutFile targets -> setFromList (concatMap snd (toList targets))
  CannotOpenGraph reason -> damageRefusedBy reason
  -- as when a command of a @;@ line is refused and the error says what the
  -- commands before it did
  Multiple errors -> foldMap damageRefusedBy errors
  _ -> mempty

-- | The damage that an error from reading a node file stands for, if it says
-- that the file is there and can't be used as a node.
damageOfReadError :: UserError -> Maybe Damage
damageOfReadError = \case
  FailedToDeserializeNode nid reason -> Just (UnparsableNodeFile nid reason)
  UnreadableNodeFile nid e -> Just (UnparsableNodeFile nid (readFailure e))
  _ -> Nothing

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
--
-- Only what the action itself threw. An interrupt, the end of a 'timeout'
-- or the killing of the thread that arrives while the action runs passes
-- through as it is ('fromExceptionVia'): a command that is stopped while it
-- reads a file is stopped like any other, and is not reported as a command
-- that failed.
embedCatchingErrors ::
  (Members [Error UserError, IOE] r) => IO a -> Eff r a
embedCatchingErrors = fromExceptionVia mapExceptionToUserError

-- | The user error that an exception stands for
mapExceptionToUserError :: SomeException -> UserError
mapExceptionToUserError = \case
  (E.fromException @IOError -> Just e) -> IOFail e
  (E.fromException @HttpException -> Just e) -> WebError e
  (E.fromException @NSErrorException -> Just e) -> FileCoordinationError e
  e -> OtherException e

-- | Like 'mapExceptionToUserError', for an exception that may be a user
-- error thrown as one (as an unlifted action throws it), which stays as it is
exceptionAsUserError :: SomeException -> UserError
exceptionAsUserError = \case
  (E.fromException @UserError -> Just e) -> e
  e -> mapExceptionToUserError e

spec_embedCatchingErrors :: Spec
spec_embedCatchingErrors = do
  let embedded :: IO () -> IO (Either UserError ())
      embedded = runEff . runErrorNoCallStack @UserError . embedCatchingErrors
      -- as Ctrl-C, a signal or a time-out arrive: thrown at the thread
      -- while it runs the action
      arriving :: (Exception e) => e -> IO ()
      arriving e = myThreadId >>= (`E.throwTo` e)
  it "makes a user error of an exception of the action" do
    Left (IOFail e) <- embedded (throwIO (userError "disk on fire"))
    e `shouldBe` userError "disk on fire"
  it "makes a user error of an error call" do
    Left (OtherException e) <- embedded (error "broken invariant")
    displayException e `shouldContain` "broken invariant"
  it "passes on the Ctrl-C of the REPL" $
    embedded (arriving Haskeline.Interrupt)
      `shouldThrow` \Haskeline.Interrupt -> True
  it "passes on an interrupt signal" $
    embedded (arriving E.UserInterrupt) `shouldThrow` (== E.UserInterrupt)
  it "passes on the killing of the thread" $
    embedded (arriving E.ThreadKilled) `shouldThrow` (== E.ThreadKilled)
  it "is ended by a time-out" do
    -- the action would take a minute
    result <- timeout 10000 (embedded (threadDelay 60000000))
    isNothing result `shouldBe` True

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
