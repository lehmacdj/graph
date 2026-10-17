module Error.UserError
  ( module Error.UserError,
    RecordSide (..),
    left,
    (#),
  )
where

import Control.Lens
import Error.FileCoordination (NSErrorException (..))
import Models.Damage
import Models.NID (NID)
import MyPrelude

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
  | -- | An HTTP request failed: the exception of the HTTP library (see
    -- 'Error.Catching.mapExceptionToUserError'), kept as any exception so
    -- that this type doesn't depend on that library
    WebError SomeException
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
  | -- | @:i@ imported nothing: the directory could not be read; with why
    UnreadableDirectory FilePath IOError
  | -- | Only ever a warning: @:i@ imported the directory (the first path)
    -- without these parts of it, which could not be read: each by its path
    -- in the directory, with why
    UnreadablePartsSkipped FilePath (NonEmpty (FilePath, IOError))
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
  -- \| The damage that an error says a command was refused for. A command line
  -- doesn't warn of that damage as well ('Error.Warn.warningOfDamageReadPast').
  show (UnreadableDirectory dir e) =
    "nothing was imported: "
      ++ dir
      ++ " could not be read (unreadable-directory): "
      ++ fileFailure "read" e
  show (UnreadablePartsSkipped dir skipped) =
    intercalate "\n" $
      [ "warning: "
          ++ (if length skipped == 1 then "a part" else "parts")
          ++ " of "
          ++ dir
          ++ " could not be read and "
          ++ (if length skipped == 1 then "was" else "were")
          ++ " not imported (unreadable-files)"
      ]
        ++ [ "- " ++ part ++ ": " ++ fileFailure "read" e
           | (part, e) <- toList skipped
           ]

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

instance ToUserError Text where
  toUserError = OtherError

instance ToUserError String where
  toUserError = OtherError . pack
