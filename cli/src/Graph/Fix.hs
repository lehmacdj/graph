-- | Repairing the damage that 'Graph.Fsck' finds.
--
-- The rules, per kind of damage:
--
-- * An edge that only one of its two nodes records gets its missing record.
--   Nothing says whether the edge was being added or removed when the write
--   stopped; adding loses nothing (the edge can be removed again).
-- * A node file that is missing while other nodes record edges with the node
--   is rebuilt from those records. The edges that only it recorded (loops,
--   edges to other lost nodes) are not recoverable, nor is its data unless
--   the data file is still there.
-- * A node file whose content is not a node is rebuilt in the same way, once
--   a copy of it is kept under another name: the node is never without its
--   file, and if the copy can't be written the file is left as it is.
-- * Such a copy whose node has no file any more is left over from a node that
--   was deleted since (deleting it removes the copies, if it finds them and
--   can). It is removed. No node is ever made from a kept copy.
-- * A node file that is there but can't be read at all (for its permissions,
--   say) may be a good node file, and may hold any id and any record: no
--   repair is known to be right without it, so nothing at all is repaired
--   until it can be read.
-- * A file that says it is another node than it is named after gets its id
--   corrected, unless other nodes record its edges for the node it says it
--   is: then it may be a copy or a misnamed file, which only its owner can
--   tell, and neither it nor the edges of the two nodes are touched.
--
-- A file that is not a node file, such as the data of a node that has none,
-- is no part of the graph: nothing is done about it.
--
-- Nothing is ever deleted but such a left-over copy. Everything is computed
-- from one scan of the files as they are, listed, and only then written: if a
-- file that the repairs were worked out from has changed by then, or one
-- can't be written, nothing is written at all.
module Graph.Fix where

import DAL.FileSystemOperations.Metadata
import DAL.RawGraph
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Effect.IOWrapper.Echo
import Error.UserError
import Error.Warn
import GHC.IO.Exception (IOErrorType (InappropriateType), IOException (ioe_type))
import Graph.Fsck
import Models.Command (FixMode (..))
import Models.Damage
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude
import System.Directory
  ( doesPathExist,
    getPermissions,
    listDirectory,
    pathIsSymbolicLink,
    writable,
  )

data Repair
  = -- | Keep a copy of the file of the node, which can't be read as a node,
    -- under this name (before the file is replaced)
    SetAside NID FilePath
  | -- | Make the file of the first node say so instead of the second
    CorrectId NID NID
  | -- | Create the file of the node with the edges that other nodes record
    -- with it (so many)
    Rebuild NID Int
  | -- | Add the record of the edge on this side (to the node that lacks it)
    AddRecord RecordSide (Edge Text)
  | -- | Remove the files with these names, which a fix kept of the file of
    -- the node: the node was deleted since
    RemoveLeftOver NID [FilePath]
  deriving stock (Eq, Ord, Show, Generic)

data FixPlan = FixPlan
  { repairs :: [Repair],
    -- | the files to keep a copy of before they are replaced, with the names
    -- of the copies
    setAside :: Map NID FilePath,
    -- | the kept copies to remove, by the node whose file they were kept of
    removals :: Map NID [FilePath],
    -- | every node file to write, whole
    writes :: Map NID (Node Text ()),
    -- | the node files that the writes were worked out from, besides the ones
    -- that they replace: the plan is for the graph as it was when these were
    -- read
    derivedFrom :: Set NID,
    -- | the damage that will be left, with the reason
    notRepaired :: [(Damage, String)]
  }
  deriving stock (Eq, Show, Generic)

-- | The files whose id is in doubt, with the node each says it is: a file
-- that says it is another node than it is named after, while some edge it
-- records is recorded at its other end for that other node (or leads to
-- that other node itself).
filesInDoubt :: GraphFiles -> Map NID NID
filesInDoubt files =
  Map.mapMaybeWithKey
    (\name node -> justIfTrue (node.nid /= name && doubtful name node) node.nid)
    readable
  where
    readable = readableNodes files
    doubtful name node =
      any (recordedForClaimed node.nid) (recordedEdges name node)
    recordedForClaimed claimed (side, e) =
      let other = snd (recordingNode side e)
          renamed = case side of
            OutgoingRecord -> e {source = claimed}
            IncomingRecord -> e {sink = claimed}
       in other == claimed
            || maybe False (records (otherSide side) renamed) (lookup other readable)

-- | What 'fix' does to a graph: nothing while a node file can't be read.
planFix :: GraphFiles -> FixPlan
planFix files
  | null files.unreadable =
      FixPlan {repairs, setAside, removals, writes, derivedFrom, notRepaired}
  | otherwise =
      FixPlan
        { repairs = [],
          setAside = mempty,
          removals = mempty,
          writes = mempty,
          derivedFrom = mempty,
          notRepaired =
            [ ( damage,
                "nothing is repaired while "
                  <> intercalate ", " (map nodeFileName (toList files.unreadable))
                  <> " can't be read"
              )
            | damage <- toList (scanDamage files)
            ]
        }
  where
    readable = readableNodes files
    -- the files whose content is not a node
    unparsable = keysSet (Map.filter isLeft files.nodes)
    doubted = filesInDoubt files
    -- neither a file in doubt nor the node it says it is takes part in any
    -- repair
    untouchable = keysSet doubted <> setFromList (toList doubted)
    -- the readable nodes that repairs go by, each as the node its file is
    -- named after
    sound =
      Map.mapWithKey (\name node -> node & #nid .~ name) $
        readable `Map.withoutKeys` untouchable
    corrected =
      [ CorrectId name node.nid
      | (name, node) <- mapToList (readable `Map.withoutKeys` untouchable),
        node.nid /= name
      ]
    -- the records that are missing: the node to add each to, its side there
    -- and the edge
    additions = [(other, side, e) | (_, other, side, e) <- derivations]
    -- the same with the node whose file has the record that each one mirrors
    derivations =
      [ (name, other, otherSide side, e)
      | (name, node) <- mapToList sound,
        (side, e) <- recordedEdges name node,
        let other = snd (recordingNode side e),
        other `notMember` untouchable,
        maybe True (not . records (otherSide side) e) (lookup other sound)
      ]
    rebuilt =
      ( setFromList [other | (other, _, _) <- additions, other `notMember` sound]
          <> unparsable
          <> setFromList [nilNID | nilNID `notMember` files.nodes]
      )
        `difference` untouchable
    repaired =
      foldl'
        (\nodes (nid, side, e) -> Map.adjust (addRecord side e) nid nodes)
        (sound <> mapFromList [(nid, emptyNode nid) | nid <- toList (asSet rebuilt)])
        additions
    addRecord side e = case side of
      OutgoingRecord -> #outgoing %~ insertSet (outConnect e)
      IncomingRecord -> #incoming %~ insertSet (inConnect e)
    writes =
      Map.filterWithKey
        (\nid node -> lookup nid files.nodes /= Just (Right node))
        repaired
    setAside =
      mapFromList
        [ (nid, copyName nid)
        | nid <- toList (unparsable `difference` untouchable)
        ]
    -- what was kept of a node that has no file and gets none
    removals =
      leftOver files `Map.withoutKeys` (untouchable <> keysSet writes)
    derivedFrom =
      setFromList [name | (name, other, _, _) <- derivations, other `member` writes]
        -- whether an id is corrected depends on what the other nodes of the
        -- file's edges record
        <> setFromList
          [ other
          | CorrectId name _ <- corrected,
            node <- toList (lookup name readable),
            (side, e) <- recordedEdges name node,
            let other = snd (recordingNode side e),
            other `member` files.nodes
          ]
    -- the first name of this kind that no file of the node has yet (see
    -- 'namesTaken' for what else may have it)
    copyName nid =
      let taken = fromMaybe mempty (lookup nid files.others)
       in headEx . filter (`notMember` taken) $
            keptNodeFileNames (nodeFileName nid)
    repairs =
      [SetAside nid name | (nid, name) <- mapToList setAside]
        <> corrected
        <> [ Rebuild nid (length node.incoming + length node.outgoing)
           | nid <- toList rebuilt,
             node <- toList (lookup nid repaired)
           ]
        <> [ AddRecord side e
           | (nid, side, e) <- additions,
             nid `member` sound
           ]
        <> [RemoveLeftOver nid names | (nid, names) <- mapToList removals]
    notRepaired =
      [ (damage, whyNotRepaired doubted damage)
      | damage <-
          toList . scanDamage $
            applyPlan
              FixPlan
                { repairs,
                  setAside,
                  removals,
                  writes,
                  derivedFrom,
                  notRepaired = []
                }
              files
      ]

-- | The files as a plan leaves them.
applyPlan :: FixPlan -> GraphFiles -> GraphFiles
applyPlan plan files =
  GraphFiles
    { nodes = map Right plan.writes <> files.nodes,
      unread = files.unread,
      unreadable = files.unreadable,
      others =
        Map.unionWith
          (<>)
          (map singletonSet plan.setAside)
          ( Map.differenceWith
              (\names removed -> Just (names `difference` setFromList removed))
              files.others
              plan.removals
          )
    }

-- | Why a piece of damage is left as it is, given the files whose id is in
-- doubt ('filesInDoubt').
whyNotRepaired :: Map NID NID -> Damage -> String
whyNotRepaired doubted = \case
  FileSaysOtherId name claimed
    | name `member` doubted ->
        "other nodes record its edges for "
          <> show claimed
          <> ", so it may be a copy of "
          <> nodeFileName claimed
          <> " or a misnamed file: rename it or correct the id in it by hand"
  damage
    | Just name <- find (`involves` damage) (keys doubted) ->
        "it involves a node that "
          <> nodeFileName name
          <> " may or may not be (see there)"
    | Just name <- find ((`involves` damage) . snd) (mapToList doubted) <&> fst ->
        "it involves the node that "
          <> nodeFileName name
          <> " says it is (see there)"
  _ -> "fix has no repair for it"
  where
    involves nid damage = nid `elem` damagedNodes damage

describeRepair :: GraphFiles -> Repair -> String
describeRepair files = \case
  SetAside nid name ->
    "keep "
      <> nodeFileName nid
      <> ", which cannot be read as a node, as "
      <> name
  CorrectId nid claimed ->
    "correct the id in "
      <> nodeFileName nid
      <> " from "
      <> show claimed
      <> " to "
      <> show nid
  Rebuild nid edges ->
    "rebuild "
      <> nodeFileName nid
      <> ( case edges of
             0 ->
               " without edges, since no other node records an edge with "
                 <> show nid
             _ ->
               " from the "
                 <> counted edges "edge"
                 <> " that other nodes record with "
                 <> show nid
         )
      <> " ("
      <> intercalate "; " (edgesLost <> dataKept)
      <> ")"
    where
      wasKept = nid `member` files.nodes
      -- the other files named after the node, but for unreadable node files
      -- kept earlier
      dataFiles =
        filter (not . (nodeFileName nid `isPrefixOf`)) . toList $
          fromMaybe mempty (lookup nid files.others)
      edgesLost
        | wasKept =
            ["any other edge it had is only in the file that is kept"]
        | null dataFiles =
            ["any other edge it had is lost, and so is its data if it had any"]
        | otherwise = ["any other edge it had is lost"]
      dataKept =
        ["its data in " <> intercalate ", " dataFiles <> " is kept" | not (null dataFiles)]
  AddRecord side e ->
    let (recording, _) = recordingNode side e
     in "add the edge "
          <> describeEdge e
          <> " to the "
          <> ( case side of
                 IncomingRecord -> "incoming"
                 OutgoingRecord -> "outgoing"
             )
          <> " edges in "
          <> nodeFileName recording
  RemoveLeftOver nid names ->
    "remove "
      <> intercalate ", " names
      <> ", left over from a deleted node: there is no "
      <> nodeFileName nid
      <> " any more, of which fix had kept "
      <> (if length names == 1 then "it" else "them")

counted :: Int -> String -> String
counted 1 thing = "1 " <> thing
counted n thing = show n <> " " <> thing <> "s"

-- | Repair the damage of the whole graph: list every repair, then make
-- them (unless only the list was asked for). Fails if damage is left, so
-- that whoever ran @ge -e fix@ can tell from the exit code.
fix ::
  ( Members
      [RawGraph, IOE, Echo, Error UserError, GraphMetadataFilesystemOperations]
      es,
    Warn UserError :> es
  ) =>
  FixMode ->
  Eff es ()
fix mode = scanGraph >>= repair mode

-- | What fix fails with when node files can't be read: which, why (in
-- words, then once as the system said it) and what to do about each.
unreadableFiles :: NonEmpty (NID, IOError) -> Text
unreadableFiles failures =
  intercalate "\n" $
    ( "fix did nothing: it can't read every node file, and a file that it"
        <> " can't read may hold any id and any edge, so no repair is known to"
        <> " be right without it (unreadable-node-file)"
    )
      : [ "- "
            <> pack (nodeFileName nid)
            <> ": "
            <> pack (readFailure e)
            <> ". "
            <> advice e
        | (nid, e) <- toList failures
        ]
  where
    advice e
      | isPermissionError e = "Make it readable and run fix again."
      | isDoesNotExistError e =
          "Remove the link, or bring back the file that it points to, and run"
            <> " fix again."
      | ioe_type e == InappropriateType =
          "Move it out of the graph's directory and run fix again."
      | otherwise = "Run fix again when it can be read."

-- | The names for a copy of a node file that can't be read as a node
-- ('keptNodeFileNames') that something has which is no file of the node as
-- the scan has them: a directory or a symbolic link of that name, or, where
-- the volume takes names that differ only in letter case for one name, a
-- file that the directory lists under another name. No copy can be kept
-- under such a name, so fix goes on to the next one, as for a name that a
-- file of the node has: it would fail there on every run otherwise.
--
-- By node, for the nodes whose file is to be copied, and up to the first
-- name that is free.
namesTaken ::
  (Members [IOE, Error UserError] es) =>
  FilePath ->
  GraphFiles ->
  Eff es (Map NID (Set FilePath))
namesTaken directory files =
  embedCatchingErrors . fmap (Map.filter (not . null)) $
    Map.traverseWithKey
      (\nid _ -> setFromList <$> go nid (keptNodeFileNames (nodeFileName nid)))
      (Map.filter isLeft files.nodes)
  where
    go nid = \case
      name : names
        | name `member` fromMaybe mempty (lookup nid files.others) -> go nid names
        | otherwise ->
            isNameTaken (directory </> name)
              >>= bool (pure []) ((name :) <$> go nid names)
      [] -> pure []

-- | 'fix' for a graph as it was scanned.
repair ::
  ( Members
      [RawGraph, IOE, Echo, Error UserError, GraphMetadataFilesystemOperations]
      es,
    Warn UserError :> es
  ) =>
  FixMode ->
  GraphScan ->
  Eff es ()
repair mode scan = do
  directory <- getGraphFilePath
  -- in a dry run of ge the messages say what would be done
  dry <- onlyDescribesWrites
  unless (looksLikeGraph scan.files) . throwText $
    "fix did nothing: no file in "
      <> pack directory
      <> " is a node file that says the id it is named after, so it doesn't"
      <> " look like a graph"
  let changedMeanwhile nid =
        pack (nodeFileName nid)
          <> " changed while fix was looking at the graph; run fix again"
  -- A file that the directory listed and that is gone was deleted while the
  -- files were read. (A symbolic link to a file that isn't there is listed
  -- and can't be opened either, but it is still there itself.)
  for_ (mapToList scan.failures) \(nid, e) -> when (isDoesNotExistError e) do
    link <-
      embedCatchingErrors $
        pathIsSymbolicLink (directory </> nodeFileName nid)
          `catch` \(_ :: IOError) -> pure False
    unless link . throwText $ "fix did nothing: " <> changedMeanwhile nid
  for_ (NonEmpty.nonEmpty (mapToList scan.failures)) $
    throwText . unreadableFiles
  taken <- namesTaken directory scan.files
  let plan = planFix (scan.files & #others %~ Map.unionWith (<>) taken)
      onlyLists = mode == ListRepairs || dry
  -- A file that is to be created must not be there under a name that the
  -- directory doesn't list: where the volume takes names that differ in
  -- letter case for one name, writing it would replace another node's file.
  -- (The directory is listed after the names were tried, so that a file that
  -- another ge has created since the scan is not taken for such a one: it is
  -- listed by then, and is a file that changed.)
  shadowed <- embedCatchingErrors do
    opening <-
      filterM
        (doesPathExist . (directory </>) . nodeFileName)
        [nid | nid <- keys plan.writes, nid `notMember` scan.files.nodes]
    listed <- if null opening then pure [] else listDirectory directory
    pure (filter ((`notElem` listed) . nodeFileName) opening)
  for_ (headMay shadowed) \nid ->
    throwText $
      "fix did nothing: the directory lists no "
        <> pack (nodeFileName nid)
        <> ", but a file opens under that name"
        <> ( case filter (sameButForCase nid) (keys scan.files.nodes) of
               other : _ ->
                 ": "
                   <> pack (nodeFileName other)
                   <> ", whose name differs only in letter case, which this"
                   <> " volume doesn't tell apart"
               [] -> ""
           )
        <> ". Writing "
        <> pack (nodeFileName nid)
        <> " would replace that file, so fix can't rebuild "
        <> tshow nid
        <> " on this volume, and ge has no other repair for it: rm and rmnf"
        <> " of the edges that other nodes record with "
        <> tshow nid
        <> " are refused, like every edit next to damage. What a person can"
        <> " do: run fix on a copy of the graph on a volume that tells such"
        <> " names apart, or take those edges out of the node files that"
        <> " record them by hand (fsck lists them)"
  if null plan.repairs
    then echo "nothing to repair"
    else do
      echo $
        if onlyLists
          then "repairs (only listed, nothing is written):"
          else "repairs:"
      for_ plan.repairs (echo . ("- " <>) . describeRepair scan.files)
  unless (null plan.notRepaired) do
    echo "not repaired:"
    for_ plan.notRepaired \(damage, reason) ->
      echo ("- " <> describeDamage damage <> ": " <> reason)
  when (mode == ApplyRepairs && not (null plan.writes && null plan.removals)) do
    let wroteNothing why =
          throwText $
            (if dry then "fix would write nothing: " else "fix wrote nothing: ")
              <> why
        -- the files that the plan replaces and the ones that it took the new
        -- records from, and the nodes that it takes to have no file
        relied = keysSet plan.writes <> plan.derivedFrom <> keysSet plan.removals
    -- The plan is for the files as they were scanned: the ones it relies on
    -- are read again, as the directory lists them now, and nothing is
    -- written if any of them is not as it was.
    again <- readNodeFiles (Just relied) scan
    for_
      ( find
          ( \nid ->
              lookup nid again.contents /= lookup nid scan.contents
                || (nid `member` again.files.nodes)
                  /= (nid `member` scan.files.nodes)
          )
          (toList relied)
      )
      (wroteNothing . changedMeanwhile)
    -- Nor if one of them can't be written: a repair made in part leaves
    -- other damage than the one that was listed. A file that is replaced
    -- must be writable, and so must the directory for a file that is
    -- created: a node file, or the copy that is kept of one.
    unwritable <- embedCatchingErrors do
      canAdd <- writable <$> getPermissions directory
      filterM
        ( \nid -> do
            let path = directory </> nodeFileName nid
            exists <- doesPathExist path
            canReplace <- if exists then writable <$> getPermissions path else pure True
            let adds = not exists || nid `member` plan.setAside
            pure $ not (canReplace && (canAdd || not adds))
        )
        (keys plan.writes)
    unless (null unwritable) . wroteNothing $
      "there is no permission to write "
        <> intercalate ", " (map (pack . nodeFileName) unwritable)
        <> " in "
        <> pack directory
    -- A file that can't be read as a node is replaced only once a copy of it
    -- is kept, so no node is ever without its file: if the copy can't be
    -- written the file is as it was, and if the new file can't be, what the
    -- old one held is in the copy.
    let stoppedPartWay why =
          "fix stopped part way: "
            <> why
            <> ". What was written before it stays; run fix again for the"
            <> " rest."
        stopped what why = throwText (stoppedPartWay (what <> ": " <> why))
        inWords doing = \case
          IOFail io -> pack (fileFailure doing io)
          e -> tshow e
    -- An interrupt is no error: it gets the note that an error gets, when a
    -- file has been written, copied or removed by then (also the one that
    -- was being written when the interrupt arrived).
    start <- graphFilesChanged
    let interruptedPartWay =
          untoldGraphChanges start <&> \changed ->
            [stoppedPartWay "it was interrupted" | changed > 0]
    notingWhatInterruptLeaves interruptedPartWay do
      for_ (mapToList plan.writes) \(nid, node) -> do
        let file = pack (nodeFileName nid)
            copy = pack <$> lookup nid plan.setAside
        for_ (lookup nid plan.setAside) \name ->
          keepNodeMetadataCopy nid name `catchError` \_ e ->
            stopped
              (pack name <> ", the copy to keep of " <> file <> ", could not be written")
              (inWords "write" e <> "; " <> file <> " is as it was")
        writeNodeMetadata node `catchError` \_ e ->
          stopped
            (file <> " could not be written")
            (inWords "write" e <> maybe "" ("; what it held is kept in " <>) copy)
      for_ (concat (toList plan.removals)) \name ->
        removeKeptNodeMetadata name `catchError` \_ e ->
          stopped (pack name <> " could not be removed") (inWords "remove" e)
  for_ (NonEmpty.nonEmpty plan.notRepaired) $
    throwError . DamageNotRepaired . length
  where
    sameButForCase nid other =
      other /= nid
        && toLower (nodeFileName other) == toLower (nodeFileName nid)
