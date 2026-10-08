-- | Checking a whole graph for damage by reading every node file as it is.
--
-- This doesn't go through the transactions that the rest of @ge@ reads the
-- graph with: it must work on a graph that they refuse, each file is read
-- once, and no file is read through another one's view of it.
module Graph.Fsck where

import Control.Concurrent (threadDelay)
import DAL.DTO
import DAL.FileSystemOperations.Metadata (isKeptNodeFile, isKeptNodeFileName)
import DAL.RawGraph
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Effect.IOWrapper.Echo
import Error.UserError
import Models.Damage
import Models.NID
import Models.Node
import MyPrelude
import System.Directory (listDirectory)
import System.FileCoordination

-- | The files of a graph that are named after a node.
data GraphFiles = GraphFiles
  { -- | Every node file by the node it is named after: the node it holds,
    -- or why it can't be read as one
    nodes :: Map NID (Either String (Node Text ())),
    -- | The nodes whose files are there by now but were not read: they were
    -- created after the directory was listed for the scan, and nothing is
    -- said about the edges that other nodes record with them
    unread :: Set NID,
    -- | The node files among them that are there but could not be read at
    -- all (for their permissions, say), unlike one whose content is not a
    -- node: such a file may be a good node file
    unreadable :: Set NID,
    -- | The names of the other files named after a node, such as its data.
    -- They are no part of the graph, which is what the node files say: a
    -- data file whose node has no file is not damage and is left alone.
    -- What is named like a copy that fix kept of a node file but is no file
    -- (a directory, a symbolic link) is not among them: it is no copy.
    others :: Map NID (Set FilePath)
  }
  deriving stock (Eq, Show, Generic)

-- | The node files that hold a node
readableNodes :: GraphFiles -> Map NID (Node Text ())
readableNodes files = Map.mapMaybe (either (const Nothing) Just) files.nodes

-- | All the damage in a graph. Linear in the number of edges recorded (times
-- a logarithm): every record is looked up once in the node it leads to.
scanDamage :: GraphFiles -> Set Damage
scanDamage files =
  damageAmong known
    <> setFromList
      [UnparsableNodeFile nid reason | (nid, Left reason) <- mapToList files.nodes]
    <> setFromList [MissingOriginFile | nilNID `notMember` files.nodes]
    <> setFromList
      [ KeptFileWithoutNode nid names
      | -- where the files are not those of a graph, no node is known to have
        -- been there, and so nothing to be left over from one
        looksLikeGraph files,
        (nid, names) <- mapToList (leftOver files)
      ]
  where
    readable = readableNodes files
    -- a node that has no file is known to be missing; one whose file can't
    -- be read is not known at all, so nothing is said about its edges
    referenced =
      setFromList
        [ snd (recordingNode side e)
        | (nid, node) <- mapToList readable,
          (side, e) <- recordedEdges nid node
        ]
    known =
      map Just readable
        <> mapFromList
          [ (nid, Nothing)
          | nid <- toList (asSet referenced),
            nid `notMember` files.nodes,
            nid `notMember` files.unread
          ]

-- | Whether a file with this name is named as a copy that fix kept of the
-- node's file, which it couldn't read as a node.
isKeptFileOf :: NID -> FilePath -> Bool
isKeptFileOf nid = isKeptNodeFileName (nodeFileName nid)

-- | Whether the files are those of a graph: at least one of them is a node
-- file that says the id it is named after. (Any file whose name is twelve
-- letters and digits and @.json@ is named like a node file, so a directory
-- in which no such file is a node is taken for something else, also if it is
-- a graph of which every node file was lost.) No other file says so,
-- whatever it is named like: what fix does to a graph, it must not do to a
-- directory of something else.
looksLikeGraph :: GraphFiles -> Bool
looksLikeGraph files =
  any (\(name, node) -> node.nid == name) (mapToList (readableNodes files))

-- | The nodes that have no node file but copies that a fix kept of one, with
-- the names of those. They are left over from nodes that were deleted:
-- deleting a node removes the copies kept of its file, if it finds them and
-- can. (A fix never leaves a node without its file: it replaces the file
-- only once the copy is written.)
leftOver :: GraphFiles -> Map NID [FilePath]
leftOver files =
  mapFromList
    [ (nid, kept)
    | (nid, names) <- mapToList files.others,
      nid `notMember` files.nodes,
      nid `notMember` files.unread,
      let kept = filter (isKeptFileOf nid) (toList names),
      not (null kept)
    ]

-- | Read a node file's contents as a node.
parseNodeFile :: ByteString -> Either String (Node Text ())
parseNodeFile = fmap nodeFromDTO . eitherDecodeStrict @(NodeDTO Text)

-- | The node a file is named after and whether it is that node's node file
-- (rather than, say, its data), if it is named after a node at all.
classifyFileName :: FilePath -> Maybe (NID, Bool)
classifyFileName name =
  let (stem, extension) = break (== '.') name
   in (,extension == ".json") <$> readMay ('@' : stem)

-- | A graph's files as they are, with the bytes of every node file.
data GraphScan = GraphScan
  { files :: GraphFiles,
    -- | what each node file held when it was read
    contents :: Map NID ByteString,
    -- | why each of the unreadable node files could not be read
    failures :: Map NID IOError
  }
  deriving stock (Eq, Show, Generic)

-- | Read every node file of the graph once. A file that can't be read is
-- noted as such and the scan goes on.
scanGraph :: (Members [RawGraph, IOE, Error UserError] es) => Eff es GraphScan
scanGraph =
  readNodeFiles
    Nothing
    (GraphScan (GraphFiles mempty mempty mempty mempty) mempty mempty)

-- | Read the files of some nodes (of all with 'Nothing'), in place of what a
-- scan has of those nodes.
--
-- Which files there are is what the directory lists, at every reading: a
-- node has a file if a file with exactly that name is listed, and a listed
-- file that can't be opened is a file that can't be read. Going by whether a
-- name can be opened instead gives another answer for the same files where
-- the volume takes names that differ in letter case for one name, or where a
-- node file is a symbolic link to a file that isn't there, and two readings
-- of a graph that holds still must not differ.
--
-- The node files that are listed and that neither this reading nor one
-- before it read are noted as unread: a file that is read again may refer to
-- a node that was created since the scan listed the directory, which is no
-- node without a file.
readNodeFiles ::
  (Members [RawGraph, IOE, Error UserError] es) =>
  Maybe (Set NID) ->
  GraphScan ->
  Eff es GraphScan
readNodeFiles which scan = do
  directory <- getGraphFilePath
  names <- embedCatchingErrors $ listDirectory directory
  let wanted nid = maybe True (member nid) which
      named =
        [ (nid, isNode, name)
        | name <- names,
          Just (nid, isNode) <- [classifyFileName name],
          wanted nid
        ]
  -- a copy that fix kept of a node file is a file ('isKeptNodeFile'): what
  -- else has the name of one is no file of the node
  notNodeFiles <-
    embedCatchingErrors . flip filterM [(nid, name) | (nid, False, name) <- named] $
      \(nid, name) ->
        if isKeptFileOf nid name
          then isKeptNodeFile (directory </> name)
          else pure True
  let others =
        Map.fromListWith
          (<>)
          [(nid, singletonSet name) | (nid, name) <- notNodeFiles]
  read' <- for [(nid, name) | (nid, True, name) <- named] \(nid, name) ->
    (nid,) <$> embedCatchingErrors (readNodeFile (directory </> name))
  let kept :: Map NID a -> Map NID a
      kept = Map.filterWithKey (\nid _ -> not (wanted nid))
      failures = mapFromList [(nid, e) | (nid, Left e) <- read']
      nodes =
        kept scan.files.nodes
          <> mapFromList [(nid, nodeRead result) | (nid, result) <- read']
      listed =
        setFromList
          [nid | name <- names, Just (nid, True) <- [classifyFileName name]]
  pure
    GraphScan
      { files =
          GraphFiles
            { nodes,
              unread = listed `difference` keysSet nodes,
              unreadable =
                filterSet (not . wanted) scan.files.unreadable
                  <> keysSet failures,
              others = kept scan.files.others <> others
            },
        contents =
          kept scan.contents
            <> mapFromList [(nid, bytes) | (nid, Right bytes) <- read'],
        failures = kept scan.failures <> failures
      }

-- | The node that reading a node file gave, or why there is none.
nodeRead :: Either IOError ByteString -> Either String (Node Text ())
nodeRead = either (Left . readFailure) parseNodeFile

-- | The bytes of a node file, or why it couldn't be read.
readNodeFile :: FilePath -> IO (Either IOError ByteString)
readNodeFile path =
  coordinateReading path False defaultReadingOptions (try @IO @IOError . B.readFile)

-- | How often 'settle' reads the files of its findings again at most.
maxReadings :: Int
maxReadings = 5

-- | Read the files that a scan's findings are about again until its findings
-- can be relied on, and give the files as last read, and whether any of them
-- changed while they were read. The first argument runs before each such
-- reading, given its number from 1 ('pauseBeforeReading'; the tests change
-- files there).
--
-- A scan reads the files one after the other, and another @ge@ or a sync may
-- write to the graph meanwhile: a node that was created after the directory
-- was listed looks like a missing file, and an edge of which one node was
-- read before it was added and the other one after looks like a lone record.
-- Such a finding is gone when the files are read again. Damage that is there
-- all along may change its form meanwhile (the file that lacked a record is
-- deleted; a file that could not be parsed is cut off elsewhere): that is
-- still damage, and is reported as the latest reading shows it. So what is
-- reported is never what an earlier reading found, and "no damage" is only
-- said if every file was read and the latest reading of each shows none.
--
-- One such reading reads the files of every finding, and then those of the
-- findings that the files read give, until every finding is about files that
-- this reading has read: a file read earlier may be out of date by now, and a
-- node that it refers to may have been created since the directory was
-- listed. So the findings of a reading come from files that were read within
-- a moment of each other, where a scan of the whole graph takes long.
--
-- The findings count once the next reading finds the same; while it finds
-- others, the files are read again, as often as 'maxReadings' allows. What
-- the last reading shows is reported in any case.
--
-- Whether files changed is only ever told by two readings of the node files
-- that differ ('sameNodeFiles'): in what one of them held, or in which of
-- them the directory lists. No clock is asked, so a file that is dated in the
-- future changes nothing here, and neither does a file that is no node file
-- and comes or goes meanwhile, be it the data of a node of a finding.
settle ::
  (Members [RawGraph, IOE, Error UserError] es) =>
  (Int -> Eff es ()) ->
  GraphScan ->
  Eff es (GraphScan, Bool)
settle before = go (1 :: Int) False
  where
    go reading changed scan
      | null damage = pure (scan, changed)
      | otherwise = do
          before reading
          again <- readInvolved mempty scan
          let changed' = changed || not (sameNodeFiles again scan)
          if scanDamage again.files == damage || reading >= maxReadings
            then pure (again, changed')
            else go (reading + 1) changed' again
      where
        damage = scanDamage scan.files
    involved = asSet . setFromList . concatMap damagedNodes
    -- given the nodes whose files this reading has read
    readInvolved fresh scan
      | null stale = pure scan
      | otherwise =
          readNodeFiles (Just stale) scan >>= readInvolved (fresh <> stale)
      where
        stale = involved (scanDamage scan.files) `difference` fresh

-- | Whether two readings found the same node files, holding the same. The
-- other files named after a node are left out of this: they are no part of
-- the graph.
sameNodeFiles :: GraphScan -> GraphScan -> Bool
sameNodeFiles = (==) `on` (#files . #others .~ mempty)

-- | List the damage of the whole graph; fail if there is any, so that
-- whoever ran @ge -e fsck@ can tell from the exit code.
fsck :: (Members [RawGraph, IOE, Echo, Error UserError] es) => Eff es ()
fsck = fsckRereading pauseBeforeReading

-- | What 'fsck' does before it reads the files of its findings again: nothing
-- before the first time. Before a later one, which is there because the
-- reading before it found other damage than the one before that, it waits (a
-- tenth of a second, then twice as long each time): a change that another
-- @ge@ is half way through writing looks like damage, and would look the
-- same to a reading that came right after.
--
-- So fsck takes no longer on a graph that holds still, damaged or not, and
-- up to a second and a half longer on one that is being written all the
-- while.
pauseBeforeReading :: (IOE :> es) => Int -> Eff es ()
pauseBeforeReading = liftIO . threadDelay . pauseBefore

-- | How long 'pauseBeforeReading' waits, in microseconds
pauseBefore :: Int -> Int
pauseBefore reading
  | reading > 1 = 100000 * 2 ^ (reading - 2)
  | otherwise = 0

-- | 'fsck', given what it does before it reads the files of its findings
-- again (see 'settle').
fsckRereading ::
  (Members [RawGraph, IOE, Echo, Error UserError] es) =>
  (Int -> Eff es ()) ->
  Eff es ()
fsckRereading before = do
  (scan, changed) <- settle before =<< scanGraph
  let damage = scanDamage scan.files
  when changed . echo $
    "node files changed while fsck was reading them; what follows is what"
      <> " its latest reading of them shows"
  for_ damage (echo . describeDamage)
  case length damage of
    0 ->
      echo $
        "no damage found in "
          <> show (length scan.files.nodes)
          <> " node files"
    count -> throwError (DamageFound count)
