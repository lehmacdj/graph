-- |
--   Import directories from filesystems as nodes.
--   Data is automatically deduplicated.
--   Each file is hashed and stored in a node: by-hash/{hash}.
--   Other instances of files with that data are stored as a link to that node
--   from their place in the directory tree.
module Graph.Import.FileSystem where

import Data.Digest.Pure.SHA
import Data.List.NonEmpty qualified as NonEmpty
import Effect.IOWrapper.FileSystem
import Effect.IOWrapper.GetTime
import Error.Missing
import Error.UserError
import Error.Warn
import Graph.FreshNID
import Graph.GraphEditing (HasGraph)
import Graph.GraphMetadataEditing
import Graph.Import.ByteString
import Graph.Time (taggingFreshNodesWithTime)
import Graph.Utils
import Models.Edge
import Models.NID
import MyPrelude
import System.Directory.Tree hiding (readDirectory)
import System.FilePath qualified as FilePath

computeSHA :: ByteString -> Text
computeSHA = pack . showDigest . sha512 . fromStrict

-- | Import the directory under the node. A directory that can't be read
-- imports nothing and fails; the parts of it that can't be read are
-- skipped, with a warning that names them.
importDirectory ::
  ( Members
      [GetTime, FreshNID, FileSystem, Warn UserError, Error UserError]
      effs,
    Error Missing :> effs,
    HasGraph Text effs
  ) =>
  FilePath ->
  NID ->
  Eff effs ()
importDirectory base nid = do
  fileTree <- readDirectory base
  case fileTree of
    Failed _ e -> throwError (UnreadableDirectory base e)
    _ -> do
      for_ (NonEmpty.nonEmpty (unreadableParts fileTree)) $
        warn . UnreadablePartsSkipped base
      addDirectories fileTree nid

-- | The parts of a directory tree that could not be read, each by its path
-- below the root of the tree, with why.
unreadableParts :: DirTree a -> [(FilePath, IOError)]
unreadableParts = \case
  Dir _ children -> concatMap (go "") children
  _ -> []
  where
    go above = \case
      Failed name e -> [(above FilePath.</> name, e)]
      Dir name children -> concatMap (go (above FilePath.</> name)) children
      File _ _ -> []

addDirectories ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph Text effs) =>
  DirTree ByteString ->
  NID ->
  Eff effs ()
addDirectories dt' root = do
  let go dt nid = case dt of
        File fn cs -> do
          nid' <- importData cs
          -- TODO: possibly add handling of filename extensions, to categorize
          insertEdge (Edge nid (pack fn :: Text) nid')
        Dir fn [] -> do
          _ <- taggingFreshNodesWithTime $ nid `transitionsVia` (pack fn :: Text)
          pure ()
        Dir fn (x : xs) -> do
          nid' <- taggingFreshNodesWithTime $ nid `transitionsVia` (pack fn :: Text)
          -- dfs down x
          go x nid'
          -- then continue evaluating at this point
          go (Dir fn xs) nid
        Failed _ _ -> pure ()
  go dt' root
