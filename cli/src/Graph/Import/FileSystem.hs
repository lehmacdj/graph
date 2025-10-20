-- |
--   Import directories from filesystems as nodes.
--   Data is automatically deduplicated.
--   Each file is hashed and stored in a node: by-hash/{hash}.
--   Other instances of files with that data are stored as a link to that node
--   from their place in the directory tree.
module Graph.Import.FileSystem where

import Data.Digest.Pure.SHA
import Effect.IOWrapper.Echo
import Effect.IOWrapper.FileSystem
import Effect.IOWrapper.GetTime
import Error.Missing
import Graph.Effect
import Graph.FreshNID
import Graph.Import.ByteString
import Graph.Time (taggingFreshNodesWithTime)
import Graph.Utils
import Models.Edge
import MyPrelude
import System.Directory.Tree hiding (readDirectory)

computeSHA :: ByteString -> Text
computeSHA = pack . showDigest . sha512 . fromStrict

importDirectory ::
  ( Members [GetTime, FreshNID, FileSystem, Echo, Error Missing] effs,
    HasGraph Text effs
  ) =>
  FilePath ->
  NID ->
  Sem effs ()
importDirectory base nid = do
  fileTree <- readDirectory base
  when (anyFailed fileTree) do
    echo $
      "error: search failed at least partially, "
        ++ "missed directories will be ignored"
    echo "the failed files are:"
    echo . show $ failures fileTree
  addDirectories fileTree nid

addDirectories ::
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph Text effs) =>
  DirTree ByteString ->
  NID ->
  Sem effs ()
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
