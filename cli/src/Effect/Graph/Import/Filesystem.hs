{-# LANGUAGE FlexibleContexts #-}

-- |
--   Import directories from filesystems as nodes.
--   Data is automatically deduplicated.
--   Each file is hashed and stored in a node: by-hash/{hash}.
--   Other instances of files with that data are stored as a link to that node
--   from their place in the directory tree.
module Effect.Graph.Import.Filesystem where

import Data.Digest.Pure.SHA
import Effect.Console
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Graph.Import.ByteString
import Effect.Time
import Models.Graph hiding (insertEdge)
import Graph.Time (taggingFreshNodesWithTime)
import MyPrelude
import System.Directory.Tree hiding (readDirectory)
import UserError

computeSHA :: ByteString -> String
computeSHA = showDigest . sha512 . fromStrict

importDirectory ::
  ( Members [GetTime, FreshNID, FileSystemTree, Console, Error Missing] effs,
    HasGraph String effs
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
  (Members [FreshNID, Error Missing, GetTime] effs, HasGraph String effs) =>
  DirTree ByteString ->
  NID ->
  Sem effs ()
addDirectories dt' root = do
  let go dt nid = case dt of
        File fn cs -> do
          nid' <- importData cs
          -- TODO: possibly add handling of filename extensions, to categorize
          insertEdge (Edge nid fn nid')
        Dir fn [] -> do
          _ <- taggingFreshNodesWithTime $ nid `transitionsVia` fn
          pure ()
        Dir fn (x : xs) -> do
          nid' <- taggingFreshNodesWithTime $ nid `transitionsVia` fn
          -- dfs down x
          go x nid'
          -- then continue evaluating at this point
          go (Dir fn xs) nid
        Failed _ _ -> pure ()
  go dt' root
