{-# LANGUAGE FlexibleContexts #-}

-- |
--   Import directories from filesystems as nodes.
--   Data is automatically deduplicated.
--   Each file is hashed and stored in a node: by-hash/{hash}.
--   Other instances of files with that data are stored as a link to that node
--   from their place in the directory tree.
module Graph.Import.FileSystem where

import Data.Digest.Pure.SHA
import Effect.Console
import Effect.FileSystem
import Effect.FreshNID
import Effect.Time
import Error.Utils
import Graph.Effect
import Graph.Import.ByteString
import Graph.Time (taggingFreshNodesWithTime)
import Graph.Utils
import Models.Edge
import MyPrelude
import System.Directory.Tree hiding (readDirectory)

computeSHA :: ByteString -> String
computeSHA = showDigest . sha512 . fromStrict

importDirectory ::
  ( Members [GetTime, FreshNID, FileSystem, Console, Error Missing] effs,
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
