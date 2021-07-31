-- | Tiny script that is designed for dumping a graph to the console. Used in
-- GoldenTests for validation of the final graph.
module Executable.DumpGraph (main) where

import Graph.Serialize2
import Graph.Types
import MyPrelude

deserializeNodeWithErrorReporting :: FilePath -> NID -> IO (Node String)
deserializeNodeWithErrorReporting base nid = do
  result <- deserializeNode base nid
  case result of
    Right n -> pure n
    Left e ->
      error $
        "couldn't deserialize node " ++ show nid
          ++ "; failed with error "
          ++ e

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "must pass exactly one argument, a valid graphDir"
  let graphDir = unpack $ headEx args
  -- to guarantee that different ordering on different systems don't affect
  -- test results we sort the node ids so that we always dump the graph
  -- in a consistent order
  allNodes <- sort <$> getAllNodeIds graphDir
  traverse_ (print <=< deserializeNodeWithErrorReporting graphDir) allNodes
