-- | Tiny script that is designed for dumping a graph to the console. Used in
-- GoldenTests for validation of the final graph.
module Main (main) where

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
  allNodes <- getAllNodeIds graphDir
  traverse_ (print <=< deserializeNodeWithErrorReporting graphDir) allNodes
