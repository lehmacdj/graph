-- | Tiny script that is designed for dumping a graph to the console. Used in
-- GoldenTests for validation of the final graph.
module Executable.DumpGraph (main) where

import DAL.Serialization
import Models.Types
import MyPrelude

printNodeDebugRepresentation :: FilePath -> NID -> IO ()
printNodeDebugRepresentation base nid = do
  result :: Either String (Node' String) <- deserializeNode base nid
  case result of
    Right n -> print n
    Left e ->
      say $
        "couldn't deserialize node " <> tshow nid
          <> "; failed with error "
          <> pack e

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
  traverse_ (printNodeDebugRepresentation graphDir) allNodes
