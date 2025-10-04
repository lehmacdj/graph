-- | Module for dumping a graph to a ByteString representation.
-- Used in GoldenTests for validation of the final graph.
module DAL.DumpGraph (dumpGraph) where

import DAL.Serialization
import Models.NID
import Models.Node
import MyPrelude

nodeDebugRepresentation :: FilePath -> NID -> IO ByteStringBuilder
nodeDebugRepresentation base nid = do
  result :: Either String (Node String (Maybe ByteString)) <- deserializeNode base nid
  case result of
    Right n ->
      pure $ toBuilder $ encodeUtf8 $ nshowSettings defaultCompactNodeShowSettings {showIncoming = True} n <> "\n"
    Left e ->
      pure $
        toBuilder $
          encodeUtf8 $
            "couldn't deserialize node "
              <> tshow nid
              <> "; failed with error "
              <> pack e
              <> "\n"

-- | Dumps all nodes in a graph directory to a ByteString Builder.
-- Nodes are sorted to ensure consistent ordering across different systems.
dumpGraph :: FilePath -> IO ByteStringBuilder
dumpGraph graphDir = do
  -- to guarantee that different ordering on different systems don't affect
  -- test results we sort the node ids so that we always dump the graph
  -- in a consistent order
  allNodes <- sort <$> getAllNodeIds graphDir
  mconcat <$> traverse (nodeDebugRepresentation graphDir) allNodes
