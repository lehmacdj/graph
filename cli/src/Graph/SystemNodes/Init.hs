module Graph.SystemNodes.Init
  ( createSystemNodes,
    systemEdges,
    SystemParts (..),
  )
where

import Graph.GraphMetadataEditing
import Graph.SystemNodes
import Models.Edge
import Models.NID
import Models.Node
import MyPrelude

-- | The edges that every graph has between its system nodes
systemEdges :: [Edge Text]
systemEdges =
  [ Edge systemNodeNID "origin" nilNID,
    Edge systemNodeNID "tags" tagsNID,
    Edge systemNodeNID "import-dates" importDatesNID,
    Edge systemNodeNID "file-hashes" fileHashesNID,
    Edge systemNodeNID "import-urls" importUrlsNID,
    Edge systemNodeNID "historical-special-nodes" historicalSystemNodesNID,
    Edge historicalSystemNodesNID "sequence-ids" sequenceIDsNID,
    Edge systemNodeNID "mime-types" mimeTypesNID,
    Edge systemNodeNID "file-extensions" fileExtensionsNID
  ]

-- | Make sure that the system nodes and the edges to them exist. What is
-- there already is left alone, so on a graph that has them all this only
-- reads: an edit there would be refused for damage that it has nothing to do
-- with (an edge counts as there if either of its two nodes records it;
-- whether both do is for fsck to say).
--
-- Returns what it added to a graph that was there already: the nodes that it
-- created, whose files were lost or which the version of @ge@ that made the
-- graph didn't have, and the edges that it added between nodes that were both
-- there. Nothing is returned for a new graph, which has no node file but
-- perhaps that of the origin: there creating them is no news.
createSystemNodes ::
  forall effs. (GraphMetadataEditing :> effs) => Eff effs SystemParts
createSystemNodes = do
  hadSystemNodes <- isJust <$> getNodeMetadata @Text systemNodeNID
  -- only listed for a graph without the system node, so not on every start
  isNew <-
    if hadSystemNodes
      then pure False
      else all (== nilNID) <$> listNodes @Text
  created <-
    catMaybes
      <$> sequence
        [ ensureNode systemNodeNID,
          ensureNode nilNID,
          ensureNode tagsNID,
          ensureNode importDatesNID,
          ensureNode fileHashesNID,
          ensureNode importUrlsNID,
          ensureNode historicalSystemNodesNID,
          ensureNode sequenceIDsNID,
          ensureNode mimeTypesNID,
          ensureNode fileExtensionsNID
        ]
  inserted <- catMaybes <$> traverse ensureEdge systemEdges
  pure
    if isNew
      then SystemParts [] []
      else
        SystemParts
          { nodes = created,
            -- the edge to a node that was created says nothing more
            edges =
              filter
                (\e -> e.source `notElem` created && e.sink `notElem` created)
                inserted
          }
  where
    ensureNode nid =
      getNodeMetadata @Text nid >>= \case
        Just _ -> pure Nothing
        Nothing -> touchNode @Text nid $> Just nid
    ensureEdge :: Edge Text -> Eff effs (Maybe (Edge Text))
    ensureEdge e = do
      source <- getNodeMetadata e.source
      sink <- getNodeMetadata e.sink
      let recorded =
            maybe False (member (outConnect e) . (.outgoing)) source
              || maybe False (member (inConnect e) . (.incoming)) sink
      if recorded then pure Nothing else insertEdge e $> Just e

-- | System nodes and edges between them
data SystemParts = SystemParts
  { nodes :: [NID],
    edges :: [Edge Text]
  }
  deriving stock (Eq, Show, Generic)
