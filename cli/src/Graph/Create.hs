module Graph.Create where

import Graph.GraphMetadataEditing
import Models.Edge
import Models.NID
import Models.NormalizedPath
import Models.NormalizedPath.Parse
import MyPrelude
import Utils.Testing

-- | Create all the nodes/edges such that resolving the path would produce the
-- exact same result.
createNPath ::
  (Member GraphMetadataEditing r) =>
  NormalizedPath NID ->
  Sem r ()
createNPath =
  traverseViaTransitions_ \source direction sink -> case direction of
    DPOutgoing' (DPLiteral transition) ->
      touchNode source *> touchNode sink *> insertEdge (Edge {..})
    DPIncoming' (DPLiteral transition) ->
      touchNode source *> touchNode sink *> insertEdge (dualizeEdge Edge {..})
    _ -> error "non-DPLiteral transitions are illegal for `NormalizePath NID`"

spec_createNPath :: Spec
spec_createNPath = do
  let createsGraph :: Text -> [Edge Text] -> Spec
      createsGraph pathStr expectedEdges =
        it ("createNPath " <> unpack pathStr) do
          pathToCreate <-
            unwrapEx "first path must be fully specific"
              . traverseAnchors fullySpecific
              <$> parseForTest "normalized path" pNormalizedPath pathStr
          graphFromGraphMetadataEditing (createNPath pathToCreate)
            `shouldBeMultiline` graphWithEdges expectedEdges
  "[@1<a]>@2"
    `createsGraph` [edge' 1 "a" 2]
  "@1[a & b]"
    `createsGraph` [edge' 1 "a" 1, edge' 1 "b" 1]
  "[@1<a]>@2 + @1[a]"
    `createsGraph` [edge' 1 "a" 2, edge' 1 "a" 1]
  "[@1<a/@2|b]>@3"
    `createsGraph` [edge' 1 "a" 2, edge' 2 "b" 3]
  "[@1<a & @2<b]>@3"
    `createsGraph` [edge' 1 "a" 3, edge' 2 "b" 3]
  "[@1<a/@2|b]>@3 + @4[c]"
    `createsGraph` [edge' 1 "a" 2, edge' 2 "b" 3, edge' 4 "c" 4]
  "[@1<a & @2<b]>@3"
    `createsGraph` [edge' 1 "a" 3, edge' 2 "b" 3]
  "[[@1<a/@2|b]>@3<c & @4<d]>@5"
    `createsGraph` [edge' 1 "a" 2, edge' 2 "b" 3, edge' 3 "c" 5, edge' 4 "d" 5]
