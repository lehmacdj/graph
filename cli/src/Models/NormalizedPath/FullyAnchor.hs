module Models.NormalizedPath.FullyAnchor where

import Models.NormalizedPath.Anchor
import Models.NormalizedPath.Traversals
import Models.NormalizedPath.Types
import MyPrelude

-- | Expand all Unanchored anchors to produce the least constrained path
-- possible. This may result in a very large number of paths
-- (e.g. @(a & b)/(c & d) => (a/c & a/d & b/c & b/d)@).
leastConstrainedNormalizedPath ::
  NormalizedPath Anchor ->
  NormalizedPath FullyAnchored
leastConstrainedNormalizedPath =
  over (_Unwrapped . setmapped) convertDeterministicPath
  where
    convertDeterministicPath = \case
      Rooted p -> Rooted $ convertRooted p
      Pointlike p -> Pointlike $ convertPointlike p

    convertRooted ::
      (HasCallStack) =>
      RootedDeterministicPath Anchor ->
      RootedDeterministicPath FullyAnchored
    convertRooted (RootedDeterministicPath rootBranches target) =
      let newTarget = convertPointlike target
          newRootBranches =
            rootBranches
              & mapToList
              & concatMap explodeUnanchored
              & map (uncurry singletonMap)
              & unionsWith (<>)
       in smartBuildRootedDeterministicPath
            newRootBranches
            newTarget

    explodeUnanchored ::
      (DeterministicPath Anchor, OSet (DPBranch Anchor)) ->
      [(DeterministicPath FullyAnchored, OSet (DPBranch FullyAnchored))]
    explodeUnanchored = \case
      ( Rooted
          rdp@RootedDeterministicPath {target = PointlikeDeterministicPath {anchor = Unanchored}},
        branchExtensions
        ) ->
          let RootedDeterministicPath {..} = convertRooted rdp
              rootBranches' :: [(DeterministicPath FullyAnchored, DPBranch FullyAnchored)] =
                rootBranches
                  & mapToList
                  & over (mapped . _2) toList
                  & concatMap (\(root, bs) -> map (root,) bs)
              branchExtensions' :: OSet (DPBranch FullyAnchored) = unions (mapOSet convertBranch branchExtensions)
           in (rootBranches' `cartesianProduct` toList branchExtensions')
                & map
                  ( \((root, b1), b2) ->
                      smartBuildRootBranch
                        root
                        ( singletonSet $
                            smartBuildSequence
                              (singletonSet b1)
                              (PointlikeDeterministicPath (FJoinPoint mempty) mempty)
                              (singletonSet b2)
                        )
                  )
      (dp, bs) ->
        singleton $
          smartBuildRootBranch
            (convertDeterministicPath dp)
            (unions $ mapOSet convertBranch bs)

    convertAnchor = \case
      Unanchored -> FJoinPoint mempty
      JoinPoint {..} -> FJoinPoint {..}
      Specific nid -> FSpecific nid

    convertPointlike (PointlikeDeterministicPath Unanchored (toList -> (_ : _))) =
      error "convertPointlike: Unanchored path with loops"
    convertPointlike (PointlikeDeterministicPath anchor loops) =
      PointlikeDeterministicPath
        (convertAnchor anchor)
        (unions (mapOSet convertBranch loops))

    convertBranch ::
      (HasCallStack) =>
      DPBranch Anchor ->
      OSet (DPBranch FullyAnchored)
    convertBranch = \case
      DPOutgoing t -> singletonSet $ DPOutgoing t
      DPIncoming t -> singletonSet $ DPIncoming t
      DPSequence as1 midpoint bs2 -> do
        let as1' = unions (mapOSet convertBranch as1)
        let bs2' = unions (mapOSet convertBranch bs2)
        case midpoint.anchor of
          Unanchored
            | null midpoint.loops ->
                mapOSet
                  ( \(x, y) ->
                      smartBuildSequence
                        (singletonSet x)
                        (convertPointlike joinPoint)
                        (singletonSet y)
                  )
                  (as1' `cartesianProductSet` bs2')
            | otherwise -> error "broken invariant: Unanchored path with loops"
          _ -> singletonSet $ DPSequence as1' (convertPointlike midpoint) bs2'

-- | Assign one JoinPoint to each Unanchored anchor
leastNodesNormalizedPath ::
  NormalizedPath Anchor ->
  NormalizedPath FullyAnchored
leastNodesNormalizedPath path =
  runIdentity $ traverseAnchors (Identity . fullyAnchor) path
