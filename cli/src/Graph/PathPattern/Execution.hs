-- | Command policy on top of route patterns (docs/path-pattern-ir.md §3,
-- §5). Queries append @/\@@ before compiling, so that every output meets
-- at one node; construction does not. The engine itself adds no defaults.
module Graph.PathPattern.Execution
  ( PathReading,
    readNode,
    queryPath,
    resolveCommandQuery,
    queryTargets,
    orderedQueryTargets,
    constructPath,
    constructTag,
    relabelPath,
    moveSelectedEdges,
    copySelectedEdges,
    removeSelectedEdges,
    completionTransitions,
    renderChoiceError,
  )
where

import Error.UserError
import Graph.Effect
import Graph.FreshNID
import Graph.PathPattern.Resolve (resolveQueryWith)
import Models.Command (DestinationParents (..))
import Models.Connect (Connect' (..))
import Models.Edge
import Models.Node
import Models.Path.Simple qualified as P
import Models.PathPattern
import Models.PathPattern.Compile (compilePath)
import Models.PathPattern.Match
import Models.PathPattern.Residuate
import MyPrelude

type PathReading r = (ReadGraphDataless Text :> r)

readNode :: (PathReading r) => NID -> Eff r (Maybe (Node Text ()))
readNode n = fmap void <$> getNodeDataless @Text n

-- | Query convergence is command policy, not part of compilation.
queryPath :: P.Path -> PathQuery
queryPath p = compilePath (p P.:/ P.One)

resolveCommandQuery :: (PathReading r) => NID -> P.Path -> Eff r [Resolution]
resolveCommandQuery cur = resolveQueryWith readNode cur . queryPath

queryTargets :: (PathReading r) => NID -> P.Path -> Eff r (Set NID)
queryTargets cur p = foldMap targets <$> resolveCommandQuery cur p

-- | Targets in witness order, for commands that pick a survivor (merge).
orderedQueryTargets :: (PathReading r) => NID -> P.Path -> Eff r [NID]
orderedQueryTargets cur p = do
  rs <- resolveCommandQuery cur p
  pure $
    ordNub
      [ endOf rt
      | r <- rs,
        w <- witnesses r,
        rt <- concreteRoutes r w
      ]

endOf :: ConcreteRoute -> NID
endOf r = maybe r.root (.target) (lastMay r.hops)

-- * Construction

type Constructing r =
  ( PathReading r,
    WriteGraph Text :> r,
    FreshNID :> r,
    Error UserError :> r
  )

-- | Residuate a path and choose its repair, changing nothing.
planConstruction ::
  (PathReading r, Error UserError :> r) =>
  Bool ->
  NID ->
  P.Path ->
  Eff r (Maybe Residual)
planConstruction allowOrphans cur p = do
  result <- residuate readNode noLabels cur (compilePath p)
  residuation <- either (throwError . OtherError . renderResidualError) pure result
  case chooseRepair residuation of
    NoChange -> pure Nothing
    Impossible -> throwError $ OtherError "This path cannot be constructed."
    Ambiguous rs -> throwError . OtherError $ renderChoiceError rs
    Apply r -> do
      let orphans = patchOrphans r.patch
      unless (allowOrphans || null orphans) $
        throwError . OtherError $
          "This path would create nodes that are not connected to the "
            <> "existing graph; nothing was changed. Use mk --allow-orphans "
            <> "to create them anyway.\n"
            <> unlines
              [ "  " <> intercalate ", " (map renderBinding (toList o))
              | o <- orphans
              ]
      pure (Just r)

renderResidualError :: ResidualError -> Text
renderResidualError = \case
  UnsupportedLabel m _ _ (r :| []) ->
    "Cannot create the "
      <> renderMatcher m
      <> " edge without a label: give it a literal label. Nothing was "
      <> "changed. (UnsupportedLabel)\n  "
      <> renderResidualWith renderPatchLabel r
      <> "\n"
  UnsupportedLabel m _ _ rs ->
    "Cannot create the "
      <> renderMatcher m
      <> " edge without a label ("
      <> (if length needing > 1 then "repairs " else "repair ")
      <> intercalate ", " (map tshow needing)
      <> " below), and even with one this path has "
      <> tshow (length rs)
      <> " possible repairs. mk does not fall back to a repair that needs "
      <> "no label; nothing was changed. Give the edge a literal label, or "
      <> "pin a node with @N or exclude one with !{@N} to choose a repair. "
      <> "(UnsupportedLabel)\n"
      <> renderNumbered (renderResidualWith renderPatchLabel) rs
    where
      needing =
        [ i
        | (i, r) <- zip [1 :: Int ..] (toList rs),
          any (isMissing . (.transition)) r.patch.edges
        ]
      isMissing = \case
        Missing _ -> True
        Known _ -> False

renderChoiceError :: NonEmpty Residual -> Text
renderChoiceError rs =
  "This path has "
    <> tshow (length rs)
    <> " possible repairs. Nothing was changed. Pin a node with @N or "
    <> "exclude one with !{@N} to choose.\n"
    <> renderNumbered renderResidual rs

-- | The first 'shownRepairs' items, numbered, and how many more there are.
renderNumbered :: (a -> Text) -> NonEmpty a -> Text
renderNumbered render rs =
  unlines $
    [ "  " <> tshow i <> ". " <> render r
    | (i, r) <- zip [1 :: Int ..] (take shownRepairs (toList rs))
    ]
      <> [ "  ... and " <> tshow hidden <> " more"
         | let hidden = length rs - shownRepairs,
           hidden > 0
         ]

shownRepairs :: Int
shownRepairs = 5

-- | Create the nodes and edges of a residual. Returns the binding of every
-- created node.
applyResidual ::
  (WriteGraph Text :> r, FreshNID :> r) =>
  Residual ->
  Eff r (Map Binding NID)
applyResidual r = do
  created <-
    for (toList r.patch.nodes) \b -> case b of
      Existing n -> pure (b, n)
      Fresh _ -> (b,) <$> freshNID
  let names = mapFromList created :: Map Binding NID
      concrete b = case b of
        Existing n -> n
        Fresh _ -> findWithDefault (error "applyResidual: unallocated") b names
  traverse_ (touchNode @Text . snd) created
  traverse_
    (\e -> insertEdge @Text (Edge (concrete e.source) e.transition (concrete e.sink)))
    r.patch.edges
  pure names

-- | Apply exactly the chosen repair of a path. Returns the created nodes.
constructPath :: (Constructing r) => Bool -> NID -> P.Path -> Eff r (Set NID)
constructPath allowOrphans cur p =
  planConstruction allowOrphans cur p >>= \case
    Nothing -> pure mempty
    Just r -> setFromList . toList <$> applyResidual r

-- | Construct the prefix, then link each output of its first witness to the
-- target with an empty label. Returns the created nodes.
constructTag :: (Constructing r) => NID -> P.Path -> NID -> Eff r (Set NID)
constructTag cur prefix target = do
  planned <- planConstruction False cur prefix
  created <- maybe (pure mempty) applyResidual planned
  rs <- resolveQueryWith readNode cur (compilePath prefix)
  case rs of
    r : _ | w : _ <- witnesses r ->
      for_ (ordNub (map endOf (concreteRoutes r w))) \source ->
        insertEdge @Text (Edge source "" target)
    _ -> throwError $ OtherError "The tag prefix has no complete match."
  pure (setFromList (toList created))

-- * Editing selected edges

-- | The concrete versions of route 0 over every alternative.
selectedConcreteRoutes :: [Resolution] -> [ConcreteRoute]
selectedConcreteRoutes = ordNub . concatMap selectedRoutes

lastEdgeOf :: ConcreteRoute -> Maybe (Edge Text, ConcreteHop)
lastEdgeOf r = case reverse r.hops of
  h : before : _ -> Just (concreteEdge before.target h, h)
  [h] -> Just (concreteEdge r.root h, h)
  [] -> Nothing

-- | Rename (or clone) the last edge of the single selected route to the
-- destination: its prefix must exist, and its final literal label may be
-- new. Unless 'AllParents' is given, the prefix must resolve to one parent
-- node. Everything is planned before the original edge is deleted.
relabelPath ::
  (PathReading r, WriteGraph Text :> r, Error UserError :> r) =>
  Bool ->
  DestinationParents ->
  NID ->
  P.Path ->
  P.Path ->
  Eff r ()
relabelPath removeOriginal parents cur source destination = do
  rs <- resolveCommandQuery cur source
  let command = if removeOriginal then "rn" else "cl"
  selected <- case selectedConcreteRoutes rs of
    [single] -> pure single
    routes ->
      throwError . OtherError $
        command
          <> " needs one selected source route, but found "
          <> tshow (length routes)
          <> "."
  (original, hop) <-
    lastEdgeOf selected
      `onNothing` throwError
        ( OtherError $
            command
              <> " selected a zero-edge route. Put the branch containing "
              <> "the edge first; nothing was changed."
        )
  edges <- prospectiveEdges cur destination hop.target
  when (null edges) $
    throwError . OtherError $
      command
        <> " destination has no matching parent or final label; nothing "
        <> "was changed."
  when (parents == OneParent && length edges > 1) $
    throwError . OtherError $
      renderSeveralParents command (keys edges)
  when removeOriginal $ deleteEdge @Text original
  traverse_ (insertEdge @Text) edges

renderSeveralParents :: Text -> [NID] -> Text
renderSeveralParents command ps =
  "The "
    <> command
    <> " destination's path before its final label resolves to "
    <> tshow (length ps)
    <> " parent nodes ("
    <> intercalate ", " (map tshow ps)
    <> "); nothing was changed. Pin one with /@N before the final label, "
    <> "or use "
    <> command
    <> " --all-parents to create the edge under each. (SeveralParents)"

-- | Resolve @destination/\@endpoint@ where the last hop of route 0 need not
-- exist yet: drop that pattern edge and connect the node before it. Returns
-- the edge to create keyed by that parent node.
prospectiveEdges ::
  (PathReading r, Error UserError :> r) =>
  NID ->
  P.Path ->
  NID ->
  Eff r (Map NID (Edge Text))
prospectiveEdges cur destination endpoint = do
  let q = compilePath (destination P.:/ P.Absolute endpoint)
  perAlternative <- for (toList q.alternatives) $ \p -> do
    let r0 :| rest = p.routes
    lastHop <-
      lastMay r0.hops
        `onNothing` throwError (OtherError "The destination has no final label.")
    label <- case lastHop.matcher of
      MLiteral l -> pure l
      _ ->
        throwError $
          OtherError "The destination must end in a literal label."
    let dropLast rt = case lastMay rt.hops of
          Just h | h.edge == lastHop.edge -> Route rt.root (initEx rt.hops)
          _ -> rt
        truncated = PatternPath p.tests (fmap dropLast (r0 :| rest))
    rs <- resolveQueryWith readNode cur (PathQuery (singletonSet truncated))
    pure $
      mapFromList
        [ ( parent,
            concreteEdge parent (ConcreteHop lastHop.direction label endpoint)
          )
        | rt <- selectedConcreteRoutes rs,
          let parent = endOf rt
        ]
  case ordNub (filter (not . null) perAlternative) of
    [] -> pure mempty
    [edges] -> pure edges
    _ ->
      throwError . OtherError $
        "The destination matches through several alternatives; choose one."
          <> " Nothing was changed."

-- | Relocate the traversal parent of each selected last edge. For an
-- incoming traversal this is the edge's target in the stored graph.
relocateSelectedEdges ::
  (PathReading r, WriteGraph Text :> r, Error UserError :> r) =>
  Bool ->
  NID ->
  NID ->
  P.Path ->
  Eff r ()
relocateSelectedEdges removeOriginal cur destination source = do
  rs <- resolveCommandQuery cur source
  let routes = selectedConcreteRoutes rs
  moves <- for routes \rt -> case lastEdgeOf rt of
    Nothing ->
      throwError . OtherError $
        (if removeOriginal then "mv" else "cp")
          <> " selected a zero-edge route. Put the edge branch first; "
          <> "nothing was changed."
    Just (original, hop) ->
      pure
        ( original,
          case hop.direction of
            Out -> Edge destination original.transition original.sink
            In -> Edge original.source original.transition destination
        )
  when removeOriginal $ traverse_ (deleteEdge @Text . fst) moves
  traverse_ (insertEdge @Text . snd) moves

moveSelectedEdges ::
  (PathReading r, WriteGraph Text :> r, Error UserError :> r) =>
  NID -> NID -> P.Path -> Eff r ()
moveSelectedEdges = relocateSelectedEdges True

copySelectedEdges ::
  (PathReading r, WriteGraph Text :> r, Error UserError :> r) =>
  NID -> NID -> P.Path -> Eff r ()
copySelectedEdges = relocateSelectedEdges False

-- | Delete the last edge of every selected route.
removeSelectedEdges ::
  (PathReading r, WriteGraph Text :> r, Error UserError :> r) =>
  NID ->
  P.Path ->
  Eff r ()
removeSelectedEdges cur p = do
  rs <- resolveCommandQuery cur p
  edges <- for (selectedConcreteRoutes rs) \rt -> case lastEdgeOf rt of
    Nothing ->
      throwError . OtherError $
        "rm selected a zero-edge route. Put the edge branch first; "
          <> "nothing was changed."
    Just (original, _) -> pure original
  traverse_ (deleteEdge @Text) edges

-- | Labels that continue a query prefix to at least one match.
completionTransitions :: (PathReading r) => NID -> P.Path -> Eff r (Set Text)
completionTransitions cur prefix = do
  rs <- resolveQueryWith readNode cur (compilePath prefix)
  nodes <- catMaybes <$> traverse readNode (toList (foldMap targets rs))
  let candidates =
        foldMap (foldMap (singletonSet . (.transition)) . (.outgoing)) nodes ::
          Set Text
  setFromList
    <$> filterM
      (\l -> not . null <$> queryTargets cur (prefix P.:/ P.Literal l))
      (toList candidates)
