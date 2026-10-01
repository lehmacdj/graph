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

import Control.Monad.State.Strict qualified as S
import Data.List.NonEmpty qualified as NE
import Error.UserError
import Graph.FreshNID
import Graph.GraphMetadataEditing
import Graph.PathPattern.Resolve (resolveQueryWith)
import Models.Command (DestinationParents (..))
import Models.Connect (Connect' (..))
import Models.Edge
import Models.NID
import Models.Node
import Models.Path.Simple qualified as P
import Models.PathPattern
import Models.PathPattern.Compile (compilePath)
import Models.PathPattern.Match
import Models.PathPattern.Residuate
import Models.PathPattern.Suggest (renderSurfacePath, suggestPaths)
import MyPrelude

type PathReading r = (GraphMetadataReading :> r)

readNode :: (PathReading r) => NID -> Eff r (Maybe (Node Text ()))
readNode = getNodeMetadata

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
    GraphMetadataEditing :> r,
    FreshNID :> r,
    Error UserError :> r
  )

-- | Residuate a path and choose its repair, changing nothing. If it is
-- ambiguous, the error shows, for each listed repair that a path can select,
-- the command that runs that path for that repair (from the first argument),
-- if there is one.
planConstruction ::
  (PathReading r, Error UserError :> r) =>
  (Residual -> P.Path -> Maybe Text) ->
  Bool ->
  NID ->
  P.Path ->
  Eff r (Maybe Residual)
planConstruction command allowOrphans cur p = do
  -- Nothing is written while a repair is chosen, so each node is read
  -- once for the residuation and the suggestions together.
  (result, suggested) <- flip S.evalStateT mempty do
    result <-
      residuateWithin (pathMaxRepairs p) readOnce noLabels cur (compilePath p)
    suggested <- case chooseRepair <$> result of
      Right (Ambiguous rs) ->
        suggestPaths readOnce noLabels cur p rs (take shownRepairs (toList rs))
      _ -> pure []
    pure (result, suggested)
  residuation <- either (throwError . OtherError . renderResidualError) pure result
  case chooseRepair residuation of
    NoChange -> pure Nothing
    Impossible -> throwError $ OtherError "This path cannot be constructed."
    Ambiguous rs -> do
      let shown = take shownRepairs (toList rs)
          suggestions = zipWith (\r s -> command r =<< s) shown suggested
      throwError . OtherError $ renderChoiceError suggestions rs
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

-- | 'readNode', keeping every node read.
readOnce ::
  (PathReading r) =>
  NID ->
  S.StateT (Map NID (Maybe (Node Text ()))) (Eff r) (Maybe (Node Text ()))
readOnce n =
  S.gets (lookup n) >>= \case
    Just known -> pure known
    Nothing -> do
      node <- lift (readNode n)
      S.modify' (insertMap n node)
      pure node

renderResidualError :: ResidualError -> Text
renderResidualError = \case
  TooManyRepairs n ->
    "Working out how to construct this path would take too long: its "
      <> "steps can be matched to existing nodes, or left to be created, in "
      <> "more ways than the limit of "
      <> tshow n
      <> ", and each way has to be considered. Nothing was changed. Pin a "
      <> "step to a node with @N or exclude nodes with !{@N} so that fewer "
      <> "match, or raise the limit with %override({max-repairs = …}, …). "
      <> "(TooManyRepairs)"
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

-- | The survivors of an ambiguous construction, each with the command that
-- chooses it where one was found (the suggestions are in survivor order).
renderChoiceError :: [Maybe Text] -> NonEmpty Residual -> Text
renderChoiceError suggestions rs =
  "This path has "
    <> tshow (length rs)
    <> " possible repairs. Nothing was changed. Pin a node with @N or "
    <> "exclude one with !{@N} to choose.\n"
    <> renderNumbered render (NE.zip rs (NE.fromList padded))
  where
    -- one per repair; the list covers at most the shown ones
    padded =
      take (length rs) (suggestions <> replicate (length rs) Nothing)
    render (r, suggestion) =
      renderResidual r <> foldMap ("\n    use: " <>) suggestion

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
  (GraphMetadataEditing :> r, FreshNID :> r) =>
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

-- | A suggested command, to be typed at the prompt's location (the first
-- argument): when the command ran elsewhere (inside @at@), it is prefixed
-- with @at \@N@.
atLocation :: NID -> NID -> Text -> Text
atLocation prompt cur command
  | prompt == cur = command
  | otherwise = "at " <> tshow cur <> " " <> command

-- | Apply exactly the chosen repair of a path. Returns the created nodes.
-- The first argument is the prompt's location, for the commands suggested
-- when the path is ambiguous.
constructPath ::
  (Constructing r) => NID -> Bool -> NID -> P.Path -> Eff r (Set NID)
constructPath prompt allowOrphans cur p =
  planConstruction command allowOrphans cur p >>= \case
    Nothing -> pure mempty
    Just r -> setFromList . toList <$> applyResidual r
  where
    command r q =
      Just . atLocation prompt cur $
        ( if allowOrphans || not (null (patchOrphans r.patch))
            then "mk --allow-orphans "
            else "mk "
        )
          <> renderSurfacePath q

-- | Construct the prefix, then link each output of its first witness to the
-- target with an empty label. Returns the created nodes. The first argument
-- is the prompt's location, as for 'constructPath'.
constructTag ::
  (Constructing r) => NID -> NID -> P.Path -> NID -> Eff r (Set NID)
constructTag prompt cur prefix target = do
  planned <- planConstruction command False cur prefix
  created <- maybe (pure mempty) applyResidual planned
  rs <- resolveQueryWith readNode cur (compilePath prefix)
  case rs of
    r : _ | w : _ <- witnesses r ->
      for_ (ordNub (map endOf (concreteRoutes r w))) \source ->
        insertEdge @Text (Edge source "" target)
    _ -> throwError $ OtherError "The tag prefix has no complete match."
  pure (setFromList (toList created))
  where
    -- tag cannot create orphans, so it has no command for such a repair
    command r q
      | not (null (patchOrphans r.patch)) = Nothing
      | otherwise =
          let t = renderSurfacePath q
              grouped = if ' ' `elem` t then "(" <> t <> ")" else t
           in Just (atLocation prompt cur ("tag " <> grouped <> " " <> tshow target))

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
  (PathReading r, GraphMetadataEditing :> r, Error UserError :> r) =>
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
-- Returns the selected edges: none means the source matched nothing, so
-- nothing was done.
relocateSelectedEdges ::
  (PathReading r, GraphMetadataEditing :> r, Error UserError :> r) =>
  Bool ->
  NID ->
  NID ->
  P.Path ->
  Eff r [Edge Text]
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
  pure (map fst moves)

moveSelectedEdges ::
  (PathReading r, GraphMetadataEditing :> r, Error UserError :> r) =>
  NID -> NID -> P.Path -> Eff r [Edge Text]
moveSelectedEdges = relocateSelectedEdges True

copySelectedEdges ::
  (PathReading r, GraphMetadataEditing :> r, Error UserError :> r) =>
  NID -> NID -> P.Path -> Eff r [Edge Text]
copySelectedEdges = relocateSelectedEdges False

-- | Delete the last edge of every selected route. Returns the selected
-- edges: none means the path matched nothing, so nothing was done.
removeSelectedEdges ::
  (PathReading r, GraphMetadataEditing :> r, Error UserError :> r) =>
  NID ->
  P.Path ->
  Eff r [Edge Text]
removeSelectedEdges cur p = do
  rs <- resolveCommandQuery cur p
  edges <- for (selectedConcreteRoutes rs) \rt -> case lastEdgeOf rt of
    Nothing ->
      throwError . OtherError $
        "rm selected a zero-edge route. Put the edge branch first; "
          <> "nothing was changed."
    Just (original, _) -> pure original
  traverse_ (deleteEdge @Text) edges
  pure edges

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
