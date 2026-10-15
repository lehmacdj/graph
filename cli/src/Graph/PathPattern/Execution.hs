-- | Command policy on top of route patterns (docs/path-route-ir.md,
-- Selection and Residuation). Queries append @/\@@ before compiling, so that every output meets
-- at one node; construction does not. The engine itself adds no defaults.
module Graph.PathPattern.Execution
  ( PathReading,
    readNode,
    queryPath,
    resolveCommandQuery,
    queryTargets,
    orderedQueryTargets,
    Constructed (..),
    AsTyped (..),
    plainlyTyped,
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
import Data.Map.Strict qualified as Map
import Error.UserError
import Error.Warn (Warn, warnText)
import Graph.FreshNID
import Graph.GraphMetadataEditing
import Graph.PathPattern.Resolve (resolveQueryWith)
import Models.Command (DestinationParents (..), MaxRepairs)
import Models.Connect (Connect' (..))
import Models.Edge
import Models.NID
import Models.Node
import Models.Path.Simple qualified as P
import Models.PathPattern
import Models.PathPattern.Admit (Shrinking (..), checkExpansion, expansionLimit, refusalCode, refusals, shrinking)
import Models.PathPattern.Compile (checkCopies, compilePath)
import Models.PathPattern.Match
import Models.PathPattern.Residuate
import Models.PathPattern.Suggest
  ( Written (..),
    alternativeChoices,
    joinAfter,
    reassociate,
    renderSurfacePath,
    stepAsTyped,
    suggestPaths,
    typedTimes,
    writtenSteps,
  )
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
    Warn UserError :> r,
    Error UserError :> r
  )

-- | Work out what a path creates, changing nothing. A path of a refused
-- shape is refused first, whatever the graph holds. If steps have several
-- edges to follow, the error lists the possibilities, each with the command
-- that creates just it (from the second argument), where one can be
-- written. The first argument is the command's name, for what is said.
-- Also returns a note for each side of a union that was left out because
-- it needs a written node that does not exist. The path, and each line
-- listed, is walked within the limit of the command's option
-- @--max-repairs@ (the second argument): a listed command is the
-- command with only its path replaced, so it keeps the option, and is
-- walked as it would be when it is typed.
planConstruction ::
  (PathReading r, Error UserError :> r) =>
  Text ->
  MaxRepairs ->
  AsTyped ->
  (P.Path -> Text) ->
  NID ->
  P.Path ->
  Eff r (Maybe Residual, [Text])
planConstruction name maxRepairs asTyped command cur p = do
  -- before the path is compiled, and before the graph is looked at
  for_ (checkExpansion p) (throwError . OtherError . renderRefusal name asTyped cur p)
  -- Nothing is written while the path and the listed lines are walked, so
  -- each node is read once.
  (leftOut, result, lines') <- flip S.evalStateT mempty do
    (leftOut, result) <-
      residuateNoting limit readOnce noLabels cur (compilePath p)
    lines' <- case chooseRepair <$> result of
      Right (Ambiguous rs) ->
        suggestPaths limit readOnce noLabels cur p $
          take shownPossibilities (toList rs)
      _ -> pure []
    pure (leftOut, result, lines')
  let notes = leftOutNotes name p leftOut
  residuation <- case result of
    Right residuation -> pure residuation
    -- this error names the nodes itself
    Left e@(MissingNodes _) -> failing [] (renderResidualError name asTyped cur p e)
    Left e -> failing notes (renderResidualError name asTyped cur p e)
  case chooseRepair residuation of
    NoChange -> pure (Nothing, notes)
    Impossible -> failing notes "This path cannot be constructed."
    Ambiguous rs ->
      failing notes (renderChoiceError (map (fmap command) lines') rs)
    Apply r -> pure (Just r, notes)
  where
    limit = fromMaybe (P.AtMostRepairs defaultMaxRepairs) maxRepairs

-- | The option @--max-repairs@ as it is typed, followed by a space, if it
-- was given.
renderMaxRepairs :: MaxRepairs -> Text
renderMaxRepairs = \case
  Nothing -> ""
  Just (P.AtMostRepairs n) -> "--max-repairs " <> tshow n <> " "
  Just P.UnlimitedRepairs -> "--max-repairs unlimited "

-- | Fail with a message, and the notes after it.
failing :: (Error UserError :> r) => [Text] -> Text -> Eff r a
failing notes message =
  throwError (OtherError (intercalate "\n" (stripSuffix' message : notes)))
  where
    -- a message that ends its last line already
    stripSuffix' t = if null notes then t else fromMaybe t (stripSuffix "\n" t)

-- | A note for each alternative that was not built because it needs a
-- written node that does not exist: the path with the sides of its unions
-- that give the alternative, and the nodes.
leftOutNotes :: Text -> P.Path -> [LeftOut] -> [Text]
leftOutNotes name p leftOut =
  [ "Note: "
      <> name
      <> " left out "
      <> maybe "one side of a union" (\side -> "`" <> renderSurfacePath side <> "`") (lookup l.alternative sides)
      <> ", which needs "
      <> intercalate ", " (map tshow (toList l.missing))
      <> (if length l.missing > 1 then ": these nodes do not exist" else ": that node does not exist")
      <> ", and a node with a written id is never created."
  | l <- leftOut
  ]
  where
    sides = alternativeChoices p (setFromList (map (.alternative) leftOut))

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

-- | What a construction says when it fails: the arguments are the
-- command's name, what the path had as typed, the node it builds from and
-- the path as written.
renderResidualError :: Text -> AsTyped -> NID -> P.Path -> ResidualError -> Text
renderResidualError name asTyped cur p = \case
  Refused refusal -> renderRefusal name asTyped cur p refusal
  MissingNodes ns ->
    "This path needs "
      <> intercalate ", " (map tshow (toList ns))
      <> (if length ns > 1 then ", which do not exist" else ", which does not exist")
      <> ", and a node with a written id is never created: "
      <> name
      <> " gives every new node a new id. Nothing was changed. (MissingNodes)"
  TooManyRepairs n ->
    "Working out what this path creates takes too long: its steps have "
      <> "several edges to follow, and every combination of the edges "
      <> "followed has to be worked out, over all alternatives of the path; "
      <> "there are more combinations than the limit of "
      <> tshow n
      <> ". This does not mean that the path cannot be created. Nothing was "
      <> "changed. Write @N after a step for the node it should go to, or "
      <> "!{@N} for nodes it should not, so that fewer edges can be "
      <> "followed, or raise the limit by giving "
      <> name
      <> " the option --max-repairs N before the path, with N a larger "
      <> "number or unlimited. (TooManyRepairs)"
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
      <> (if length needing > 1 then "possibilities " else "possibility ")
      <> intercalate ", " (map tshow needing)
      <> " below), and even with one this path can be created in "
      <> tshow (length rs)
      <> " ways. "
      <> name
      <> " does not choose one that needs no label; nothing was "
      <> "changed. Give the edge a literal label, or write @N after a step "
      <> "for the node it should go to, or !{@N} for nodes it should not, or "
      <> "keep one side of a +. (UnsupportedLabel)\n"
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

-- | What the path as typed had that it no longer shows once its directives
-- are expanded.
data AsTyped = AsTyped
  { -- | an @%override(…)@, whose setting for @+@ leaves nothing in it
    override :: Bool,
    -- | where the path as expanded has what each @%targets(…)@ became:
    -- a union of the nodes it finds (@%never ++ \@N ++ …@), which was not
    -- typed
    targets :: [P.Place],
    -- | a @+@ that an @%override({plus = relational}, …)@ around it makes
    -- a @|@
    relationalPlus :: Bool
  }

-- | A path with no directive.
plainlyTyped :: AsTyped
plainlyTyped = AsTyped False [] False

-- | Whether the union at a place of the path as expanded is one that the
-- path as typed has: not one of what a @%targets(…)@ became, which is
-- told by where it is, not by what it looks like (a union of nodes can be
-- typed too).
typedUnion :: AsTyped -> P.Place -> Bool
typedUnion asTyped place = not (any (`isPrefixOf` place) asTyped.targets)

-- | Why a path of a refused shape is not built, with what clashes quoted
-- as it was written, how to write what was probably meant, and the raw
-- code. The arguments are the command's name, what the path had as typed,
-- the node it builds from and the path as written. Nothing was looked at
-- but the path, so the refusal says nothing about the graph.
--
-- A step that the product rule copies is written once, though its
-- alternative has a step for each copy: a message names it once (and
-- says which of the steps typed the same it is, if there are several),
-- says that it is copied ('writtenSteps'), and does not advise leaving
-- out one of its copies. Each rewrite a message suggests is one that does
-- what it says: a join after a copied step only with the user's own path,
-- when that path is built and the join holds together only copies that
-- came the same way; moving the unions that were typed out, or keeping
-- one side of each, only where the estimate is then within the limit (for
-- every way to keep one).
renderRefusal :: Text -> AsTyped -> NID -> P.Path -> Refusal -> Text
renderRefusal name asTyped cur surface refusal =
  body <> " Nothing was changed. (" <> refusalCode refusal <> ")"
  where
    shown q = "`" <> renderSurfacePath q <> "`"
    listed = \case
      [] -> ""
      [x] -> x
      xs -> intercalate ", " (initEx xs) <> " and " <> lastEx xs
    -- choices to make: "x, y, or z"
    either' = \case
      [x] -> x
      [x, y] -> x <> " or " <> y
      xs -> intercalate ", " (initEx xs) <> ", or " <> lastEx xs
    capitalised :: Text -> Text
    capitalised t = toUpper (take 1 t) <> drop 1 t
    request (Request steps) = case steps of
      [step] -> shown step.route
      _ -> "the node where " <> listed (map (shown . (.route)) steps) <> " meet"

    -- the written step that a step of an alternative is a copy of
    indexIn :: Maybe Written -> Step -> Maybe Int
    indexIn written s = written >>= \w -> lookup s.edge w.copyOf
    -- a written step, quoted as typed
    writtenStep :: Maybe Written -> Int -> Text
    writtenStep written i = "`" <> maybe "" (`stepAsTyped` i) written <> "`"
    -- how many steps of the alternative are copies of a written step
    copiesOf :: Maybe Written -> Int -> Int
    copiesOf written i =
      maybe 0 (\w -> length (filter (== i) (toList w.copyOf))) written
    -- that a written step is copied: "The step `S` is written once, but
    -- the product rule copies it into 3 walks", with what comes between
    -- the step and the verb
    copiedStep :: Text -> Maybe Written -> Int -> Text
    copiedStep between written i =
      "The step "
        <> writtenStep written i
        <> between
        <> case maybe (1, 1) (`typedTimes` i) written of
          (1, _) -> " is written once, but the product rule copies it into "
          (n, k) ->
            " is written "
              <> times n
              <> " in this path; the product rule copies the "
              <> ordinal k
              <> " of them into "
        <> tshow (copiesOf written i)
        <> " walks"
    -- the same, inside a list: "`S` (written once, and the product rule
    -- copies it into 3 walks"
    copiedInList :: Maybe Written -> Int -> Text
    copiedInList written i =
      writtenStep written i
        <> " ("
        <> case maybe (1, 1) (`typedTimes` i) written of
          (1, _) -> "written once"
          (n, k) -> "the " <> ordinal k <> " of the " <> tshow n <> " written"
        <> ", and the product rule copies it into "
        <> tshow (copiesOf written i)
        <> " walks"
    times = \case
      2 -> "twice"
      n -> tshow n <> " times"
    ordinal :: Int -> Text
    ordinal k = case k of
      1 -> "first"
      2 -> "second"
      3 -> "third"
      4 -> "fourth"
      5 -> "fifth"
      6 -> "sixth"
      7 -> "seventh"
      8 -> "eighth"
      9 -> "ninth"
      10 -> "tenth"
      _
        | (k `mod` 100) `elem` [11, 12, 13] -> tshow k <> "th"
        | k `mod` 10 == 1 -> tshow k <> "st"
        | k `mod` 10 == 2 -> tshow k <> "nd"
        | k `mod` 10 == 3 -> tshow k <> "rd"
        | otherwise -> tshow k <> "th"
    -- quotes of copies, each different one once, with how many copies it
    -- quotes: `b` twice and `b/!{@0}`
    copyQuotes :: [Text] -> Text
    copyQuotes quotes =
      listed
        [ q <> case length (filter (== q) quotes) of
            1 -> ""
            n -> " " <> times n
        | q <- ordNub quotes
        ]
    -- the copies of a step that something names, among all its copies:
    -- "its copies `x` and `y`", "its copies, `b` twice and `c`," or "2 of
    -- its copies, `x` and `y`,"
    theCopies :: Int -> [Text] -> Text
    theCopies total quotes
      | length quotes == total && length (ordNub quotes) == total =
          "its copies " <> listed quotes
      | length quotes == total = "its copies, " <> copyQuotes quotes <> ","
      | otherwise =
          tshow (length quotes) <> " of its copies, " <> copyQuotes quotes <> ","
    -- the steps grouped by the written step they are copies of, in the
    -- order they come; a step that cannot be lined up with the path as
    -- written is a group of its own
    grouped :: Maybe Written -> [Step] -> [(Maybe Int, [Step])]
    grouped written steps =
      [ (either (const Nothing) Just k, filter ((== k) . key) steps)
      | k <- ordNub (map key steps)
      ]
      where
        key s = maybe (Left s.edge) Right (indexIn written s)
    -- the path with a join after a written step: only when the path of the
    -- alternative is the whole path as written (no other side of a union
    -- would be left out, and no %override(…) was typed around it), when
    -- every copy of the step came the same way (from the same node, the
    -- last one before it that is one node in every walk, through copies
    -- of the same written steps: a join would otherwise make walks meet
    -- that were kept apart, as the copies of b in (p & q)/b/(c & d), but
    -- not in (p & q)/@/b/(c & d)), and when the path with the join is one that a
    -- command runs (it copies no meeting point) and that mk builds
    joined :: PatternPath -> Maybe Written -> Int -> Maybe P.Path
    joined alternative written i = do
      w <- written
      guard (not asTyped.override)
      guard (renderSurfacePath (reassociate w.path) == renderSurfacePath (reassociate surface))
      let -- the steps into each vertex
          into = Map.fromListWith (+) [(e.target, 1 :: Int) | e <- toList (patternEdges alternative)]
          -- a vertex that is one node in every walk: a written node, the
          -- node built from, or one where steps meet (a join)
          oneNode = \case
            v@(Var _) -> findWithDefault 0 v into > 1
            _ -> True
          -- the way a copy came: from the last such node before it,
          -- through copies of which written steps
          wayOf root before =
            let starts = zip (root : map (.target) before) (inits before <&> length)
                (start, k) = fromMaybe (root, 0) (lastMay (filter (oneNode . fst) starts))
             in (start, map (\b -> lookup b.edge w.copyOf) (drop k before))
          ways =
            ordNub
              [ wayOf r.root before
              | r <- toList alternative.routes,
                (before, h) <- zip (inits r.hops) r.hops,
                lookup h.edge w.copyOf == Just i
              ]
      guard (length ways == 1)
      let q = joinAfter i w.path
      guard (isRight (checkCopies q))
      guard (isNothing (checkExpansion q) && null (refusals cur (compilePath q)))
      pure q
    joinAdvice q =
      "If the walks are to go on from one edge of it, write a join after it: "
        <> renderSurfacePath q
        <> ". Otherwise "

    body = case refusal of
      ExpansionTooLarge _ ->
        let shrink = shrinking (typedUnion asTyped) surface
         in "This path is too large for "
              <> name
              <> ": its size, estimated from the path as written, is more "
              <> "than the limit of "
              <> tshow expansionLimit
              <> " route steps over all its alternatives. "
              <> ( if shrink.movedOut
                     then
                       if asTyped.relationalPlus
                         then
                           -- a + made | is not moved out: its copies can
                           -- take different sides, and moved out they
                           -- could not
                           "The estimate counts a union that the product rule "
                             <> "copies as if each copy took a side of its own. "
                             <> "The copies of a ++ union all take the same "
                             <> "side, and so do those of a + that "
                             <> name
                             <> " reads as ++, but not those of a + that an "
                             <> "%override({plus = relational}, …) makes |. "
                             <> "Written with its ++ unions and the + that "
                             <> name
                             <> " reads as ++ outside the rest of the path, as "
                             <> "in a/(c & d) ++ b/(c & d) for (a ++ b)/(c & d), "
                             <> "the path means the same and its estimate is "
                             <> "within the limit. "
                         else
                           "The estimate counts a + or ++ union that the "
                             <> "product rule copies as if each copy took a "
                             <> "side of its own, though the copies all take "
                             <> "the same side. Written with its + and ++ "
                             <> "unions outside the rest of the path, as in "
                             <> "a/(c & d) ++ b/(c & d) for (a ++ b)/(c & d), "
                             <> "the path means the same and its estimate is "
                             <> "within the limit. "
                     else ""
                 )
              <> capitalised
                ( either'
                    ( ["write it so" | shrink.movedOut]
                        <> ["keep one side of each union" | shrink.oneSide]
                        <> ["create the path with several commands"]
                    )
                )
              <> "."
      ReturnsToMeetingPoint before again ->
        "This path leaves an unnamed node and comes back to it: "
          <> shown again
          <> " returns to the node that "
          <> shown before
          <> " reaches, and "
          <> name
          <> " does not build a route that visits an unnamed node twice. "
          <> "Write a node that exists there both times, as in a/@N/a/@N, "
          <> "or create the node first and use its id in a second command."
      WildcardJoin alternative wild others ->
        let written = writtenSteps surface alternative
            copies = case indexIn written wild of
              Just i -> [o | o <- others, indexIn written o == Just i]
              Nothing -> []
            rest = [o | o <- others, o `notElem` copies]
         in case indexIn written wild of
              Just i
                | not (null copies) ->
                    copiedStep " has no label and" written i
                      <> ", and "
                      <> theCopies
                        (copiesOf written i)
                        (map (shown . (.route)) (wild : copies))
                      <> " meet"
                      <> ( if null rest
                             then ""
                             else " " <> listed (map (shown . (.route)) rest)
                         )
                      <> " at a node that is not written. "
                      <> name
                      <> " does not use a step without a label to choose "
                      <> "such a node. Write its label."
              _ ->
                "The step "
                  <> shown wild.route
                  <> " has no label and meets "
                  <> listed (map (shown . (.route)) others)
                  <> " at a node that is not written, and "
                  <> name
                  <> " does not use a step without a label to choose such a "
                  <> "node. If it means the same edge as another step into "
                  <> "the node, leave it out; otherwise write its label, or "
                  <> "write the node where the steps meet (/@N)."
      SeveralWildcards alternative steps ->
        let written = writtenSteps surface alternative
            specs = patternEdges alternative
            start s = lookup s.edge specs <&> \e -> (e.source, e.direction, e.matcher)
            describe = \case
              (Just i, _ : _ : _) ->
                writtenStep written i
                  <> " (copied into "
                  <> tshow (copiesOf written i)
                  <> " walks)"
              (_, step : _) -> shown step.route
              (_, []) -> ""
         in case grouped written steps of
              [(Just i, _)] ->
                copiedStep "" written i
                  <> ", so one alternative of this path has "
                  <> tshow (copiesOf written i)
                  <> " * or regex steps, and "
                  <> name
                  <> " builds a path with at most one. "
                  <> maybe "" joinAdvice (joined alternative written i)
                  <> (if isJust (joined alternative written i) then "write" else "Write")
                  <> " the labels you mean."
              groups ->
                let firsts = mapMaybe (start <=< (headMay . snd)) groups
                    couldBeOne =
                      or [a == b | (k, a) <- zip [1 :: Int ..] firsts, b <- drop k firsts]
                 in "This path has more than one * or regex step in one "
                      <> "alternative: "
                      <> listed (map describe groups)
                      <> ". "
                      <> name
                      <> " builds a path with at most one. Write the labels "
                      <> "you mean"
                      <> ( if couldBeOne
                             then
                               "; if steps from one node are meant to be "
                                 <> "one step, write it once, with a join "
                                 <> "after it for what continues from it, as "
                                 <> "in */@/(a & b) for */a & */b"
                             else ""
                         )
                      <> "."
      CircularSuppliers alternative requests ->
        let written = writtenSteps surface alternative
            single = \case
              Request [s] -> indexIn written s
              _ -> Nothing
            -- the written steps that several of the steps are copies of
            copied =
              [ i
              | (i, n) <- mapToList (Map.fromListWith (+) [(i, 1 :: Int) | Just i <- map single requests]),
                n > 1
              ]
            isCopy r = maybe False (`elem` copied) (single r)
            -- each request, but the copies of a written step once
            described =
              [ case single r of
                  Just i
                    | i `elem` copied ->
                        let copies = [request r' | r' <- requests, single r' == Just i]
                         in copiedInList written i
                              <> ( if length copies == copiesOf written i
                                     then ": "
                                     else ", among them "
                                 )
                              <> copyQuotes copies
                              <> ")"
                  _ -> request r
              | (k, r) <- zip [0 :: Int ..] requests,
                not (isCopy r) || all ((/= single r) . single) (take k requests)
              ]
         in case copied of
              [i]
                | all isCopy requests ->
                    copiedStep "" written i
                      <> ", and "
                      <> theCopies (copiesOf written i) (map request requests)
                      <> " could each follow an edge that another of them "
                      <> "creates, so "
                      <> name
                      <> " has no order to take them in. "
                      <> maybe "" joinAdvice (joined alternative written i)
                      <> (if isJust (joined alternative written i) then "write" else "Write")
                      <> " the node it should go to after it (/@N), or create "
                      <> "the walks with separate commands."
              _ ->
                let joins = any ((> 1) . length . (.steps)) requests
                    lone = any ((== 1) . length . (.steps)) requests
                 in "These steps could each follow an edge that another of "
                      <> "them creates, so "
                      <> name
                      <> " has no order to take them in: "
                      <> listed described
                      <> ". "
                      <> ( if joins && lone && null copied
                             then
                               "If a step means the same edge as a step into "
                                 <> "a node where steps meet, leave it out and "
                                 <> "write what follows it after that node, as "
                                 <> "in (a & c)/@/b for a/b & (a & c)/@; "
                                 <> "otherwise write"
                             else "Write"
                         )
                      <> " the node a step should go to (/@N after it), or create "
                      <> "them with separate commands."

-- | The possibilities of an ambiguous construction, each with the command
-- that creates just it where one could be written (the commands are in the
-- order of the possibilities).
renderChoiceError :: [Maybe Text] -> NonEmpty Residual -> Text
renderChoiceError commands rs =
  "This path can be created in "
    <> tshow (length rs)
    <> " ways; nothing was changed. To choose one, write @N after a step "
    <> "for the node it should go to, or !{@N} for nodes it should not, or "
    <> "keep one side of a +.\n"
    <> renderNumbered render (NE.zip rs (NE.fromList padded))
  where
    -- one per possibility; the list covers at most the shown ones
    padded = take (length rs) (commands <> replicate (length rs) Nothing)
    render (r, command) =
      renderResidual r <> "\n    " <> maybe unwritten ("use: " <>) command
    unwritten = "(no command was found that creates just this)"

-- | The first 'shownPossibilities' items, numbered, and how many more there
-- are.
renderNumbered :: (a -> Text) -> NonEmpty a -> Text
renderNumbered render rs =
  unlines $
    [ "  " <> tshow i <> ". " <> render r
    | (i, r) <- zip [1 :: Int ..] (take shownPossibilities (toList rs))
    ]
      <> [ "  ... and " <> tshow hidden <> " more"
         | let hidden = length rs - shownPossibilities,
           hidden > 0
         ]

shownPossibilities :: Int
shownPossibilities = 5

-- | Create the nodes and edges of a residual. Returns the binding of every
-- created node.
applyResidual ::
  (GraphMetadataEditing :> r, FreshNID :> r) =>
  Residual ->
  Eff r (Map Binding NID)
applyResidual r = do
  -- only new nodes are created: a node with a written id never is
  created <- for [b | b@(Fresh _) <- toList r.patch.nodes] \b -> (b,) <$> freshNID
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

-- | What a construction created: the new nodes, and how many edges.
data Constructed = Constructed {nodes :: Set NID, edges :: Int}

-- | Create what the path creates, if that is decided. The arguments are the
-- command's option @--max-repairs@; the prompt's location, for the
-- commands listed when it is not; the node it builds from; and what the
-- path had as typed, for what a refusal suggests.
constructPath ::
  (Constructing r) =>
  MaxRepairs ->
  NID ->
  NID ->
  AsTyped ->
  P.Path ->
  Eff r Constructed
constructPath maxRepairs prompt cur asTyped p = do
  (planned, notes) <- planConstruction "mk" maxRepairs asTyped command cur p
  traverse_ warnText notes
  case planned of
    Nothing -> pure (Constructed mempty 0)
    Just r -> do
      created <- applyResidual r
      pure (Constructed (setFromList (toList created)) (length r.patch.edges))
  where
    command q =
      atLocation prompt cur $
        "mk " <> renderMaxRepairs maxRepairs <> renderSurfacePath q

-- | Construct the prefix, then link each output of its first witness to the
-- target with an empty label. Returns the created nodes. The arguments
-- before the prefix are as for 'constructPath'.
constructTag ::
  (Constructing r) =>
  MaxRepairs ->
  NID ->
  NID ->
  AsTyped ->
  P.Path ->
  NID ->
  Eff r (Set NID)
constructTag maxRepairs prompt cur asTyped prefix target = do
  (planned, notes) <-
    planConstruction "tag" maxRepairs asTyped command cur prefix
  traverse_ warnText notes
  created <- maybe (pure mempty) applyResidual planned
  rs <- resolveQueryWith readNode cur (compilePath prefix)
  case rs of
    r : _ | w : _ <- witnesses r ->
      for_ (ordNub (map endOf (concreteRoutes r w))) \source ->
        insertEdge @Text (Edge source "" target)
    _ -> throwError $ OtherError "The tag prefix has no complete match."
  pure (setFromList (toList created))
  where
    command q =
      let t = renderSurfacePath q
          grouped = if ' ' `elem` t then "(" <> t <> ")" else t
       in atLocation prompt cur $
            "tag "
              <> renderMaxRepairs maxRepairs
              <> grouped
              <> " "
              <> tshow target

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
