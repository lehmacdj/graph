-- | The lines that 'suggestPaths' gives are right: running @mk@ with a
-- line creates exactly the possibility the line stands beside
-- (docs/path-route-ir.md section 5, Suggested paths).
module Models.PathPattern.SuggestSpec where

import Models.Graph (Graph)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern.Compile (checkCopies, compilePath)
import Models.PathPattern.Residuate
import Models.PathPattern.ResiduateSpec (genRepairCase)
import Models.PathPattern.Suggest
import Models.PathPattern.TestCorpus
import MyPrelude
import Utils.Testing

-- | The possibilities of an ambiguous path, if it is one within the limit.
ambiguousIn ::
  P.RepairsLimit -> Graph Text () -> P.Path -> Maybe (NonEmpty Residual)
ambiguousIn limit g p =
  case runIdentity
    ( residuateWithin
        limit
        (pure . (corpusNode g))
        noLabels
        corpusCurrent
        (compilePath p)
    ) of
    Right (Repairs rs@(_ :| _ : _)) -> Just rs
    _ -> Nothing

-- | Why a line is wrong for a possibility, if it is: it cannot be typed,
-- no command runs it, or @mk@ with it does not create exactly the
-- possibility.
wrongLine :: Graph Text () -> Residual -> P.Path -> Maybe String
wrongLine g r line
  | parseSurfacePath (renderSurfacePath line) /= Just line = Just "does not parse back"
  | isLeft (checkCopies line) = Just "is refused"
  | otherwise =
      case residuatePure (corpusNode g) (const Nothing) corpusCurrent (compilePath line) of
        Right (Repairs (r' :| []))
          | patchSubsumes r'.patch r.patch && patchSubsumes r.patch r'.patch ->
              Nothing
        other -> Just ("creates something else: " <> show other)

-- | The first five possibilities with their lines, as @mk@ shows them.
shownLines ::
  Graph Text () -> P.Path -> NonEmpty Residual -> [(Residual, Maybe P.Path)]
shownLines g p rs =
  zip shown . runIdentity $
    suggestPaths
      (P.AtMostRepairs defaultMaxRepairs)
      (pure . (corpusNode g))
      noLabels
      corpusCurrent
      p
      shown
  where
    shown = take 5 (toList rs)

spec_suggestedLinesAreRight :: Spec
spec_suggestedLinesAreRight = describe "a line that mk shows is right" do
  let allRight g p = case ambiguousIn (P.AtMostRepairs defaultMaxRepairs) g p of
        Nothing -> expectationFailure ("not ambiguous: " <> show p)
        Just rs ->
          [ (renderSurfacePath line, wrong)
          | (r, Just line) <- shownLines g p rs,
            Just wrong <- [wrongLine g r line]
          ]
            `shouldBe` []
      bare = graphWithEdges [edge' 1 "zz" 9]
  it "where an alternative has several of the possibilities" do
    allRight
      (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3])
      [path| a/b ++ a/c ++ d |]
  it "where a union is copied into conjuncts" $
    allRight bare [path| (a | b)/(c & d) |]
  it "where a step is copied and its copies go the same way" $
    allRight
      (graphWithEdges [edge' 1 "a" 2, edge' 1 "a" 3, edge' 2 "b" 4, edge' 3 "b" 5])
      [path| a/b/(c & d) |]
  it "on random ambiguous paths and graphs" . withMaxSuccess 500 $
    -- few random paths are ambiguous, now that mk refuses the shapes in
    -- which steps could follow each other's edges: they are looked for
    let ambiguous (p, g) =
          fewAlternatives 1000 p && isJust (ambiguousIn (P.AtMostRepairs 150) g p)
     in forAll (genRepairCase `suchThat` ambiguous) \(p, g) ->
          case ambiguousIn (P.AtMostRepairs 150) g p of
            Nothing -> discard
            Just rs ->
              conjoin
                [ counterexample (unpack (renderSurfacePath line) <> " " <> wrong) False
                | (r, Just line) <- shownLines g p rs,
                  Just wrong <- [wrongLine g r line]
                ]
  it "most listed possibilities of random paths get a line" do
    cases <- generate (vectorOf 3000 genRepairCase)
    let shown =
          [ isJust line
          | (p, g) <- cases,
            fewAlternatives 1000 p,
            Just rs <- [ambiguousIn (P.AtMostRepairs 150) g p],
            (_, line) <- shownLines g p rs
          ]
    (length (filter id shown) * 100 `div` max 1 (length shown))
      `shouldSatisfy` (>= 50)
