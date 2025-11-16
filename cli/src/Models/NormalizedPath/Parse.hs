module Models.NormalizedPath.Parse
  ( pNormalizedPath,
    pNormalizedPath',
  )
where

import Data.Functor
import Models.NID
import Models.NormalizedPath
import Models.Path.Parse (pSmallNID, pTransition)
import MyPrelude hiding (try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)
import Utils.Parsing

-- | Parse a normalized path term (atom) - generalized version
pNormalizedPath' :: Parser NID -> Parser (NormalizedPath Anchor)
pNormalizedPath' pNID =
  fmap (NormalizedPath . setFromList) $
    (pDeterministicPath pNID `sepBy1` symbol "+")
      <|> (symbol "%never" $> [])

-- | Parse a normalized path term (atom)
pNormalizedPath :: Parser (NormalizedPath Anchor)
pNormalizedPath = pNormalizedPath' pSmallNID

pDeterministicPath :: Parser NID -> Parser (DeterministicPath Anchor)
pDeterministicPath pNID =
  label "deterministic path" $
    try (Pointlike <$> pPointlike pNID)
      <|> (Rooted <$> pRootedPath pNID)

pSingleBranch :: Parser (DPBranch Anchor)
pSingleBranch =
  label "single branch" $
    try (symbol "~*" $> DPIncoming DPWild)
      <|> (symbol "*" $> DPOutgoing DPWild)
      <|> try (DPIncoming . DPLiteral <$> (char '~' *> pTransition))
      <|> (DPOutgoing . DPLiteral <$> pTransition)

pBranch :: Parser NID -> Parser (DPBranch Anchor)
pBranch pNID =
  label "branch" $
    try (pSequence pNID) <|> pSingleBranch

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: Parser NID -> Parser (PointlikeDeterministicPath Anchor)
pPointlike pNID = label "pointlike" do
  anchor <- pExplicitAnchor pNID
  loops <- option [] $ brackets (pBranch pNID `sepBy1` symbol "&")
  pure $ PointlikeDeterministicPath anchor (setFromList loops)

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pRootedPath :: Parser NID -> Parser (RootedDeterministicPath Anchor)
pRootedPath pNID = label "rooted path" do
  rootBranches <- impureNonNull <$> brackets (pRootBranches pNID)
  target <- option unanchored (pTarget pNID)
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: Parser NID -> Parser (DPBranch Anchor)
pSequence pNID = label "sequence" do
  (initial, splitFirst -> ((m, b), rest)) <- pSequenceBranchSet pNID `via1` pMidpoint
  pure . uncurry ($) $
    foldl'
      (\(acc, x) (n, y) -> (acc . singletonNNSet . DPSequence x n, y))
      (DPSequence initial m :: NonNull (OSet (DPBranch Anchor)) -> DPBranch Anchor, b :: NonNull (OSet (DPBranch Anchor)))
      rest
  where
    pMidpoint =
      symbol "/" *> option unanchored (pPointlike pNID) <* symbol "|"

-- | Parse a set of branches separated by &
pSequenceBranchSet :: Parser NID -> Parser (NonNull (OSet (DPBranch Anchor)))
pSequenceBranchSet pNID =
  label "sequence branch set" $
    try (impureNonNull . setFromList <$> pMultiple)
      <|> (singletonNNSet <$> pSingleBranch)
  where
    pMultiple = parens (pBranch pNID `sepBy1` symbol "&")

-- | Parse a set of branches separated by &
pRootedBranchSet :: Parser NID -> Parser (NonNull (OSet (DPBranch Anchor)))
pRootedBranchSet pNID =
  label "rooted branch set" $
    try (singletonNNSet <$> pBranch pNID)
      <|> (impureNonNull . setFromList <$> pMultiple)
  where
    pMultiple = parens (pBranch pNID `sepBy1` symbol "&")

-- | Parse explicit anchor: "@", "@nid", "!{@nid, @nid2, ...}"
pExplicitAnchor :: Parser NID -> Parser Anchor
pExplicitAnchor pNID =
  label "explicit anchor" $
    try (Specific <$> pNID)
      <|> between
        (symbol "!{")
        (symbol "}")
        (JoinPoint . setFromList <$> pNID `sepEndBy1` symbol ",")
      <|> (symbol "@" $> JoinPoint mempty)

-- | Parse root branches: "@<branches & @nid<branches & ..."
pRootBranches ::
  Parser NID ->
  Parser (OMap (DeterministicPath Anchor) (NonNull (OSet (DPBranch Anchor))))
pRootBranches pNID = label "root branches" do
  branches <- sepBy1 (pRootBranch pNID) (symbol "&")
  pure $ unionsWith (<>) $ uncurry singletonMap <$> branches

pRootBranch ::
  Parser NID ->
  Parser (DeterministicPath Anchor, NonNull (OSet (DPBranch Anchor)))
pRootBranch pNID = label "root branch" do
  root <-
    option
      (Pointlike unanchored)
      (pDeterministicPath pNID <* symbol "<")
  branches <- pRootedBranchSet pNID
  pure (root, branches)

pTarget :: Parser NID -> Parser (PointlikeDeterministicPath Anchor)
pTarget pNID =
  label "target" $
    char '>' *> pPointlike pNID
