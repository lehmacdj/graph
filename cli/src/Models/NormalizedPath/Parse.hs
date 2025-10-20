module Models.NormalizedPath.Parse
  ( pNormalizedPath,
    pNormalizedPath',
  )
where

import Data.Functor
import Models.NID
import Models.NormalizedPath
import MyPrelude hiding (try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)
import Utils.Parsing

-- | Parse a normalized path term (atom) - generalized version
pNormalizedPath' :: Parser NID -> Parser Text -> Parser (NormalizedPath Anchor)
pNormalizedPath' pNID pTransition =
  fmap (NormalizedPath . setFromList) $
    (pDeterministicPath pNID pTransition `sepBy1` symbol "+")
      <|> (symbol "%never" $> [])

-- | Parse a normalized path term (atom)
pNormalizedPath :: Parser Text -> Parser (NormalizedPath Anchor)
pNormalizedPath = pNormalizedPath' pSmallNID

pDeterministicPath :: Parser NID -> Parser Text -> Parser (DeterministicPath Anchor)
pDeterministicPath pNID pTransition =
  label "deterministic path" $
    try (Pointlike <$> pPointlike pNID pTransition)
      <|> (Rooted <$> pRootedPath pNID pTransition)

pSingleBranch :: Parser Text -> Parser (DPBranch Anchor)
pSingleBranch pTransition =
  label "single branch" $
    try (symbol "~*" $> DPIncoming DPWild)
      <|> (symbol "*" $> DPOutgoing DPWild)
      <|> try (DPIncoming . DPLiteral <$> (char '~' *> pTransition))
      <|> (DPOutgoing . DPLiteral <$> pTransition)

pBranch :: Parser NID -> Parser Text -> Parser (DPBranch Anchor)
pBranch pNID pTransition =
  label "branch" $
    try (pSequence pNID pTransition) <|> pSingleBranch pTransition

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: Parser NID -> Parser Text -> Parser (PointlikeDeterministicPath Anchor)
pPointlike pNID pTransition = label "pointlike" do
  anchor <- pExplicitAnchor pNID
  loops <- option [] $ brackets (pBranch pNID pTransition `sepBy1` symbol "&")
  pure $ PointlikeDeterministicPath anchor (setFromList loops)

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pRootedPath :: Parser NID -> Parser Text -> Parser (RootedDeterministicPath Anchor)
pRootedPath pNID pTransition = label "rooted path" do
  rootBranches <- brackets (pRootBranches pNID pTransition)
  target <- option unanchored (pTarget pNID pTransition)
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: Parser NID -> Parser Text -> Parser (DPBranch Anchor)
pSequence pNID pTransition = label "sequence" do
  (initial, splitFirst -> ((m, b), rest)) <- pSequenceBranchSet pNID pTransition `via1` pMidpoint
  pure . uncurry ($) $
    foldl'
      (\(acc, x) (n, y) -> (acc . singletonSet . DPSequence x n, y))
      (DPSequence initial m :: OSet (DPBranch Anchor) -> DPBranch Anchor, b :: OSet (DPBranch Anchor))
      rest
  where
    pMidpoint =
      symbol "/" *> option unanchored (pPointlike pNID pTransition) <* symbol "|"

-- | Parse a set of branches separated by &
pSequenceBranchSet :: Parser NID -> Parser Text -> Parser (OSet (DPBranch Anchor))
pSequenceBranchSet pNID pTransition =
  label "sequence branch set" $
    try (setFromList <$> pMultiple)
      <|> (singletonSet <$> pSingleBranch pTransition)
  where
    pMultiple = parens (pBranch pNID pTransition `sepBy1` symbol "&")

-- | Parse a set of branches separated by &
pRootedBranchSet :: Parser NID -> Parser Text -> Parser (OSet (DPBranch Anchor))
pRootedBranchSet pNID pTransition =
  label "rooted branch set" $
    try (singletonSet <$> pBranch pNID pTransition)
      <|> (setFromList <$> pMultiple)
  where
    pMultiple = parens (pBranch pNID pTransition `sepBy1` symbol "&")

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
  Parser Text ->
  Parser (OMap (DeterministicPath Anchor) (OSet (DPBranch Anchor)))
pRootBranches pNID pTransition = label "root branches" do
  branches <- sepBy1 (pRootBranch pNID pTransition) (symbol "&")
  pure $ unionsWith (<>) $ uncurry singletonMap <$> branches

pRootBranch ::
  Parser NID ->
  Parser Text ->
  Parser (DeterministicPath Anchor, OSet (DPBranch Anchor))
pRootBranch pNID pTransition = label "root branch" do
  root <-
    option
      (Pointlike unanchored)
      (pDeterministicPath pNID pTransition <* symbol "<")
  branches <- pRootedBranchSet pNID pTransition
  pure (root, branches)

pTarget :: Parser NID -> Parser Text -> Parser (PointlikeDeterministicPath Anchor)
pTarget pNID pTransition =
  label "target" $
    char '>' *> pPointlike pNID pTransition
