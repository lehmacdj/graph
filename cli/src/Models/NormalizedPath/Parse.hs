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
pNormalizedPath' :: (Ord t) => Parser NID -> Parser t -> Parser (NormalizedPath Anchor t)
pNormalizedPath' pNID pTransition =
  fmap (NormalizedPath . setFromList) $
    (pDeterministicPath pNID pTransition `sepBy1` symbol "+")
      <|> (symbol "%never" $> [])

-- | Parse a normalized path term (atom)
pNormalizedPath :: (Ord t) => Parser t -> Parser (NormalizedPath Anchor t)
pNormalizedPath = pNormalizedPath' pSmallNID

pDeterministicPath :: (Ord t) => Parser NID -> Parser t -> Parser (DeterministicPath Anchor t)
pDeterministicPath pNID pTransition =
  label "deterministic path" $
    try (Pointlike <$> pPointlike pNID pTransition)
      <|> (Rooted <$> pRootedPath pNID pTransition)

pSingleBranch :: (Ord t) => Parser t -> Parser (DPBranch Anchor t)
pSingleBranch pTransition =
  label "single branch" $
    try (symbol "~*" $> DPIncoming DPWild)
      <|> (symbol "*" $> DPOutgoing DPWild)
      <|> try (DPIncoming . DPLiteral <$> (char '~' *> pTransition))
      <|> (DPOutgoing . DPLiteral <$> pTransition)

pBranch :: (Ord t) => Parser NID -> Parser t -> Parser (DPBranch Anchor t)
pBranch pNID pTransition =
  label "branch" $
    try (pSequence pNID pTransition) <|> pSingleBranch pTransition

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: (Ord t) => Parser NID -> Parser t -> Parser (PointlikeDeterministicPath Anchor t)
pPointlike pNID pTransition = label "pointlike" do
  anchor <- pExplicitAnchor pNID
  loops <- option [] $ brackets (pBranch pNID pTransition `sepBy1` symbol "&")
  pure $ PointlikeDeterministicPath anchor (setFromList loops)

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath Anchor t)
pRootedPath pNID pTransition = label "rooted path" do
  rootBranches <- brackets (pRootBranches pNID pTransition)
  target <- option unanchored (pTarget pNID pTransition)
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: forall t. (Ord t) => Parser NID -> Parser t -> Parser (DPBranch Anchor t)
pSequence pNID pTransition = label "sequence" do
  (initial, splitFirst -> ((m, b), rest)) <- pSequenceBranchSet pNID pTransition `via1` pMidpoint
  pure . uncurry ($) $
    foldl'
      (\(acc, x) (n, y) -> (acc . singletonSet . DPSequence x n, y))
      (DPSequence initial m :: OSet (DPBranch Anchor t) -> DPBranch Anchor t, b :: OSet (DPBranch Anchor t))
      rest
  where
    pMidpoint =
      symbol "/" *> option unanchored (pPointlike pNID pTransition) <* symbol "|"

-- | Parse a set of branches separated by &
pSequenceBranchSet :: (Ord t) => Parser NID -> Parser t -> Parser (OSet (DPBranch Anchor t))
pSequenceBranchSet pNID pTransition =
  label "sequence branch set" $
    try (setFromList <$> pMultiple)
      <|> (singletonSet <$> pSingleBranch pTransition)
  where
    pMultiple = parens (pBranch pNID pTransition `sepBy1` symbol "&")

-- | Parse a set of branches separated by &
pRootedBranchSet :: (Ord t) => Parser NID -> Parser t -> Parser (OSet (DPBranch Anchor t))
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
  (Ord t) =>
  Parser NID ->
  Parser t ->
  Parser (OMap (DeterministicPath Anchor t) (OSet (DPBranch Anchor t)))
pRootBranches pNID pTransition = label "root branches" do
  branches <- sepBy1 (pRootBranch pNID pTransition) (symbol "&")
  pure $ unionsWith (<>) $ uncurry singletonMap <$> branches

pRootBranch ::
  (Ord t) =>
  Parser NID ->
  Parser t ->
  Parser (DeterministicPath Anchor t, OSet (DPBranch Anchor t))
pRootBranch pNID pTransition = label "root branch" do
  root <-
    option
      (Pointlike unanchored)
      (pDeterministicPath pNID pTransition <* symbol "<")
  branches <- pRootedBranchSet pNID pTransition
  pure (root, branches)

pTarget :: (Ord t) => Parser NID -> Parser t -> Parser (PointlikeDeterministicPath Anchor t)
pTarget pNID pTransition =
  label "target" $
    char '>' *> pPointlike pNID pTransition
