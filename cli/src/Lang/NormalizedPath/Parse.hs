module Lang.NormalizedPath.Parse
  ( pNormalizedPath,
    pNormalizedPath',
  )
where

import Data.Functor
import Data.Map qualified as Map (fromListWith)
import Lang.Parsing
import Models.NID
import Models.NormalizedPath
import MyPrelude hiding (try)
import Text.Megaparsec (option, sepBy1, try)
import Text.Megaparsec.Char (char)

-- | Parse a normalized path term (atom) - generalized version
pNormalizedPath' :: (Ord t) => Parser NID -> Parser t -> Parser (NormalizedPath Anchor t)
pNormalizedPath' pNID pTransition =
  fmap (NormalizedPath . setFromList) $
    (pDeterministicPath pNID pTransition `sepBy1` symbol "+")
      <|> (symbol "!" $> [])

-- | Parse a normalized path term (atom)
pNormalizedPath :: (Ord t) => Parser t -> Parser (NormalizedPath Anchor t)
pNormalizedPath = pNormalizedPath' pSmallNID

pDeterministicPath :: (Ord t) => Parser NID -> Parser t -> Parser (DeterministicPath Anchor t)
pDeterministicPath pNID pTransition =
  try (Pointlike <$> pPointlike pNID pTransition)
    <|> (Rooted <$> pRootedPath pNID pTransition)

pSingleBranch :: (Ord t) => Parser t -> Parser (DPBranch Anchor t)
pSingleBranch pTransition =
  (symbol "*" $> DPWild)
    <|> (DPLiteral <$> pTransition)

pBranch :: (Ord t) => Parser NID -> Parser t -> Parser (DPBranch Anchor t)
pBranch pNID pTransition = try (pSequence pNID pTransition) <|> pSingleBranch pTransition

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: (Ord t) => Parser NID -> Parser t -> Parser (PointlikeDeterministicPath Anchor t)
pPointlike pNID pTransition = do
  anchor <- pExplicitAnchor pNID
  loops <- option [] $ brackets (pBranch pNID pTransition `sepBy1` symbol "&")
  pure $ PointlikeDeterministicPath anchor (setFromList loops)

pRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath Anchor t)
pRootedPath pNID pTransition =
  try (pMultiRootedPath pNID pTransition)
    <|> pSingleRootedPath pNID pTransition

pSingleRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath Anchor t)
pSingleRootedPath pNID pTransition = do
  rootBranch <- pRootBranch pNID pTransition
  target <- option unanchored (pTarget pNID pTransition)
  pure $ RootedDeterministicPath (uncurry singletonMap rootBranch) target

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pMultiRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath Anchor t)
pMultiRootedPath pNID pTransition = do
  rootBranches <- brackets (pRootBranches pNID pTransition)
  target <- option unanchored (pTarget pNID pTransition)
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: forall t. (Ord t) => Parser NID -> Parser t -> Parser (DPBranch Anchor t)
pSequence pNID pTransition = do
  (initial, splitFirst -> ((m, b), rest)) <- pBranchSet pNID pTransition `via1` pMidpoint
  pure . uncurry ($) $
    foldl'
      (\(acc, x) (n, y) -> (acc . singletonSet . DPSequence x n, y))
      (DPSequence initial m :: Set (DPBranch Anchor t) -> DPBranch Anchor t, b :: Set (DPBranch Anchor t))
      rest
  where
    pMidpoint =
      symbol "/" *> option unanchored (pPointlike pNID pTransition) <* symbol "|"

-- | Parse a set of branches separated by &
pBranchSet :: (Ord t) => Parser NID -> Parser t -> Parser (Set (DPBranch Anchor t))
pBranchSet pNID pTransition =
  (setFromList <$> pMultiple)
    <|> (singletonSet <$> pSingleBranch pTransition)
  where
    pMultiple = parens (pBranch pNID pTransition `sepBy1` symbol "&")

-- | Parse explicit anchor: "@", "@nid"
pExplicitAnchor :: Parser NID -> Parser Anchor
pExplicitAnchor pNID = try (Specific <$> pNID) <|> (symbol "@" $> JoinPoint)

-- | Parse root branches: "@<branches & @nid<branches & ..."
pRootBranches ::
  (Ord t) =>
  Parser NID ->
  Parser t ->
  Parser (Map (PointlikeDeterministicPath Anchor t) (Set (DPBranch Anchor t)))
pRootBranches pNID pTransition = do
  branches <- sepBy1 (pRootBranch pNID pTransition) (symbol "&")
  pure $ Map.fromListWith (<>) branches

pRootBranch ::
  (Ord t) =>
  Parser NID ->
  Parser t ->
  Parser (PointlikeDeterministicPath Anchor t, Set (DPBranch Anchor t))
pRootBranch pNID pTransition = do
  root <- option unanchored (pPointlike pNID pTransition <* symbol "<")
  branch <- pBranch pNID pTransition
  pure (root, singletonSet branch)

pTarget :: (Ord t) => Parser NID -> Parser t -> Parser (PointlikeDeterministicPath Anchor t)
pTarget pNID pTransition = char '>' *> pPointlike pNID pTransition
