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
pNormalizedPath' :: (Ord t) => Parser NID -> Parser t -> Parser (NormalizedPath t)
pNormalizedPath' pNID pTransition =
  NormalizedPath . setFromList <$> pDeterministicPath pNID pTransition `sepBy1` symbol "+"

-- | Parse a normalized path term (atom)
pNormalizedPath :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pNormalizedPath = pNormalizedPath' pSmallNID

pDeterministicPath :: (Ord t) => Parser NID -> Parser t -> Parser (DeterministicPath t)
pDeterministicPath pNID pTransition =
  try (Pointlike <$> pPointlike pNID pTransition)
    <|> (Rooted <$> pRootedPath pNID pTransition)

pSingleBranch :: (Ord t) => Parser t -> Parser (DPBranch t)
pSingleBranch pTransition =
  (symbol "*" $> DPWild)
    <|> (DPLiteral <$> pTransition)

pBranch :: (Ord t) => Parser NID -> Parser t -> Parser (DPBranch t)
pBranch pNID pTransition = try (pSequence pNID pTransition) <|> pSingleBranch pTransition

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: (Ord t) => Parser NID -> Parser t -> Parser (PointlikeDeterministicPath t)
pPointlike pNID pTransition = do
  anchor <- pExplicitAnchor pNID
  loops <- option [] $ brackets (pBranch pNID pTransition `sepBy1` symbol "&")
  pure $ PointlikeDeterministicPath anchor (setFromList loops)

pRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath t)
pRootedPath pNID pTransition =
  try (pMultiRootedPath pNID pTransition)
    <|> pSingleRootedPath pNID pTransition

pSingleRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath t)
pSingleRootedPath pNID pTransition = do
  rootBranch <- pRootBranch pNID pTransition
  target <- option unanchored (pTarget pNID pTransition)
  pure $ RootedDeterministicPath (uncurry singletonMap rootBranch) target

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pMultiRootedPath :: (Ord t) => Parser NID -> Parser t -> Parser (RootedDeterministicPath t)
pMultiRootedPath pNID pTransition = do
  rootBranches <- brackets (pRootBranches pNID pTransition)
  target <- option unanchored (pTarget pNID pTransition)
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: forall t. (Ord t) => Parser NID -> Parser t -> Parser (DPBranch t)
pSequence pNID pTransition = do
  (initial, splitFirst -> ((m, b), rest)) <- pBranchSet pNID pTransition `via1` pMidpoint
  pure . uncurry ($) $
    foldl'
      (\(acc, x) (n, y) -> (acc . singletonSet . DPSequence x n, y))
      (DPSequence initial m :: Set (DPBranch t) -> DPBranch t, b :: Set (DPBranch t))
      rest
  where
    pMidpoint =
      symbol "/" *> option unanchored (pPointlike pNID pTransition) <* symbol "|"

-- | Parse a set of branches separated by &
pBranchSet :: (Ord t) => Parser NID -> Parser t -> Parser (Set (DPBranch t))
pBranchSet pNID pTransition =
  (setFromList <$> pMultiple)
    <|> (singletonSet <$> pSingleBranch pTransition)
  where
    pMultiple = parens (pBranch pNID pTransition `sepBy1` symbol "&")

-- | Parse explicit anchor: "@", "@nid"
pExplicitAnchor :: Parser NID -> Parser Anchor
pExplicitAnchor pNID = do
  _ <- char '@'
  optional pNID >>= \case
    Just nid -> pure (Specific nid)
    Nothing -> s $> JoinPoint

-- | Parse root branches: "@<branches & @nid<branches & ..."
pRootBranches ::
  (Ord t) =>
  Parser NID ->
  Parser t ->
  Parser (Map (PointlikeDeterministicPath t) (Set (DPBranch t)))
pRootBranches pNID pTransition = do
  branches <- sepBy1 (pRootBranch pNID pTransition) (symbol "&")
  pure $ Map.fromListWith (<>) branches

pRootBranch ::
  (Ord t) =>
  Parser NID ->
  Parser t ->
  Parser (PointlikeDeterministicPath t, Set (DPBranch t))
pRootBranch pNID pTransition = do
  root <- option unanchored (pPointlike pNID pTransition <* symbol "<")
  branch <- pBranch pNID pTransition
  pure (root, singletonSet branch)

pTarget :: (Ord t) => Parser NID -> Parser t -> Parser (PointlikeDeterministicPath t)
pTarget pNID pTransition = char '>' *> pPointlike pNID pTransition
