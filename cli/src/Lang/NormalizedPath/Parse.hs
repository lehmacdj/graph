module Lang.NormalizedPath.Parse where

import Control.Monad.Fail (MonadFail (fail))
import Data.Functor
import Data.Map qualified as Map (fromListWith)
import Lang.Parsing
import Models.NID
import Models.NormalizedPath
import MyPrelude hiding (try)
import Text.Megaparsec (option, sepBy1, try)
import Text.Megaparsec.Char (char, digitChar)

-- | Parse a small integer and convert to NID
pSmallNodeId :: Parser NID
pSmallNodeId = lexeme $ do
  digits <- some digitChar
  case readMay digits of
    Just n -> pure (smallNID n)
    Nothing -> fail "Invalid integer for node ID"

-- | Parse a normalized path term (atom)
pNormalizedPath :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pNormalizedPath pTransition =
  NormalizedPath . setFromList <$> pDeterministicPath pTransition `sepBy1` symbol "+"

pDeterministicPath :: (Ord t) => Parser t -> Parser (DeterministicPath t)
pDeterministicPath pTransition =
  try (Pointlike <$> pPointlike pTransition)
    <|> (Rooted <$> pRootedPath pTransition)

pSingleBranch :: (Ord t) => Parser t -> Parser (DPBranch t)
pSingleBranch pTransition =
  (symbol "*" $> DPWild)
    <|> (DPLiteral <$> pTransition)

pBranch :: (Ord t) => Parser t -> Parser (DPBranch t)
pBranch pTransition = try (pSequence pTransition) <|> pSingleBranch pTransition

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t)
pPointlike pTransition = do
  anchor <- pExplicitAnchor
  loops <- option [] $ brackets (pBranch pTransition `sepBy1` symbol "&")
  pure $ PointlikeDeterministicPath anchor (setFromList loops)

pRootedPath :: (Ord t) => Parser t -> Parser (RootedDeterministicPath t)
pRootedPath pTransition =
  try (pMultiRootedPath pTransition)
    <|> pSingleRootedPath pTransition

pSingleRootedPath :: (Ord t) => Parser t -> Parser (RootedDeterministicPath t)
pSingleRootedPath pTransition = do
  rootBranch <- pRootBranch pTransition
  target <- option unanchored (pTarget pTransition)
  pure $ RootedDeterministicPath (uncurry singletonMap rootBranch) target

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pMultiRootedPath :: (Ord t) => Parser t -> Parser (RootedDeterministicPath t)
pMultiRootedPath pTransition = do
  rootBranches <- brackets (pRootBranches pTransition)
  target <- option unanchored (pTarget pTransition)
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: forall t. (Ord t) => Parser t -> Parser (DPBranch t)
pSequence pTransition = do
  (initial, splitFirst -> ((m, b), rest)) <- pBranchSet pTransition `via1` pMidpoint
  pure . uncurry ($) $
    foldl'
      (\(acc, x) (n, y) -> (acc . singletonSet . DPSequence x n, y))
      (DPSequence initial m :: Set (DPBranch t) -> DPBranch t, b :: Set (DPBranch t))
      rest
  where
    pMidpoint =
      symbol "/" *> option unanchored (pPointlike pTransition) <* symbol "|"

-- | Parse a set of branches separated by &
pBranchSet :: (Ord t) => Parser t -> Parser (Set (DPBranch t))
pBranchSet pTransition =
  (setFromList <$> pMultiple)
    <|> (singletonSet <$> pSingleBranch pTransition)
  where
    pMultiple = parens (pBranch pTransition `sepBy1` symbol "&")

-- | Parse explicit anchor: "@", "@nid"
pExplicitAnchor :: Parser Anchor
pExplicitAnchor = do
  _ <- char '@'
  optional pSmallNodeId >>= \case
    Just nid -> pure (Specific nid)
    Nothing -> s $> JoinPoint

-- | Parse root branches: "@<branches & @nid<branches & ..."
pRootBranches :: (Ord t) => Parser t -> Parser (Map (PointlikeDeterministicPath t) (Set (DPBranch t)))
pRootBranches pTransition = do
  branches <- sepBy1 (pRootBranch pTransition) (symbol "&")
  pure $ Map.fromListWith (<>) branches

pRootBranch :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t, Set (DPBranch t))
pRootBranch pTransition = do
  root <- option unanchored (pPointlike pTransition <* symbol "<")
  branch <- pBranch pTransition
  pure (root, singletonSet branch)

pTarget :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t)
pTarget pTransition = char '>' *> pPointlike pTransition
