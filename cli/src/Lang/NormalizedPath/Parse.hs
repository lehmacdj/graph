module Lang.NormalizedPath.Parse (pNormalizedPath) where

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

pBranch :: (Ord t) => Parser t -> Parser (DPBranch t)
pBranch pTransition =
  try (pSequence pTransition)
    <|> (symbol "*" $> DPWild)
    <|> (DPLiteral <$> pTransition)

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlike :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t)
pPointlike pTransition = do
  anchor <- pExplicitAnchor
  loops <- brackets (pBranchSet pTransition)
  pure $ PointlikeDeterministicPath anchor loops

-- | Parse rooted deterministic path: "[@<branches]", "[@<branches]>target", "[branches]"
pRootedPath :: (Ord t) => Parser t -> Parser (RootedDeterministicPath t)
pRootedPath pTransition = do
  _ <- char '['
  rootBranches <- try (pRootBranches pTransition) <|> (singletonMap unanchored <$> pBranchSet pTransition)
  _ <- char ']'
  target <- try (char '>' *> pTarget pTransition) <|> pure unanchored
  pure $ RootedDeterministicPath rootBranches target

-- | Parse sequence branch using /| operator: "a/@|b", "a/|b", "(a & b)/@|c"
pSequence :: (Ord t) => Parser t -> Parser (DPBranch t)
pSequence pTransition = do
  firstPart <- pBranchSet pTransition
  _ <- symbol "/"
  midpoint <- option unanchored $ try (pPointlike pTransition)
  _ <- symbol "|"
  secondPart <- pBranchSet pTransition
  pure $ DPSequence firstPart midpoint secondPart

-- | Parse explicit anchor: "@", "@nid"
pExplicitAnchor :: Parser Anchor
pExplicitAnchor = do
  _ <- char '@'
  optional pSmallNodeId >>= \case
    Just nid -> pure (Specific nid)
    Nothing -> pure JoinPoint

-- | Parse root branches: "@<branches & @nid<branches & ..."
pRootBranches :: (Ord t) => Parser t -> Parser (Map (PointlikeDeterministicPath t) (Set (DPBranch t)))
pRootBranches pTransition = do
  branches <- sepBy1 pRootBranch (symbol "&")
  pure $ Map.fromListWith (<>) branches
  where
    pRootBranch = do
      root <- option unanchored (pPointlike pTransition <* symbol "<")
      branch <- pBranch pTransition
      pure (root, singletonSet branch)

pTarget :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t)
pTarget pTransition = char '>' *> pPointlike pTransition

-- | Parse a set of branches separated by &
pBranchSet :: (Ord t) => Parser t -> Parser (Set (DPBranch t))
pBranchSet pTransition =
  try (setFromList <$> pMultiple)
    <|> try (singletonSet <$> pBranch pTransition)
  where
    pMultiple = parens (pBranch pTransition `sepBy1` symbol "&")
