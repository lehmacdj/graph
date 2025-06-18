module Lang.NormalizedPath.Parse
  ( pNormalizedPath,
    normalizedPathTerm,
  )
where

import Control.Monad.Combinators.Expr
import Data.Functor
import Lang.Parsing
import Models.NID
import Models.NormalizedPath
import MyPrelude hiding (try)
import Text.Megaparsec (sepBy1, try, (<?>))
import Text.Megaparsec.Char (char)

-- | Parse a normalized path term (atom)
normalizedPathTerm :: (Ord t) => Parser t -> Parser (NormalizedPath t)
normalizedPathTerm pTransition =
  try (pPointlikePath pTransition)
    <|> try (pRootedPath pTransition) 
    <|> try (pSequencePath pTransition)
    <|> try (pLiteral pTransition)
    <|> try (pWild)
    <|> parens (pNormalizedPath pTransition)

-- | Parse a literal as a rooted deterministic path: "a"
pLiteral :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pLiteral pTransition = do
  t <- pTransition
  pure . NormalizedPath . singletonSet . Rooted $
    RootedDeterministicPath
      (singletonMap unanchored (singletonSet (DPLiteral t)))
      unanchored

-- | Parse a wild as a rooted deterministic path: "*"
pWild :: (Ord t) => Parser (NormalizedPath t)
pWild = do
  symbol "*"
  pure . NormalizedPath . singletonSet . Rooted $
    RootedDeterministicPath
      (singletonMap unanchored (singletonSet DPWild))
      unanchored

-- | Parse pointlike deterministic path: "@[loops]" or "@nid[loops]"
pPointlikePath :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pPointlikePath pTransition = do
  char '@'
  anchor <- try (Specific <$> nodeId) <|> pure JoinPoint
  char '['
  loops <- pLoopSet pTransition
  char ']'
  pure . NormalizedPath . singletonSet . Pointlike $
    PointlikeDeterministicPath anchor loops

-- | Parse rooted deterministic path: "[@<branches & @nid<branches]>target"
pRootedPath :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pRootedPath pTransition = do
  char '['
  rootBranches <- pRootBranches pTransition
  char ']'
  char '>'
  target <- pTarget pTransition
  pure . NormalizedPath . singletonSet . Rooted $
    RootedDeterministicPath (mapFromList rootBranches) target

-- | Parse sequence path using ternary /| operator: "a/@|b"
pSequencePath :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pSequencePath pTransition = do
  firstPart <- pSequencePart pTransition
  symbol "/"
  midpoint <- pMidpoint pTransition
  symbol "|"
  secondPart <- pSequencePart pTransition
  pure . NormalizedPath . singletonSet . Rooted $
    RootedDeterministicPath
      (singletonMap unanchored (singletonSet $ DPSequence firstPart midpoint secondPart))
      unanchored

-- | Parse a part of a sequence (set of branches)
pSequencePart :: (Ord t) => Parser t -> Parser (Set (DPBranch t))
pSequencePart pTransition = setFromList <$> sepBy1 pSimpleBranch (symbol "&")
  where
    pSimpleBranch = 
      try (symbol "*" $> DPWild)
        <|> (DPLiteral <$> pTransition)

-- | Parse a midpoint in a sequence
pMidpoint :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t)
pMidpoint pTransition =
  try (do
    char '@'
    anchor <- try (Specific <$> nodeId) <|> pure JoinPoint
    char '['
    loops <- pLoopSet pTransition
    char ']'
    pure $ PointlikeDeterministicPath anchor loops)
  <|> (do
    char '@'
    anchor <- try (Specific <$> nodeId) <|> pure JoinPoint
    pure $ PointlikeDeterministicPath anchor mempty)

-- | Parse root branches: "@<branches & @nid<branches & ..."
pRootBranches :: (Ord t) => Parser t -> Parser [(PointlikeDeterministicPath t, Set (DPBranch t))]
pRootBranches pTransition = sepBy1 pSingleRootBranch (symbol "&")
  where
    pSingleRootBranch = do
      char '@'
      anchor <- try (Specific <$> nodeId) <|> pure JoinPoint
      char '<'
      branches <- pLoopSet pTransition
      pure (PointlikeDeterministicPath anchor mempty, branches)

-- | Parse target: "@" or "@nid"
pTarget :: (Ord t) => Parser t -> Parser (PointlikeDeterministicPath t)
pTarget _pTransition = do
  char '@'
  anchor <- try (Specific <$> nodeId) <|> pure JoinPoint
  pure $ PointlikeDeterministicPath anchor mempty

-- | Parse a set of loops/branches separated by &
pLoopSet :: (Ord t) => Parser t -> Parser (Set (DPBranch t))
pLoopSet pTransition = setFromList <$> sepBy1 pBranch (symbol "&")
  where
    pBranch = 
      try (symbol "*" $> DPWild)
        <|> (DPLiteral <$> pTransition)

-- | Binary operators for normalized paths
binary :: String -> (NormalizedPath t -> NormalizedPath t -> NormalizedPath t) -> Operator Parser (NormalizedPath t)
binary name f = InfixL (f <$ symbol name)

-- | Union operation for normalized paths
unionNormalizedPath :: (Ord t) => NormalizedPath t -> NormalizedPath t -> NormalizedPath t
unionNormalizedPath (NormalizedPath s1) (NormalizedPath s2) = NormalizedPath (s1 <> s2)

-- | Operator precedence table
table :: (Ord t) => [[Operator Parser (NormalizedPath t)]]
table =
  [ [binary "+" unionNormalizedPath]
  ]

-- | Main parser for normalized paths
pNormalizedPath :: (Ord t) => Parser t -> Parser (NormalizedPath t)
pNormalizedPath pTransition = makeExprParser (normalizedPathTerm pTransition) table