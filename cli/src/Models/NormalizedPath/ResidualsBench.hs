-- | Profiling / benchmark harness for 'pathResidualsUnresolvedBy'.
--
-- Run via the @residuals-bench@ executable:
--
-- > stack build graph:exe:residuals-bench
-- > stack exec residuals-bench -- --cases 1000
--
-- For a cost-center profile, build with @profile-build.sh@ and run e.g.:
--
-- > stack --profile exec residuals-bench -- --cases 200 +RTS -p -RTS
--
-- The harness generates the same distribution of (path, graph) cases as
-- the property tests in "Models.NormalizedPath.UnresolvedResiduals"
-- ('coveredPathFrom' + 'graphResolvingPathFrom'), deletes a
-- scenario-dependent subset of the resolving graph's edges, and times the
-- full evaluation of 'pathResidualsUnresolvedBy' on each case. Every case
-- is derived deterministically from its case number, so any case reported
-- as slow can be reproduced exactly by rerunning with the same arguments
-- (or from the printed path/start/graph).
module Models.NormalizedPath.ResidualsBench (residualsBenchMain) where

import GHC.Clock qualified
import Models.Graph
import Models.NID (NID)
import Models.NormalizedPath
import Models.NormalizedPath.Generators
import Models.NormalizedPath.TestUtils
import Models.NormalizedPath.UnresolvedResiduals (pathResidualsUnresolvedBy)
import MyPrelude
import System.Environment qualified as Env
import System.Exit (exitFailure)
import System.Random (mkStdGen, randoms)
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (mkQCGen)
import Text.Printf (printf)

-- * Options

data Scenario
  = RemoveNone
  | RemoveOne
  | RemoveSubset
  | RemoveAll
  deriving (Eq, Show)

-- | Which edge-removal scenario each case uses. Mixed cycles through all
-- four so one sweep covers resolved, one-missing-edge, partial, and empty
-- graphs.
data ScenarioMode = Fixed Scenario | Mixed
  deriving (Eq, Show)

data Options = Options
  { cases :: Int,
    firstSeed :: Int,
    config :: GenConfig,
    configName :: String,
    scenarioMode :: ScenarioMode,
    timeoutMs :: Int,
    -- | how many of the slowest cases to print in detail
    top :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { cases = 1000,
      firstSeed = 0,
      config = defaultGenConfig,
      configName = "default",
      scenarioMode = Mixed,
      timeoutMs = 5000,
      top = 10
    }

-- | Deeper/wider than 'defaultGenConfig': generates paths beyond what the
-- property tests exercise, to probe for combinatorial blowups in
-- alternative enumeration before they qualify as "extremely degenerate".
deepGenConfig :: GenConfig
deepGenConfig =
  GenConfig
    { maxDepth = 4,
      maxBranches = 4,
      maxLoops = 2,
      maxUnionSize = 4,
      maxExcluding = 2
    }

usage :: String
usage =
  unlines
    [ "usage: residuals-bench [OPTIONS]",
      "",
      "  --cases N        number of generated cases (default 1000)",
      "  --seed N         first case seed (default 0)",
      "  --config NAME    generator config: default | shallow | deep",
      "  --scenario NAME  edges to delete before computing the residual:",
      "                   mixed | none | one | subset | all (default mixed)",
      "  --timeout-ms N   per-case time limit in ms (default 5000)",
      "  --top K          print the K slowest cases in detail (default 10)"
    ]

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions
  where
    go opts = \case
      [] -> Right opts
      ("--cases" : v : rest) -> withInt v rest \n -> opts {cases = n}
      ("--seed" : v : rest) -> withInt v rest \n -> opts {firstSeed = n}
      ("--config" : v : rest) -> case v of
        "default" ->
          go opts {config = defaultGenConfig, configName = v} rest
        "shallow" ->
          go opts {config = shallowGenConfig, configName = v} rest
        "deep" ->
          go opts {config = deepGenConfig, configName = v} rest
        _ -> Left ("unknown config: " <> v)
      ("--scenario" : v : rest) -> case v of
        "mixed" -> go opts {scenarioMode = Mixed} rest
        "none" -> go opts {scenarioMode = Fixed RemoveNone} rest
        "one" -> go opts {scenarioMode = Fixed RemoveOne} rest
        "subset" -> go opts {scenarioMode = Fixed RemoveSubset} rest
        "all" -> go opts {scenarioMode = Fixed RemoveAll} rest
        _ -> Left ("unknown scenario: " <> v)
      ("--timeout-ms" : v : rest) -> withInt v rest \n -> opts {timeoutMs = n}
      ("--top" : v : rest) -> withInt v rest \n -> opts {top = n}
      (arg : _) -> Left ("unknown argument: " <> arg)
      where
        withInt v rest f = case readMay v of
          Just n -> go (f n) rest
          Nothing -> Left ("not a number: " <> v)

-- * Case generation

data BenchCase = BenchCase
  { caseSeed :: Int,
    scenario :: Scenario,
    startNid :: NID,
    path :: NormalizedPath Anchor,
    -- | the graph the residual is computed against, after edge removal
    benchGraph :: Graph Text ()
  }

-- | One deterministic case per seed: the same path/graph generation as the
-- property tests, then scenario-dependent edge removal.
benchCase :: Options -> Int -> BenchCase
benchCase opts caseSeed =
  BenchCase {caseSeed, scenario, startNid, path, benchGraph}
  where
    -- mimic QuickCheck's size ramp so the case distribution matches the
    -- property tests
    size = caseSeed `mod` 100
    (startNid, dp) =
      unGen genStartAndPath (mkQCGen caseSeed) size
    genStartAndPath :: Gen (NID, DeterministicPath Anchor)
    genStartAndPath = do
      start <- genNID
      p <- genDeterministicPathLiteralOnly opts.config
      pure (start, coveredPathFrom start p)
    path = NormalizedPath (singletonSet dp)
    fullGraph = graphResolvingPathFrom (mkStdGen caseSeed) startNid path
    edges = graphEdges fullGraph
    scenario = case opts.scenarioMode of
      Fixed s -> s
      Mixed -> case caseSeed `mod` 4 of
        0 -> RemoveNone
        1 -> RemoveOne
        2 -> RemoveSubset
        _ -> RemoveAll
    removed = case scenario of
      RemoveNone -> []
      RemoveOne
        | null edges -> []
        | otherwise -> [edges `indexEx` (caseSeed `mod` length edges)]
      RemoveSubset ->
        [e | (e, keep) <- zip edges (randoms (mkStdGen (caseSeed + 1))), keep]
      RemoveAll -> edges
    benchGraph = deleteEdges removed fullGraph

-- * Measurement

data CaseResult = CaseResult
  { benchedCase :: BenchCase,
    -- | wall-clock seconds; for a timed-out case, the time limit
    seconds :: Double,
    timedOut :: Bool,
    -- | number of residual alternatives (0 if timed out)
    altCount :: Int
  }

-- | Monotonic wall-clock time in seconds ('GHC.Clock.getMonotonicTime';
-- the one MyPrelude re-exports from unliftio measures nanoseconds).
monotonicSeconds :: IO Double
monotonicSeconds = GHC.Clock.getMonotonicTime

runCase :: Int -> BenchCase -> IO CaseResult
runCase timeoutMs c = do
  t0 <- monotonicSeconds
  result <-
    timeout (timeoutMs * 1000) $
      evaluate . force . toList $
        pathResidualsUnresolvedBy c.startNid c.benchGraph c.path
  t1 <- monotonicSeconds
  pure
    CaseResult
      { benchedCase = c,
        seconds = t1 - t0,
        timedOut = isNothing result,
        altCount = maybe 0 length result
      }

-- * Reporting

percentile :: [Double] -> Double -> Double
percentile sorted p = case sorted of
  [] -> 0
  _ ->
    let i = ceiling (p * fromIntegral (length sorted)) - 1
     in sorted `indexEx` max 0 (min (length sorted - 1) i)

describeCase :: CaseResult -> String
describeCase r =
  unlines
    [ printf
        "  %.3fs%s  seed=%d scenario=%s alternatives=%d"
        r.seconds
        (if r.timedOut then " (TIMED OUT)" else "" :: String)
        c.caseSeed
        (show c.scenario)
        r.altCount,
      "    start: " <> show c.startNid,
      "    path:  " <> show c.path,
      "    graph: " <> show (graphEdges c.benchGraph)
    ]
  where
    c = r.benchedCase

report :: Options -> [CaseResult] -> IO ()
report opts results = do
  let times = sort (map (.seconds) results)
      timeouts = filter (.timedOut) results
      slowest = take opts.top (sortOn (negate . (.seconds)) results)
      total = sum times
      worstAlts = maximumMay (map (.altCount) results)
  printf "cases:        %d\n" (length results)
  printf "total time:   %.3fs\n" total
  printf "mean:         %.6fs\n" (total / fromIntegral (max 1 (length times)))
  printf "p50:          %.6fs\n" (percentile times 0.50)
  printf "p90:          %.6fs\n" (percentile times 0.90)
  printf "p99:          %.6fs\n" (percentile times 0.99)
  printf "max:          %.6fs\n" (percentile times 1.00)
  printf "max #alts:    %d\n" (fromMaybe 0 worstAlts)
  printf "timeouts:     %d\n" (length timeouts)
  unless (null timeouts) do
    putStrLn "\ntimed-out cases:"
    traverse_ (putStr . pack . describeCase) timeouts
  putStrLn "\nslowest cases:"
  traverse_ (putStr . pack . describeCase) slowest

residualsBenchMain :: IO ()
residualsBenchMain = do
  args <- Env.getArgs
  opts <- case parseOptions args of
    Right opts -> pure opts
    Left err -> do
      putStrLn (pack (err <> "\n\n" <> usage))
      exitFailure
  printf
    "running %d cases (config=%s, scenario=%s, timeout=%dms)\n\n"
    opts.cases
    opts.configName
    (show opts.scenarioMode)
    opts.timeoutMs
  results <-
    for [opts.firstSeed .. opts.firstSeed + opts.cases - 1] \s ->
      runCase opts.timeoutMs (benchCase opts s)
  report opts results
