module Executable.GraphEditor.Options
  ( Options (..),
    withOptions,
  )
where

import Models.Command
import Models.Command.Parse
import MyPrelude
import Options.Applicative

data Options = Options
  { graphLocation :: FilePath,
    executeExpression :: Maybe Command,
    createNew :: Bool,
    testOnlyNidGenerationSeed :: Maybe Int,
    testOnlyMonotonicIncreasingDeterministicTime :: Bool,
    -- | for how long each part of the end at SIGTERM or SIGHUP may take, in
    -- microseconds, instead of 'endingAllowance'
    testOnlyEndingAllowance :: Maybe Int,
    -- | whether SIGCHLD is ignored, so that the system reaps the processes
    -- that ge starts: the stop of one ('stopProcess') then fails, and a wait
    -- for the exit status of one that ends by itself never returns (only for
    -- a test whose editor is stopped at an interrupt)
    testOnlyChildrenReapedBySystem :: Bool,
    dryRun :: Bool
  }
  deriving (Show, Generic)

optionsP :: Parser Options
optionsP =
  Options
    <$> strArgument
      ( metavar "<graph directory>"
          <> help "a .g directory with the graph to act on"
      )
    <*> optional
      ( option
          (eitherReader (parseCommand . pack))
          ( long "expression"
              <> short 'e'
              <> help "execute a single command"
              <> metavar "<command>"
          )
      )
    <*> switch (long "new" <> help "create a new graph if one isn't found")
    <*> optional
      ( option
          auto
          ( long "test-only-nid-generation-seed"
              <> help "fixed seed for random number generator; not for use outside of testing because it breaks uniqueness of generated node ids"
              <> metavar "<test-only-nid-generation-seed>"
              <> internal
          )
      )
    <*> switch
      ( long "test-only-monotonic-increasing-deterministic-time"
          <> help "use a deterministic time that increases monotonically"
          <> internal
      )
    <*> optional
      ( option
          auto
          ( long "test-only-ending-allowance"
              <> help "for how long each part of the end at SIGTERM or SIGHUP may take, in microseconds"
              <> metavar "<microseconds>"
              <> internal
          )
      )
    <*> switch
      ( long "test-only-children-reaped-by-the-system"
          <> help "ignore SIGCHLD, so that the system reaps the processes that ge starts: stopping one fails, and waiting for one that ends by itself never returns"
          <> internal
      )
    <*> switch
      ( long "dry-run"
          <> help "print the changes to the graph instead of writing them"
      )

optionsPI :: ParserInfo Options
optionsPI =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Manipulate information graphs."
        <> header "ge - graph editor"
    )

withOptions :: (Options -> IO a) -> IO a
withOptions ge = execParser optionsPI >>= ge
