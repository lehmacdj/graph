{-# LANGUAGE TemplateHaskell #-}

module Executable.GraphEditor.Options
  ( Options,
    withOptions,
  )
where

import Lang.Command
import Lang.Command.Parse
import MyPrelude
import Options.Applicative

data Options = Options
  { _graphLocation :: FilePath,
    _executeExpression :: Maybe Command,
    _createNew :: Bool,
    _testOnlyNidGenerationSeed :: Maybe Int,
    _testOnlyMonotonicIncreasingDeterministicTime :: Bool
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
          (eitherReader parseCommand)
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
