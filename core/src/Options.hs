{-# LANGUAGE TemplateHaskell #-}

module Options
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
    _createNew :: Bool
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
