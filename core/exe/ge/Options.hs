{-# LANGUAGE TemplateHaskell #-}

module Options
  ( Options,
    graphLocation,
    executeExpression,
    withOptions,
  )
where

import Control.Lens (makeLenses)
import Lang.Command
import Lang.Command.Parse
import MyPrelude
import Options.Applicative

data Options = Options
  { _graphLocation :: FilePath,
    _executeExpression :: Maybe Command
  }
  deriving (Show)

makeLenses ''Options

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
