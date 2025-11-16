{-# LANGUAGE TemplateHaskell #-}

-- | Simple Readline effect for effectful, wrapping haskeline
module Effect.Readline
  ( Readline,
    readline,
    readlineWithDefault,
    outputStrLn,
    runReadlineIO,
    module System.Console.Haskeline,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude hiding (Reader)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

data Readline :: Effect where
  Readline :: Maybe String -> Readline m (Maybe String)
  OutputStrLn :: String -> Readline m ()

makeEffect ''Readline

readlineWithDefault :: (Readline :> es) => String -> Eff es (Maybe String)
readlineWithDefault = readline . Just

runReadlineIO :: (IOE :> es) => Eff (Readline : es) a -> Eff es a
runReadlineIO action = do
  liftIO $ runInputT defaultSettings $ runReadlineInputT action

runReadlineInputT :: (IOE :> es) => Eff (Readline : es) a -> InputT IO a
runReadlineInputT = interpret $ \_ -> \case
  Readline prompt -> getInputLine (fromMaybe "> " prompt)
  OutputStrLn s -> outputStrLn s
