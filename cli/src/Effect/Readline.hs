{-# LANGUAGE TemplateHaskell #-}

-- | Simple Readline effect for effectful, wrapping haskeline
module Effect.Readline
  ( Readline,
    readline,
    readlineWithDefault,
    getInputLine,
    outputStrLn,
    handleInterrupt,
    withInterrupt,
    runReadlineIO,
    module System.Console.Haskeline,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude hiding (Reader)
import System.Console.Haskeline (InputT, Settings, defaultSettings, runInputT)
import System.Console.Haskeline qualified as H

data Readline :: Effect where
  Readline :: Maybe String -> Readline m (Maybe String)
  OutputStrLn :: String -> Readline m ()
  GetInputLine :: String -> Readline m (Maybe String)
  HandleInterrupt :: Eff es a -> Eff es a -> Readline m a
  WithInterrupt :: Eff es a -> Readline m a

makeEffect ''Readline

readlineWithDefault :: (Readline :> es) => String -> Eff es (Maybe String)
readlineWithDefault = readline . Just

runReadlineIO :: (IOE :> es) => Eff (Readline : es) a -> Eff es a
runReadlineIO action = do
  liftIO $ runInputT defaultSettings $ runReadlineInputT action

runReadlineInputT :: forall es a. Eff (Readline : es) a -> InputT IO a
runReadlineInputT = error "runReadlineInputT not yet properly implemented for complex cases"

-- For now, a simpler runner that works for basic cases
runEchoReadline :: (IOE :> es) => Eff (Readline : es) a -> Eff es a
runEchoReadline = interpret $ \_ -> \case
  Readline prompt -> liftIO $ H.runInputT H.defaultSettings $ H.getInputLine (fromMaybe "> " prompt)
  OutputStrLn s -> liftIO $ putStrLn s
  GetInputLine prompt -> liftIO $ H.runInputT H.defaultSettings $ H.getInputLine prompt
  HandleInterrupt onInterrupt action -> do
    -- For now, just run the action (TODO: implement proper interrupt handling)
    unsafeEff_ $ H.runInputT H.defaultSettings $ H.handleInterrupt (pure ()) (pure ())
    action
  WithInterrupt action -> do
    -- For now, just run the action (TODO: implement proper interrupt handling)
    action
