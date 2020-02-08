{-# LANGUAGE TemplateHaskell #-}

module Effect.Execute where

import Control.Monad.Freer.TH

import System.Process.Typed

data Execute r where
  ExecuteProcess :: String -> [String] -> Execute ()
makeEffect ''Execute

interpretExecuteIO
  :: forall m effs.
    ( MonadIO m
    , MemberLast m effs
    )
  => Eff (Execute : effs) ~> Eff effs
interpretExecuteIO = interpret $ \case
  ExecuteProcess c args -> do
