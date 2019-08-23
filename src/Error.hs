{-# LANGUAGE LambdaCase #-}

module Error
  ( module Error
  , module Data.Validation
  , NonEmpty(..)
  , left
  , (#)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Lens
import System.IO
import System.IO.Error
import Control.Arrow (left)

import Data.Validation
import Control.Monad.IO.Class
import Control.Monad.Freer
import Control.Monad.Freer.Error

import Network.HTTP.Conduit (HttpException)

import Graph (Id)

data Err
  = UE String
  | IOFail IOError
  | MissingNode Id
  | NotSingleton String -- ^ report that the thing that has a given
                        -- representation wasn't a singleton
  | WebError String HttpException -- ^ url and the error that occured

type Errors = NonEmpty Err

instance Show Err where
  show (UE s) = s
  show (IOFail e) = show e
  show (MissingNode nid) = "node " ++ show nid ++ " is missing"
  show (NotSingleton xs) = xs ++ " was expected to be a singleton but wasn't"
  show (WebError uri e) = "couldn't fetch " ++ uri ++ "\n" ++ show e

type E a = Validation (NonEmpty Err) a

throwUE :: String -> E a
throwUE s = _Failure # (UE s :| [])

throwE :: Err -> E a
throwE e = _Failure # (e :| [])

rethrowE :: Member (Error Errors) effs => E a -> Eff effs a
rethrowE = \case
  Success x -> pure x
  Failure e' -> throwError e'

maybeToE :: Err -> Maybe a -> E a
maybeToE e Nothing = throwE e
maybeToE _ (Just x) = pure x

ioToE :: IO a -> IO (E a)
ioToE = fmap (view revalidate . left ((:| []) . IOFail)) . tryIOError

putStrErr :: String -> IO ()
putStrErr = hPutStrLn stderr

printErr :: Show a => a -> IO ()
printErr = putStrErr . show

displayErrOrGet :: MonadIO m => E a -> (a -> m ()) -> m ()
displayErrOrGet (Failure e) _ = liftIO $ mapM_ (putStrErr . show) e
displayErrOrGet (Success x) f = f x

displayErr :: MonadIO m => E a -> m ()
displayErr e = displayErrOrGet e (const (pure ()))

displayErrF
  :: (MonadIO m, LastMember m effs)
  => Eff (Error Errors ': effs) () -> Eff effs ()
displayErrF = (`handleError` printer) where
  printer err = liftIO $ printErr err

ioBindE :: E a -> (a -> IO (E b)) -> IO (E b)
ioBindE (Failure e) _ = pure $ _Failure # e
ioBindE (Success x) f = f x

eToMaybe :: E a -> Maybe a
eToMaybe = \case
  Success a -> Just a
  Failure _ -> Nothing

isFailed :: E a -> Bool
isFailed = \case
  Failure _ -> True
  Success _ -> False
