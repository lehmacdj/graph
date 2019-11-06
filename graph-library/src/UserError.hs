{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UserError
  ( module UserError
  , module Data.Validation
  , NonEmpty(..)
  , left
  , (#)
  ) where

import MyPrelude

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

import Graph (NID)

data UserError
  = OtherError String
  | IOFail IOError
  | MissingNode NID
  | NotSingleton String -- ^ report that the thing that has a given
                        -- representation wasn't a singleton
  | WebError String HttpException -- ^ url and the error that occured

instance Show Err where
  show (OtherError s) = s
  show (IOFail e) = show e
  show (MissingNode nid) = "node " ++ show nid ++ " is missing"
  show (NotSingleton xs) = xs ++ " was expected to be a singleton but wasn't"
  show (WebError uri e) = "couldn't fetch " ++ uri ++ "\n" ++ show e

type ThrowUserError = Error (NonEmpty UserError)

throwString :: String -> E a
throwString = throw . OtherError

throw :: Err -> E a
throw e = throwError . singleton

throwIfNothing
  :: Member ThrowUserError effs
  => UserError -> Maybe a -> Eff effs a
throwIfNothing _ (Just x) = pure x
throwIfNothing err Nothing = throw err

throwLeft
  :: Member ThrowUserError effs
  => Either UserError a -> Eff effs a
throwLeft (Right x) = pure x
throw (Left err) = throw err

-- | Trap an IO error in the Eff monad
-- It remains to be seen if this is actually useful, because it requires
-- it to be possible to unwrap Eff in order to escape the IO monad on the outside
trapIOError
  :: Member ThrowUserError effs
  => IO a -> IO (Eff effs a)
trapIOError = fmap (throwLeft . left (singleton . IOFail)) . tryIOError

putStrErr :: String -> IO ()
putStrErr = hPutStrLn stderr

printUserError :: Show a => a -> IO ()
printUserError = putStrErr . show

displayError
  :: (MonadIO m, LastMember m effs)
  => Eff (Error UserError ': effs) () -> Eff effs ()
displayErrF = (`handleError` printer) where
  printer err = liftIO $ printUserError err
