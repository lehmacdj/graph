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
import Control.Arrow (left)

import Data.Validation
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
  | AesonDeserialize String -- ^ deserialization fails

type UserErrors = NonEmpty UserError

instance Show UserError where
  show (OtherError s) = s
  show (IOFail e) = show e
  show (MissingNode nid) = "node " ++ show nid ++ " is missing"
  show (NotSingleton xs) = xs ++ " was expected to be a singleton but wasn't"
  show (WebError uri e) = "couldn't fetch " ++ uri ++ "\n" ++ show e

type ThrowUserError = Error (NonEmpty UserError)

throwString :: Member ThrowUserError effs => String -> Eff effs a
throwString = throw . OtherError

throw :: Member ThrowUserError effs => UserError -> Eff effs a
throw = throwError . (singleton :: UserError -> NonEmpty UserError)

throwIfNothing
  :: Member ThrowUserError effs
  => UserError -> Maybe a -> Eff effs a
throwIfNothing _ (Just x) = pure x
throwIfNothing err Nothing = throw err

throwLeft
  :: Member ThrowUserError effs
  => Either UserError a -> Eff effs a
throwLeft (Right x) = pure x
throwLeft (Left err) = throw err

-- | Trap an IO error in the Eff monad
-- It remains to be seen if this is actually useful, because it requires
-- it to be possible to unwrap Eff in order to escape the IO monad on the outside
trapIOError
  :: forall m effs a. (MonadIO m, Member ThrowUserError effs)
  => IO a -> m (Eff effs a)
trapIOError = liftIO . fmap (throwLeft . left IOFail) . tryIOError

trapIOError'
  :: forall m effs a. (MonadIO m, Member ThrowUserError effs, LastMember m effs)
  => IO a -> Eff effs a
trapIOError' = join . trapIOError

printErrors
  :: (MonadIO m, LastMember m effs)
  => Eff (ThrowUserError ': effs) () -> Eff effs ()
printErrors = (`handleError` printer) where
  printer errs = liftIO $ mapM_ eprint errs

errorToLeft
  :: Show e => Eff (Error e : effs) a -> Eff effs (Either String a)
errorToLeft = (`handleError` \e -> pure (Left (show e))) . fmap Right

errorToNothing
  :: Eff (Error e : effs) a -> Eff effs (Maybe a)
errorToNothing = (`handleError` const (pure Nothing)) . fmap Just
