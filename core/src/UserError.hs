module UserError
  ( module UserError,
    module Data.Validation,
    NonEmpty (..),
    left,
    (#),
  )
where

import Control.Arrow (left)
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Validation
import Graph (NID)
import MyPrelude
import Network.HTTP.Conduit (HttpException)
import Polysemy.Error

data UserError
  = OtherError String
  | IOFail IOError
  | MissingNode NID
  | -- | report that the thing that has a given
    -- representation wasn't a singleton
    NotSingleton String
  | -- | url and the error that occured
    WebError String HttpException
  | -- | deserialization fails
    AesonDeserialize String

type UserErrors = NonEmpty UserError

instance Show UserError where
  show (OtherError s) = s
  show (IOFail e) = show e
  show (MissingNode nid) = "node " ++ show nid ++ " is missing"
  show (NotSingleton xs) = xs ++ " was expected to be a singleton but wasn't"
  show (WebError uri e) = "couldn't fetch " ++ uri ++ "\n" ++ show e
  show (AesonDeserialize e) = "failed to deserialize JSON: " ++ e

type ThrowUserError = Error (NonEmpty UserError)

throwString :: Member ThrowUserError effs => String -> Sem effs a
throwString = UserError.throw . OtherError

throw :: Member ThrowUserError effs => UserError -> Sem effs a
throw = Polysemy.Error.throw . (singleton :: UserError -> NonEmpty UserError)

throwIfNothing ::
  Member ThrowUserError effs =>
  UserError ->
  Maybe a ->
  Sem effs a
throwIfNothing _ (Just x) = pure x
throwIfNothing err Nothing = UserError.throw err

throwLeft ::
  Member ThrowUserError effs =>
  Either UserError a ->
  Sem effs a
throwLeft (Right x) = pure x
throwLeft (Left err) = UserError.throw err

-- | Trap an IO error in the Sem monad
-- It remains to be seen if this is actually useful, because it requires
-- it to be possible to unwrap Sem in order to escape the IO monad on the outside
trapIOError ::
  forall m effs a.
  (MonadIO m, Member ThrowUserError effs) =>
  IO a ->
  m (Sem effs a)
trapIOError = liftIO . fmap (throwLeft . left IOFail) . tryIOError

trapIOError' ::
  forall effs a.
  (Member (Embed IO) effs, Member ThrowUserError effs) =>
  IO a ->
  Sem effs a
trapIOError' = join . trapIOError

printErrors ::
  Member (Embed IO) effs =>
  Sem (ThrowUserError ': effs) () ->
  Sem effs ()
printErrors = (`handleError` printer)
  where
    printer errs = liftIO $ mapM_ eprint errs

errorToLeft ::
  Show e => Sem (Error e : effs) a -> Sem effs (Either String a)
errorToLeft = (`handleError` pure . Left . show) . fmap Right

errorToNothing ::
  Sem (Error e : effs) a -> Sem effs (Maybe a)
errorToNothing = (`handleError` const (pure Nothing)) . fmap Just
