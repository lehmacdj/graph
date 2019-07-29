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

data Error
  = UE String
  | IOFail IOError
  deriving (Eq)

newtype Errors = Errors (NonEmpty Error)

instance Show Error where
  show (UE s) = s
  show (IOFail e) = show e

type E a = Validation (NonEmpty Error) a

throwUE :: String -> E a
throwUE s = _Failure # (UE s :| [])

throwE :: Error -> E a
throwE e = _Failure # (e :| [])

maybeToE :: Error -> Maybe a -> E a
maybeToE e Nothing = throwE e
maybeToE _ (Just x) = pure x

ioToE :: IO a -> IO (E a)
ioToE = fmap (view revalidate . left ((:| []) . IOFail)) . tryIOError

putStrErr :: String -> IO ()
putStrErr = hPutStrLn stderr

displayErrOrGet :: MonadIO m => E a -> (a -> m ()) -> m ()
displayErrOrGet (Failure e) _ = liftIO $ mapM_ (putStrErr . show) e
displayErrOrGet (Success x) f = f x

displayErr :: MonadIO m => E a -> m ()
displayErr e = displayErrOrGet e (const (pure ()))

ioBindE :: E a -> (a -> IO (E b)) -> IO (E b)
ioBindE (Failure e) _ = pure $ _Failure # e
ioBindE (Success x) f = f x

