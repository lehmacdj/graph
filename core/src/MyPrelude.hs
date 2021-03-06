module MyPrelude
  ( module MyPrelude,
    module ClassyPrelude,
    module Polysemy,
    module Control.DeepSeq,
  )
where

import ClassyPrelude
import Control.DeepSeq
import Control.Lens hiding (op)
import qualified Data.Set as Set
import Polysemy
import Polysemy.Error
import System.IO

-- | natural transformation
type (~>) f g = forall x. f x -> g x

-- | execute a computation only if it is Just
withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) f = f x
withJust Nothing _ = pure ()

-- | Copied from cabal codebase
toSetOf :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (Const . singleton) s)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet = Set.map

-- | do something when a mono is NonNull
whenNonNull ::
  (MonoFoldable mono, Applicative f) =>
  mono ->
  (NonNull mono -> f ()) ->
  f ()
whenNonNull mono f = case fromNullable mono of
  Nothing -> pure ()
  Just xs -> f xs

-- | generalized foldl1 monadically
foldlM1 ::
  (IsSequence mono, Monad m) =>
  (Element mono -> Element mono -> m (Element mono)) ->
  NonNull mono ->
  m (Element mono)
foldlM1 f m = uncurry (foldlM f) (splitFirst m)

-- | Taken from extras-1.6.17
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
  where
    f x xs = do x' <- op x; if null x' then xs else do { xs' <- xs; return $ x' ++ xs' }

-- | Apply a function n times to a given value
-- implementation taken from protolude
applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f :: [a -> a])

describe :: String -> Maybe a -> Either String a
describe s Nothing = Left s
describe _ (Just x) = Right x

forgetLeft :: Either e a -> Maybe a
forgetLeft (Right x) = Just x
forgetLeft (Left _) = Nothing

-- # IO functions for stderr

eputStr :: String -> IO ()
eputStr = hPutStrLn stderr

eprint :: Show a => a -> IO ()
eprint = eputStr . show

ioErrorToMaybe :: IO a -> IO (Maybe a)
ioErrorToMaybe = (`ClassyPrelude.catch` ioHandler) . (Just <$>)
  where
    ioHandler :: IOError -> IO (Maybe a)
    ioHandler = pure . const Nothing

ignoreIOError :: IO a -> IO ()
ignoreIOError a = ioErrorToMaybe a $> ()

-- # Effect utilities

-- | The identity funciton
withEffects :: forall effs a. Sem effs a -> Sem effs a
withEffects = id

-- | Handle error without allowing error to be present in resulting computation.
-- Added in migration from freer-simple to polysemy, soft deprecated, consider
-- finding a sufficient replacement in the near future
handleError :: Sem (Error e : effs) a -> (e -> Sem effs a) -> Sem effs a
handleError action handler = do
  result <- runError action
  case result of
    Right x -> pure x
    Left e -> handler e
