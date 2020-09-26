{-# LANGUAGE NoImplicitPrelude #-}

module MyPrelude
  ( module MyPrelude,
    module ClassyPrelude,
    module Control.Monad.Freer,
  )
where

import ClassyPrelude
import Control.Lens hiding (op)
import Control.Monad.Freer
import System.IO

-- | execute a computation only if it is Just
withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) f = f x
withJust Nothing _ = pure ()

-- | Copied from cabal codebase
toSetOf :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (\x -> Const (singleton x)) s)

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

-- # IO functions for stderr

eputStr :: String -> IO ()
eputStr = hPutStrLn stderr

eprint :: Show a => a -> IO ()
eprint = eputStr . show

-- # Effect utilities

-- | The identity funciton
withEffect :: forall effs a. Eff effs a -> Eff effs a
withEffect = id
