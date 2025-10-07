module MyPrelude.Conc
  ( ipooledMapConcurrentlyN,
    ipooledMapConcurrently,
    ipooledForConcurrentlyN,
    ipooledForConcurrently,
    ipooledMapConcurrentlyN_,
    ipooledMapConcurrently_,
    ipooledForConcurrentlyN_,
    ipooledForConcurrently_,
  )
where

import ClassyPrelude
import Control.Lens (FoldableWithIndex, withIndex)
import Control.Lens.Combinators (mapMOf, unsafePartsOf)
import Control.Lens.Indexed (TraversableWithIndex (..), itoList, itraversed)

preservingIndex :: (Functor m) => (i -> a -> m b) -> (i, a) -> m (i, b)
preservingIndex f (i, a) = (i,) <$> f i a

-- | Converts a function that works on lists to a function that works on indexed
-- traverals preserving the index. See e.g. 'ipooledMapConcurrentlyN'.
usingIndex ::
  (Monad m, TraversableWithIndex i t) =>
  ([(i, a)] -> m [(i, b)]) ->
  (t a -> m (t b))
usingIndex = mapMOf (unsafePartsOf (itraversed . withIndex))

-- | Similar to 'pooledMapConcurrentlyN' but with an index.
ipooledMapConcurrentlyN ::
  forall i t m a b.
  (MonadUnliftIO m, TraversableWithIndex i t) =>
  Int ->
  (i -> a -> m b) ->
  t a ->
  m (t b)
ipooledMapConcurrentlyN numProcs f =
  usingIndex (pooledMapConcurrentlyN numProcs (preservingIndex f))

-- | Similar to 'pooledMapConcurrently' but with an index.
ipooledMapConcurrently :: (MonadUnliftIO m, TraversableWithIndex i t) => (i -> a -> m b) -> t a -> m (t b)
ipooledMapConcurrently f = usingIndex (pooledMapConcurrently (preservingIndex f))

-- | Similar to 'pooledForConcurrentlyN' but with an index.
ipooledForConcurrentlyN ::
  (MonadUnliftIO m, TraversableWithIndex i t) =>
  Int ->
  t a ->
  (i -> a -> m b) ->
  m (t b)
ipooledForConcurrentlyN numProcs = flip (ipooledMapConcurrentlyN numProcs)

-- | Similar to 'pooledForConcurrently' but with an index.
ipooledForConcurrently :: (MonadUnliftIO m, TraversableWithIndex i t) => t a -> (i -> a -> m b) -> m (t b)
ipooledForConcurrently = flip ipooledMapConcurrently

-- | Similar to 'pooledMapConcurrentlyN_' but with an index.
ipooledMapConcurrentlyN_ ::
  (MonadUnliftIO m, FoldableWithIndex i t) =>
  Int ->
  (i -> a -> m b) ->
  t a ->
  m ()
ipooledMapConcurrentlyN_ numProcs f xs =
  pooledMapConcurrentlyN_ numProcs (uncurry f) (itoList xs)

-- | Similar to 'pooledMapConcurrently_' but with an index.
ipooledMapConcurrently_ :: (MonadUnliftIO m, FoldableWithIndex i t) => (i -> a -> m b) -> t a -> m ()
ipooledMapConcurrently_ f xs =
  pooledMapConcurrently_ (uncurry f) (itoList xs)

-- | Similar to 'pooledForConcurrentlyN_' but with an index.
ipooledForConcurrentlyN_ ::
  (MonadUnliftIO m, FoldableWithIndex i t) =>
  Int ->
  t a ->
  (i -> a -> m b) ->
  m ()
ipooledForConcurrentlyN_ numProcs = flip (ipooledMapConcurrentlyN_ numProcs)

-- | Similar to 'pooledForConcurrently_' but with an index.
ipooledForConcurrently_ :: (MonadUnliftIO m, FoldableWithIndex i t) => t a -> (i -> a -> m b) -> m ()
ipooledForConcurrently_ = flip ipooledMapConcurrently_
