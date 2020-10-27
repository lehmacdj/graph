{-# LANGUAGE NoImplicitPrelude #-}

module Effect.NodeLocated
  ( module Effect.NodeLocated,
    module Control.Monad.Freer.Writer,
    module Control.Monad.Freer.Reader,
  )
where

import ClassyPrelude hiding (Reader, ask)
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Graph (NID)

type GetLocation = Reader NID

type SetLocation = Writer NID

currentLocation :: Member GetLocation effs => Eff effs NID
currentLocation = ask

changeLocation :: Member SetLocation effs => NID -> Eff effs ()
changeLocation = tell

-- | Implementation mostly taken from
-- https://hackage.haskell.org/package/freer-0.2.4.1/docs/src/Control-Monad-Freer-StateRW.html
runLocable :: Eff (SetLocation : GetLocation : effs) a -> NID -> Eff effs a
runLocable m s = fst <$> loop s m
  where
    loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w, s)
    loop s' (Val x) = return (x, s')
    loop s' (E u q) = case decomp u of
      Right (Tell o) -> k o ()
      Left u' -> case decomp u' of
        Right Ask -> k s' s'
        Left u'' -> E u'' (tsingleton (k s'))
      where
        k s'' = qComp q (loop s'')
