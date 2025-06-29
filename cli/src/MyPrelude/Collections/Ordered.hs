{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyPrelude.Collections.Ordered
  ( module X,
    module MyPrelude.Collections.Ordered,
  )
where

import ClassyPrelude
import Data.Map.Ordered as X (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Set.Ordered as X (OSet)
import Data.Set.Ordered qualified as OSet
import Language.Haskell.TH.Syntax (Code (Code, examineCode), Exp (AppE, VarE), Lift (..), ModName (ModName), Name (Name), NameFlavour (NameG), NameSpace (VarName), OccName (OccName), PkgName (PkgName), TExp (TExp))

orderedContainersName :: String -> String -> Name
orderedContainersName moduleName occName =
  Name
    (OccName occName)
    (NameG VarName (PkgName "ordered-containers") (ModName moduleName))

-- * OSet

instance (NFData a) => NFData (OSet a) where
  rnf = rnf . toList

instance (Lift a) => Lift (OSet a) where
  liftTyped x = Code $ do
    TExp elems <- (liftTyped . toList $ x).examineCode
    pure . TExp $
      AppE
        (VarE (orderedContainersName "Data.Set.Ordered" "fromList"))
        elems

type instance Element (OSet a) = a

instance MonoFoldable (OSet a)

deriving via (OSet.Bias OSet.L (OSet a)) instance (Ord a) => Semigroup (OSet a)

deriving via (OSet.Bias OSet.L (OSet a)) instance (Ord a) => Monoid (OSet a)

instance GrowingAppend (OSet a)

instance (Ord a) => SetContainer (OSet a) where
  type ContainerKey (OSet a) = a
  member = OSet.member
  notMember = OSet.notMember
  union = (OSet.|<>)
  difference = (OSet.\\)
  intersection = (OSet.|/\)
  keys = toList

instance (Ord a) => IsSet (OSet a) where
  insertSet = flip (OSet.|>)
  deleteSet = OSet.delete
  singletonSet = OSet.singleton
  setFromList = OSet.fromList
  setToList = toList

-- * OMap

instance (Ord k, NFData k, NFData v) => NFData (OMap k v) where
  rnf = rnf . mapToList

instance (Lift k, Lift v) => Lift (OMap k v) where
  liftTyped x = Code $ do
    TExp assocs <- (liftTyped . OMap.assocs $ x).examineCode
    pure . TExp $
      AppE
        (VarE (orderedContainersName "Data.Map.Ordered" "fromList"))
        assocs

type instance Element (OMap k v) = v

instance MonoFoldable (OMap k v)

instance (Ord k) => Semigroup (OMap k v) where
  (<>) = (OMap.|<>)

instance (Ord k) => Monoid (OMap k v) where
  mempty = OMap.empty

instance GrowingAppend (OMap k v)

instance (Ord k) => SetContainer (OMap k v) where
  type ContainerKey (OMap k v) = k
  member = OMap.member
  notMember = OMap.notMember
  union = (OMap.|<>)
  difference = (OMap.\\)
  intersection = (OMap.|/\)
  keys = map fst . OMap.assocs

instance MonoFunctor (OMap k v)

instance (Ord k) => MonoTraversable (OMap k v)

instance (Ord k) => PolyMap (OMap k) where
  differenceMap = (OMap.\\)
  intersectionMap = (OMap.|/\)
  intersectionWithMap f = OMap.intersectionWith (const f)

instance BiPolyMap OMap where
  type BPMKeyConstraint OMap key = Ord key
  mapKeysWith merge fkey =
    unionsWith merge
      . fmap (OMap.singleton . first fkey)
      . mapToList

-- | insertMap is equivalent to OMap's @(|>)@, i.e. the map remembers the order
-- based on the first time a key is inserted, but the last value for that key.
-- This is consistent with the behavior of @OMap.fromList@.
instance (Ord k) => IsMap (OMap k v) where
  type MapValue (OMap k v) = v
  lookup = OMap.lookup
  insertMap = curry (flip (OMap.|>))
  deleteMap = OMap.delete
  singletonMap = curry OMap.singleton
  mapFromList = OMap.fromList
  mapToList = OMap.assocs

instance (Ord k) => HasKeysSet (OMap k v) where
  type KeySet (OMap k v) = OSet k
  keysSet = setFromList . map fst . OMap.assocs
