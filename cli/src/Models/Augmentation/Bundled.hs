{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.Augmentation.Bundled
  ( lookupA,
    lensA,
    injDefaultA,
    replaceA,
    updateA,
    insertA,
    bundled,
    bundling,
    (##),
    (#|),
    Bundled (..),
    spec_bundledFieldAccess,
    spec_bundledShowable,
  )
where

import Data.Type.Bool (If)
import Data.TypeMap.Internal.Vector (TypeVector (..))
import Data.TypeMap.Static qualified as TMap
import Data.TypeMap.Vector qualified as TMap
import Data.Vector qualified as Vector
import GHC.Base (Any)
import GHC.Generics (C, D, M1, Meta (..), S, (:*:))
import GHC.Records (HasField (..))
import Models.Common
import Models.Node (DefaultAugmentation (..))
import MyPrelude
import Unsafe.Coerce (unsafeCoerce)
import Utils.Testing

type family Indicies (ts :: [Type]) :: [(Type, Type)] where
  Indicies '[] = '[]
  Indicies (t ': ts) = '(t, t) ': Indicies ts

type role Bundled nominal

newtype Bundled ts = Bundled (TMap.TypeVector (Indicies ts))
  deriving stock (Generic)

lookupA ::
  forall t ts.
  (KnownNat (TMap.Index t (Indicies ts))) =>
  Bundled ts ->
  TMap.Lookup t (Indicies ts)
lookupA (Bundled v) = TMap.index @t @(Indicies ts) v

lensA ::
  forall t ts.
  (KnownNat (TMap.Index t (Indicies ts))) =>
  Lens' (Bundled ts) (TMap.Lookup t (Indicies ts))
lensA = lens (lookupA @t) (flip (replaceA @t))

replaceA ::
  forall t ts.
  (KnownNat (TMap.Index t (Indicies ts))) =>
  TMap.Lookup t (Indicies ts) ->
  Bundled ts ->
  Bundled ts
replaceA a (Bundled v) = Bundled $ updateTypeVector v
  where
    updateTypeVector (TypeVector m) =
      TypeVector $ m Vector.// [(na, unsafeCoerce a :: Any)]
    na = fromInteger (natVal (Proxy @(TMap.Index t (Indicies ts)))) :: Int

updateA ::
  forall t ts.
  (KnownNat (TMap.Index t (Indicies ts))) =>
  (TMap.Lookup t (Indicies ts) -> TMap.Lookup t (Indicies ts)) ->
  Bundled ts ->
  Bundled ts
updateA f b = replaceA @t (f (lookupA @t b)) b

insertA ::
  forall t ts.
  t ->
  Bundled ts ->
  Bundled (t : ts)
insertA a (Bundled v) = Bundled $ TMap.cons @t a v

infixr 5 `insertA`

bundled :: forall a. a -> Bundled '[a]
bundled a = Bundled $ TMap.cons @a a TMap.empty
{-# INLINE bundled #-}

bundling :: (nid -> Sem r a) -> nid -> Sem r (Bundled '[a])
bundling f = fmap bundled . f

injDefaultA ::
  forall t ts.
  ( KnownNat (TMap.Index t (Indicies ts)),
    DefaultAugmentation (Bundled ts)
  ) =>
  TMap.Lookup t (Indicies ts) ->
  Bundled ts
injDefaultA t = replaceA @t t defaultAugmentation

(#|) ::
  forall a b m nid.
  (Monad m) =>
  (nid -> m a) ->
  (nid -> m b) ->
  (nid -> m (Bundled '[a, b]))
f #| g = \nid -> do
  a <- f nid
  b <- g nid
  pure $ a `insertA` bundled b

infixr 5 #|

-- | Together with #| this allows combining two functions that produce
-- augmentations, e.g. `fetchSomething ## fetchTags #| fetchTimestamp`
(##) ::
  forall a ts m nid.
  (Monad m) =>
  (nid -> m a) ->
  (nid -> m (Bundled ts)) ->
  (nid -> m (Bundled (a ': ts)))
f ## g = \nid -> do
  a <- f nid
  Bundled as <- g nid
  pure $ Bundled $ TMap.cons @a a as

infixr 5 ##

-- * ShowableAugmentation

instance ShowableAugmentation (Bundled '[]) where
  augmentationProperties _ = mempty

instance
  forall t ts.
  (ShowableAugmentation t, ShowableAugmentation (Bundled ts)) =>
  ShowableAugmentation (Bundled (t : ts))
  where
  augmentationProperties b@(Bundled v) =
    augmentationProperties (lookupA @t b)
      <> augmentationProperties (Bundled @ts (tailTypeVector v))
    where
      tailTypeVector ::
        TypeVector ('(t, t) ': Indicies ts) -> TypeVector (Indicies ts)
      tailTypeVector (TypeVector m) = TypeVector (Vector.tail m)

spec_bundledShowable :: Spec
spec_bundledShowable = do
  it "shows all properties" $
    augmentationProperties bundledExample1
      `shouldBe` [ (Just "a", "42"),
                   (Just "b", "hello"),
                   (Just "c", "True"),
                   (Just "d", "3.14")
                 ]
  it "duplicate properties all appear in order" $
    augmentationProperties bundledExample2
      `shouldBe` [ (Just "a", "1"),
                   (Just "b", "hello"),
                   (Just "a", "2")
                 ]

-- * DefaultAugmentation

instance DefaultAugmentation (Bundled '[]) where
  defaultAugmentation = Bundled TMap.empty

instance
  forall t ts.
  (DefaultAugmentation t, DefaultAugmentation (Bundled ts)) =>
  DefaultAugmentation (Bundled (t : ts))
  where
  defaultAugmentation =
    defaultAugmentation @t
      `insertA` defaultAugmentation @(Bundled ts)

-- * Eq

instance Eq (Bundled '[]) where
  _ == _ = True

instance
  forall t ts.
  (Eq t, Eq (Bundled ts)) =>
  Eq (Bundled (t : ts))
  where
  b1 == b2 =
    (lookupA @t b1 == lookupA @t b2)
      && (Bundled @ts (tailTypeVector v1) == Bundled @ts (tailTypeVector v2))
    where
      Bundled v1 = b1
      Bundled v2 = b2
      tailTypeVector ::
        TypeVector ('(t, t) ': Indicies ts) -> TypeVector (Indicies ts)
      tailTypeVector (TypeVector m) = TypeVector (Vector.tail m)

-- * NFData

instance NFData (Bundled '[]) where
  rnf = rwhnf

instance forall t ts. (NFData t, NFData (Bundled ts)) => NFData (Bundled (t : ts)) where
  rnf b = rnf (lookupA @t b) `seq` rnf (Bundled @ts (tailTypeVector v))
    where
      Bundled v = b
      tailTypeVector ::
        TypeVector ('(t, t) ': Indicies ts) -> TypeVector (Indicies ts)
      tailTypeVector (TypeVector m) = TypeVector (Vector.tail m)

-- * HasField

instance
  forall (symbol :: Symbol) t (ts :: [Type]) a.
  ( HasField symbol (TMap.Lookup t (Indicies ts)) a,
    HasAugmentationWithField symbol t a ts,
    KnownNat (TMap.Index t (Indicies ts))
  ) =>
  HasField symbol (Bundled ts) a
  where
  getField b = getField @symbol (lookupA @t b)

-- | Type family to find which type in the list has the given field
-- Uses Generic introspection to check field presence at type level
type family FindTypeWithField (symbol :: Symbol) (ts :: [Type]) :: Type where
  FindTypeWithField symbol (t ': ts) =
    If (RepHasField symbol (Rep t)) t (FindTypeWithField symbol ts)

-- | Search the generic representation for a selector with the given name
type family RepHasField (symbol :: Symbol) (rep :: Type -> Type) :: Bool where
  RepHasField symbol (M1 D _ f) = RepHasField symbol f
  RepHasField symbol (M1 C _ f) = RepHasField symbol f
  RepHasField symbol (M1 S ('MetaSel ('Just symbol) _ _ _) _) = 'True
  RepHasField symbol (M1 S ('MetaSel ('Just other) _ _ _) _) = 'False
  RepHasField symbol (f :*: g) = RepHasField symbol f || RepHasField symbol g
  RepHasField symbol _ = 'False

type family (a :: Bool) || (b :: Bool) :: Bool where
  'True || _ = 'True
  'False || b = b

class
  ( HasField symbol t a,
    t ~ FindTypeWithField symbol ts
  ) =>
  HasAugmentationWithField (symbol :: Symbol) t a (ts :: [Type])
    | symbol ts -> t a

instance
  ( t ~ FindTypeWithField symbol ts,
    HasField symbol t a
  ) =>
  HasAugmentationWithField symbol t a ts

spec_bundledFieldAccess :: Spec
spec_bundledFieldAccess = do
  it "can access a" $ bundledExample1.a `shouldBe` 42
  it "can access b" $ bundledExample1.b `shouldBe` ("hello" :: Text)
  it "can access c" $ bundledExample1.c `shouldBe` True
  it "can access d" $ bundledExample1.d `shouldBe` (3.14 :: Double)
  it "accesses first a in duplicated" $ bundledExample2.a `shouldBe` 1
  it "can access any field from a type with multiple" $
    bundledExample2.b `shouldBe` ("hello" :: Text)

-- * examples for tests

newtype TestA = TestA {a :: Int}
  deriving stock (Generic)

instance ShowableAugmentation TestA where
  augmentationProperties (TestA a) = [(Just "a", tshow a)]

newtype TestB = TestB {b :: Text}
  deriving stock (Generic)

instance ShowableAugmentation TestB where
  augmentationProperties (TestB b) = [(Just "b", b)]

newtype TestC = TestC {c :: Bool}
  deriving stock (Generic)

instance ShowableAugmentation TestC where
  augmentationProperties (TestC c) = [(Just "c", tshow c)]

newtype TestD = TestD {d :: Double}
  deriving stock (Generic)

instance ShowableAugmentation TestD where
  augmentationProperties (TestD d) = [(Just "d", tshow d)]

data TestAB = TestAB {a :: Int, b :: Text}
  deriving stock (Generic)

instance ShowableAugmentation TestAB where
  augmentationProperties (TestAB a b) =
    [(Just "a", tshow a), (Just "b", b)]

bundledExample1 :: Bundled '[TestA, TestB, TestC, TestD]
bundledExample1 =
  TestA 42
    `insertA` TestB "hello"
    `insertA` TestC True
    `insertA` bundled (TestD 3.14)

bundledExample2 :: Bundled '[TestAB, TestA]
bundledExample2 = TestAB 1 "hello" `insertA` bundled (TestA 2)
