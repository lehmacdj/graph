-- | Common exports for using Vary's polymorphic variants
module MyPrelude.Vary where

import ClassyPrelude
import Data.Kind (Type)
import Vary (Vary, (:|))
import Vary qualified
import Vary.Utils (Mappable, Subset)

fromVary :: forall a (l :: [Type]). (a :| l) => Vary l -> Maybe a
fromVary = Vary.into

fromVaryOnly :: Vary '[a] -> a
fromVaryOnly = Vary.intoOnly

vary :: forall a (l :: [Type]). (a :| l) => a -> Vary l
vary = Vary.from

onVary ::
  forall a b (l :: [Type]).
  (a -> b) ->
  (Vary l -> b) ->
  Vary (a ': l) ->
  b
onVary = Vary.on

varyExhausted :: Vary ('[] :: [Type]) -> anything
varyExhausted = Vary.exhaustiveCase

varyDefault :: forall a (l :: [Type]). a -> Vary l -> a
varyDefault = Vary.defaultCase

varyToEither :: forall a (as :: [Type]). Vary (a ': as) -> Either (Vary as) a
varyToEither = Vary.pop

mapVary ::
  forall a b (xs :: [Type]) (ys :: [Type]).
  (Mappable a b xs ys) =>
  (a -> b) ->
  Vary xs ->
  Vary ys
mapVary = Vary.mapOn

morphVary ::
  forall (ys :: [Type]) (xs :: [Type]).
  (Subset xs ys) =>
  Vary xs ->
  Vary ys
morphVary = Vary.morph
