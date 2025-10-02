module Models.Path.Simple
  ( Path,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    module X,
  )
where

import Models.Path as X
import MyPrelude

-- | Simple Path type (representing elaborated paths)
type Path = Path' Prenormal Identity

pattern Backwards :: Path t -> Path t
pattern Backwards p = Backwards' (Identity p)

pattern (:/) :: Path t -> Path t -> Path t
pattern l :/ r = Identity l ::/ Identity r

infixl 7 :/

pattern (:+) :: Path t -> Path t -> Path t
pattern l :+ r = Identity l ::+ Identity r

infixl 5 :+

pattern (:&) :: Path t -> Path t -> Path t
pattern l :& r = Identity l ::& Identity r

infixl 6 :&

{-# COMPLETE
  One,
  Zero,
  Wild,
  Literal,
  RegexMatch,
  Absolute,
  Backwards,
  (:/),
  (:+),
  (:&)
  #-}
