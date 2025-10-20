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

-- | Simple Path type (representing elaborated paths)
type Path = Path' 'Prenormal

pattern Backwards :: Path t -> Path t
pattern Backwards p = Backwards' p

pattern (:/) :: Path t -> Path t -> Path t
pattern l :/ r = l ::/ r

infixl 7 :/

pattern (:+) :: Path t -> Path t -> Path t
pattern l :+ r = l ::+ r

infixl 5 :+

pattern (:&) :: Path t -> Path t -> Path t
pattern l :& r = l ::& r

infixl 6 :&

{-# COMPLETE
  One,
  Zero,
  Wild,
  Literal,
  RegexMatch,
  Absolute,
  ExcludingNIDs,
  Backwards,
  (:/),
  (:+),
  (:&)
  #-}
