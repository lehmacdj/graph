module Models.Path.Simple
  ( Path,
    pattern Backwards,
    pattern (:/),
    pattern (:||),
    pattern (:++),
    pattern (:&),
    module X,
  )
where

import Models.Path as X

-- | Simple Path type (representing elaborated paths)
type Path = Path' 'Prenormal

pattern Backwards :: Path -> Path
pattern Backwards p = Backwards' p

pattern (:/) :: Path -> Path -> Path
pattern l :/ r = l ::/ r

infixl 7 :/

-- | @l | r@, a relational union
pattern (:||) :: Path -> Path -> Path
pattern l :|| r = Union Relational l r

infixl 5 :||

-- | @l ++ r@, a shared union
pattern (:++) :: Path -> Path -> Path
pattern l :++ r = Union Shared l r

infixl 5 :++

pattern (:&) :: Path -> Path -> Path
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
  Union,
  WithLimits,
  (:&)
  #-}

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
  (:||),
  (:++),
  WithLimits,
  (:&)
  #-}
