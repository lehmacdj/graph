module Models.Path.ParsedPath
  ( ParsedPath,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    module X,
  )
where

import Models.Path as X
import MyPrelude

-- | Represents a path that has been successfully parsed, but has had no
-- further processing applied
type ParsedPath = Path' Unelaborated Identity

pattern Backwards :: ParsedPath t -> ParsedPath t
pattern Backwards p = Backwards' (Identity p)

pattern (:/) :: ParsedPath t -> ParsedPath t -> ParsedPath t
pattern l :/ r = Identity l ::/ Identity r

infixl 7 :/

pattern (:+) :: ParsedPath t -> ParsedPath t -> ParsedPath t
pattern l :+ r = Identity l ::+ Identity r

infixl 5 :+

pattern (:&) :: ParsedPath t -> ParsedPath t -> ParsedPath t
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
  (:&),
  LocationFromHistory
  #-}
