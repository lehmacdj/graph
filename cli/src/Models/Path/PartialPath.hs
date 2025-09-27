module Models.Path.PartialPath
  ( PartialPath,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    module X,
  )
where

import Lang.Parsing (ParseError')
import Models.Path as X
import MyPrelude

-- | PartialPath type (representing paths that may contain parse errors)
type PartialPath = Path' Unelaborated (Either ParseError')

pattern Backwards :: Either ParseError' (PartialPath t) -> PartialPath t
pattern Backwards p = Backwards' p

pattern (:/) ::
  Either ParseError' (PartialPath t) ->
  Either ParseError' (PartialPath t) ->
  PartialPath t
pattern l :/ r = l ::/ r

infixl 7 :/

pattern (:+) ::
  Either ParseError' (PartialPath t) ->
  Either ParseError' (PartialPath t) ->
  PartialPath t
pattern l :+ r = l ::+ r

infixl 5 :+

pattern (:&) ::
  Either ParseError' (PartialPath t) ->
  Either ParseError' (PartialPath t) ->
  PartialPath t
pattern l :& r = l ::& r

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
