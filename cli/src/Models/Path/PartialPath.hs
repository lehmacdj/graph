module Models.Path.PartialPath
  ( PartialPath,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    module X,
  )
where

import Models.Path as X
import MyPrelude
import Utils.Parsing (ParseError')

-- | PartialPath type (representing paths that may contain parse errors)
type PartialPath = Path' WithDirectives (Either ParseError')

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
