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

-- | PartialPath type (representing paths that may contain parse errors)
type PartialPath = Path' 'Partial

pattern Backwards :: PartialPath -> PartialPath
pattern Backwards p = Backwards' p

pattern (:/) ::
  PartialPath ->
  PartialPath ->
  PartialPath
pattern l :/ r = l ::/ r

infixl 7 :/

pattern (:+) ::
  PartialPath ->
  PartialPath ->
  PartialPath
pattern l :+ r = l ::+ r

infixl 5 :+

pattern (:&) ::
  PartialPath ->
  PartialPath ->
  PartialPath
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
  (:&),
  Directive,
  Hole
  #-}
