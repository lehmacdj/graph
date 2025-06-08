module Models.Path
  ( Path (..),
    isValidPath,
  )
where

import Models.NID
import MyPrelude

data Path t
  = -- | Stay at a specific location / the current location
    One
  | --  | Dual -- ^ a transition that dualizes the view of the graph
    -- The correct way to implement Dual is simply make backlinks part of the
    -- graph structure, as opposed to intrinsic. i.e. for each normal node have
    -- a slightly special node that stores backlinks for that node.

    --  | Path t :\ Path t -- ^ set minus (useful with wild to restrict)
    --  | Negate (Path t) -- ^ negate a path, if included obsolesces other operators
    --  | Star (Path t) -- ^ kleene iteration: technically top in algebra is top^*

    -- \| a transition matched by anything (top in the algebra)

    -- | Zero
    Wild
  | Literal t
  | -- | this must not be before @:/@ in a @:+@. @:&@ acts as a scope that
    -- allows another absolute node as long as at least one sibling is not
    -- absolute. e.g.:
    -- * @#10 + a/b@ is fine
    -- * as is @a/(b & #10)@
    -- * but @a/#10@ is not ok
    --
    -- 'isValidPath' checks this condition
    --
    -- This is in order to ensure that there aren't any jumps in the
    -- deterministic paths that are created when resolving a path. We could
    -- probably loosen this condition, but better to start stricter and then
    -- loosen up later
    Absolute NID
  | -- | sequence
    Path t :/ Path t
  | -- | union
    Path t :+ Path t
  | -- | intersection
    Path t :& Path t
  deriving (Show, Eq, Ord)

-- make the operator precedence match how they are parsed

infixl 7 :/

infixl 5 :+

infixl 6 :&

isValidPath :: Path t -> Bool
isValidPath = \case
  Absolute _ -> True
  Literal _ -> True
  One -> True
  Wild -> True
  -- Zero -> True
  p1 :& p2 -> isValidPath p1 && isValidPath p2
  p1 :+ p2 -> isValidPath p1 && isValidPath p2
  p1 :/ p2 -> isValidPath p1 && isValidChildPath p2

isValidChildPath :: Path t -> Bool
isValidChildPath = \case
  Absolute _ -> False
  Literal _ -> True
  One -> True
  Wild -> True
  -- Zero -> True
  p1 :+ p2 -> isValidChildPath p1 && isValidChildPath p2
  p1 :/ p2 -> isValidChildPath p1 && isValidChildPath p2
  p1 :& p2 ->
    not (all isAbsolute (andSiblings p1 ++ andSiblings p2))
      && isValidPath p1
      && isValidPath p2
    where
      andSiblings = \case
        p1' :& p2' -> andSiblings p1' ++ andSiblings p2'
        x -> [x]
      isAbsolute = \case
        Absolute _ -> True
        Literal _ -> False
        One -> False
        Wild -> False
        -- Zero -> False
        _ :+ _ -> False
        _ :& _ -> False
        _ :/ _ -> False
