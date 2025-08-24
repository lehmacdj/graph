module Models.Path
  ( Path (..),
  )
where

import Models.NID
import MyPrelude

data Path t
  = -- | Stay at a specific location / the current location.
    -- Acts as a join point forcing intersections of a path that end in it to
    -- match at the same node (e.g. `(a/@ & b)/c` is equivalent to `(a & b)
    -- /@/c`)
    One
  | --  | Dual -- ^ a transition that dualizes the view of the graph
    -- The correct way to implement Dual is simply make backlinks part of the
    -- graph structure, as opposed to intrinsic. i.e. for each normal node have
    -- a slightly special node that stores backlinks for that node.

    --  | Path t :\ Path t -- ^ set minus (useful with wild to restrict)
    --  | Negate (Path t) -- ^ negate a path, if included obsolesces other operators
    --  | Star (Path t) -- ^ kleene iteration: technically top in algebra is top^*

    -- \| a transition matched by anything (top in the algebra)

    -- | Zero path (bottom element in path algebra)
    Zero
  | Wild
  | Literal t
  | -- | marks a specific NID. Acts like an anchor if after a :/, and as a root
    -- if at the start of a path. Creates Pointlike paths when :&-ed with
    -- other paths
    Absolute NID
  | -- | backlink-ify. traversing Backwards (Literal t) traverses the backlink
    -- (Literal t) instead, likewise for Wild. It inverts the order of paths and
    -- has no effect on node-location specifying path components or
    -- intersection/union operators
    Backwards (Path t)
  | -- | sequence
    Path t :/ Path t
  | -- | union
    Path t :+ Path t
  | -- | intersection
    Path t :& Path t
  deriving (Show, Eq, Ord, Lift)

-- make the operator precedence match how they are parsed

infixl 7 :/

infixl 5 :+

infixl 6 :&
