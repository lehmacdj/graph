{-# LANGUAGE UndecidableInstances #-}

module Models.Path
  ( Path' (..),
    Path,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    spec_showPath,
  )
where

import Data.Functor.Classes (Show1 (..))
import Models.NID
import MyPrelude
import TestPrelude

data Path' f t
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
    Backwards' (f (Path' f t))
  | -- | sequence
    f (Path' f t) ::/ f (Path' f t)
  | -- | union
    f (Path' f t) ::+ f (Path' f t)
  | -- | intersection
    f (Path' f t) ::& f (Path' f t)
  deriving (Generic)

deriving instance (Lift t, Lift (f (Path' f t))) => Lift (Path' f t)

showsPath ::
  (Show t) =>
  (Int -> f (Path' f t) -> ShowS) ->
  Int ->
  Path' f t ->
  ShowS
showsPath _ _ One = showString "@"
showsPath _ _ Zero = showString "!"
showsPath _ _ Wild = showString "*"
showsPath _ _ (Literal x) = shows x
showsPath _ _ (Absolute nid) = showString "@" . shows nid
showsPath showF d (Backwards' p) =
  showParen (d > 8) $ showString "~" . showF 8 p
showsPath showF d (l ::/ r) =
  showParen (d > 7) $ showF 8 l . showString " / " . showF 8 r
showsPath showF d (l ::& r) =
  showParen (d > 6) $ showF 7 l . showString " & " . showF 7 r
showsPath showF d (l ::+ r) =
  showParen (d > 5) $ showF 6 l . showString " + " . showF 6 r

instance (Show1 f, Show t) => Show (Path' f t) where
  showsPrec = showsPath showsPath1
    where
      showsPath1 :: (Show1 f, Show t) => Int -> f (Path' f t) -> ShowS
      showsPath1 = liftShowsPrec (showsPath showsPath1) showList

instance {-# OVERLAPPING #-} (Show t) => Show (Path' Identity t) where
  showsPrec = showsPath skipIdentity
    where
      skipIdentity :: (Show t) => Int -> Identity (Path' Identity t) -> ShowS
      skipIdentity prec (Identity p) = showsPath skipIdentity prec p

deriving instance (Eq t) => Eq (Path t)

deriving instance (Ord t) => Ord (Path t)

type Path = Path' Identity

pattern Backwards :: Path t -> Path t
pattern Backwards p = Backwards' (Identity p)

pattern (:/) :: Path t -> Path t -> Path t
pattern l :/ r = Identity l ::/ Identity r

pattern (:+) :: Path t -> Path t -> Path t
pattern l :+ r = Identity l ::+ Identity r

pattern (:&) :: Path t -> Path t -> Path t
pattern l :& r = Identity l ::& Identity r

{-# COMPLETE One, Zero, Wild, Literal, Absolute, Backwards, (:/), (:+), (:&) #-}

-- make the operator precedence match how they are parsed

infixl 7 :/

infixl 7 ::/

infixl 5 :+

infixl 5 ::+

infixl 6 :&

infixl 6 ::&

-- Tests for Show instance
spec_showPath :: Spec
spec_showPath = describe "Show Path" $ do
  it "One" $
    show (One :: Path String) `shouldBe` [rq|@|]
  it "Zero" $
    show (Zero :: Path String) `shouldBe` [rq|!|]
  it "Wild" $
    show (Wild :: Path String) `shouldBe` [rq|*|]
  it "Literal" $
    show (Literal "foo" :: Path String) `shouldBe` [rq|"foo"|]
  it "Absolute" $
    show (Absolute (smallNID 2) :: Path String) `shouldBe` [rq|@000000000002|]
  it "Backwards One" $
    show (Backwards One :: Path String) `shouldBe` [rq|~@|]
  it "Backwards Literal" $
    show (Backwards (Literal "foo") :: Path String) `shouldBe` [rq|~"foo"|]
  it "Simple sequence" $
    show (Literal "foo" :/ Literal "bar" :: Path String) `shouldBe` [rq|"foo" / "bar"|]
  it "Simple union" $
    show (Literal "foo" :+ Literal "bar" :: Path String) `shouldBe` [rq|"foo" + "bar"|]
  it "Simple intersection" $
    show (Literal "foo" :& Literal "bar" :: Path String) `shouldBe` [rq|"foo" & "bar"|]
  it "Precedence: / binds tighter than &" $
    show (Literal "a" :/ Literal "b" :& Literal "c" :: Path String) `shouldBe` [rq|"a" / "b" & "c"|]
  it "Precedence: & binds tighter than +" $
    show (Literal "a" :& Literal "b" :+ Literal "c" :: Path String) `shouldBe` [rq|"a" & "b" + "c"|]
  it "Precedence: complex expression" $
    show (Literal "a" :/ Literal "b" :& Literal "c" :+ Literal "d" :: Path String) `shouldBe` [rq|"a" / "b" & "c" + "d"|]
  it "Backwards with higher precedence" $
    show (Backwards (Literal "foo") :/ Literal "bar" :: Path String) `shouldBe` [rq|~"foo" / "bar"|]
  it "Parentheses needed for lower precedence in Backwards" $
    show (Backwards (Literal "foo" :+ Literal "bar") :: Path String) `shouldBe` [rq|~("foo" + "bar")|]
  it "Parentheses needed for + in &" $
    show (Literal "a" :& (Literal "b" :+ Literal "c") :: Path String) `shouldBe` [rq|"a" & ("b" + "c")|]
  it "No parentheses needed for higher precedence" $
    show (Literal "a" :+ Literal "b" :/ Literal "c" :: Path String) `shouldBe` [rq|"a" + "b" / "c"|]
  it "Complex with Backwards and Absolute" $
    show (Backwards (Absolute (smallNID 1)) :& Literal "test" :: Path String) `shouldBe` [rq|~@000000000001 & "test"|]
