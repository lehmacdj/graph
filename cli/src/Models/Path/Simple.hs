module Models.Path.Simple
  ( Path,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    module X,
    spec_showPath,
  )
where

import Models.Path as X
import TestPrelude

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

type TPath = Path String

-- | Tests for Show instance
spec_showPath :: Spec
spec_showPath = describe "Show Path" $ do
  it "One"
    $ show (One :: TPath)
    `shouldBe` [rq|@|]
  it "Zero"
    $ show (Zero :: TPath)
    `shouldBe` [rq|!|]
  it "Wild"
    $ show (Wild :: TPath)
    `shouldBe` [rq|*|]
  it "Literal"
    $ show (Literal "foo" :: TPath)
    `shouldBe` [rq|"foo"|]
  it "RegexMatch"
    $ show (RegexMatch [re|^foo.*bar$|] :: TPath)
    `shouldBe` [rq|re"^foo.*bar$"|]
  it "Absolute"
    $ show (Absolute (smallNID 2) :: TPath)
    `shouldBe` [rq|@000000000002|]
  it "Backwards One"
    $ show (Backwards One :: TPath)
    `shouldBe` [rq|~@|]
  it "Backwards Literal"
    $ show (Backwards (Literal "foo") :: TPath)
    `shouldBe` [rq|~"foo"|]
  it "Simple sequence"
    $ show (Literal "foo" :/ Literal "bar" :: TPath)
    `shouldBe` [rq|"foo" / "bar"|]
  it "Simple union"
    $ show (Literal "foo" :+ Literal "bar" :: TPath)
    `shouldBe` [rq|"foo" + "bar"|]
  it "Simple intersection"
    $ show (Literal "foo" :& Literal "bar" :: TPath)
    `shouldBe` [rq|"foo" & "bar"|]
  it "Precedence: / binds tighter than &"
    $ show (Literal "a" :/ Literal "b" :& Literal "c" :: TPath)
    `shouldBe` [rq|"a" / "b" & "c"|]
  it "Precedence: & binds tighter than +"
    $ show (Literal "a" :& Literal "b" :+ Literal "c" :: TPath)
    `shouldBe` [rq|"a" & "b" + "c"|]
  it "Precedence: complex expression"
    $ show (Literal "a" :/ Literal "b" :& Literal "c" :+ Literal "d" :: TPath)
    `shouldBe` [rq|"a" / "b" & "c" + "d"|]
  it "Backwards with higher precedence"
    $ show (Backwards (Literal "foo") :/ Literal "bar" :: TPath)
    `shouldBe` [rq|~"foo" / "bar"|]
  it "Parentheses needed for lower precedence in Backwards"
    $ show (Backwards (Literal "foo" :+ Literal "bar") :: TPath)
    `shouldBe` [rq|~("foo" + "bar")|]
  it "Parentheses needed for + in &"
    $ show (Literal "a" :& (Literal "b" :+ Literal "c") :: TPath)
    `shouldBe` [rq|"a" & ("b" + "c")|]
  it "No parentheses needed for higher precedence"
    $ show (Literal "a" :+ Literal "b" :/ Literal "c" :: TPath)
    `shouldBe` [rq|"a" + "b" / "c"|]
  it "Complex with Backwards and Absolute"
    $ show (Backwards (Absolute (smallNID 1)) :& Literal "test" :: TPath)
    `shouldBe` [rq|~@000000000001 & "test"|]
