module Models.Path.ParsedPath
  ( ParsedPath,
    pattern Backwards,
    pattern (:/),
    pattern (:+),
    pattern (:&),
    module X,
    spec_showPath,
  )
where

import Models.Path as X
import MyPrelude
import Utils.Parsing.Common
import Utils.Testing

-- | Represents a path that has been successfully parsed, but has had no
-- further processing applied
type ParsedPath = Path' 'WithDirectives

pattern Backwards :: ParsedPath -> ParsedPath
pattern Backwards p = Backwards' p

pattern (:/) :: ParsedPath -> ParsedPath -> ParsedPath
pattern l :/ r = l ::/ r

infixl 7 :/

pattern (:+) :: ParsedPath -> ParsedPath -> ParsedPath
pattern l :+ r = l ::+ r

infixl 5 :+

pattern (:&) :: ParsedPath -> ParsedPath -> ParsedPath
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
  Directive
  #-}

-- | Tests for Show instance
spec_showPath :: Spec
spec_showPath = describe "Show Path" $ do
  it "One" $
    show (One :: ParsedPath)
      `shouldBe` [rq|@|]
  it "Zero" $
    show (Zero :: ParsedPath)
      `shouldBe` [rq|%never|]
  it "Wild" $
    show (Wild :: ParsedPath)
      `shouldBe` [rq|*|]
  it "Literal" $
    show (Literal "foo" :: ParsedPath)
      `shouldBe` [rq|"foo"|]
  it "RegexMatch" $
    show (RegexMatch [re|^foo.*bar$|] :: ParsedPath)
      `shouldBe` [rq|regex:"^foo.*bar$"|]
  it "Absolute" $
    show (Absolute (smallNID 2) :: ParsedPath)
      `shouldBe` [rq|@000000000002|]
  it "Backwards One" $
    show (Backwards One :: ParsedPath)
      `shouldBe` [rq|~@|]
  it "Backwards Literal" $
    show (Backwards (Literal "foo") :: ParsedPath)
      `shouldBe` [rq|~"foo"|]
  it "Simple sequence" $
    show (Literal "foo" :/ Literal "bar" :: ParsedPath)
      `shouldBe` [rq|"foo" / "bar"|]
  it "Simple union" $
    show (Literal "foo" :+ Literal "bar" :: ParsedPath)
      `shouldBe` [rq|"foo" + "bar"|]
  it "Simple intersection" $
    show (Literal "foo" :& Literal "bar" :: ParsedPath)
      `shouldBe` [rq|"foo" & "bar"|]
  it "Precedence: / binds tighter than &" $
    show (Literal "a" :/ Literal "b" :& Literal "c" :: ParsedPath)
      `shouldBe` [rq|"a" / "b" & "c"|]
  it "Precedence: & binds tighter than +" $
    show (Literal "a" :& Literal "b" :+ Literal "c" :: ParsedPath)
      `shouldBe` [rq|"a" & "b" + "c"|]
  it "Precedence: complex expression" $
    show (Literal "a" :/ Literal "b" :& Literal "c" :+ Literal "d" :: ParsedPath)
      `shouldBe` [rq|"a" / "b" & "c" + "d"|]
  it "Backwards with higher precedence" $
    show (Backwards (Literal "foo") :/ Literal "bar" :: ParsedPath)
      `shouldBe` [rq|~"foo" / "bar"|]
  it "Parentheses needed for lower precedence in Backwards" $
    show (Backwards (Literal "foo" :+ Literal "bar") :: ParsedPath)
      `shouldBe` [rq|~("foo" + "bar")|]
  it "Parentheses needed for + in &" $
    show (Literal "a" :& (Literal "b" :+ Literal "c") :: ParsedPath)
      `shouldBe` [rq|"a" & ("b" + "c")|]
  it "No parentheses needed for higher precedence" $
    show (Literal "a" :+ Literal "b" :/ Literal "c" :: ParsedPath)
      `shouldBe` [rq|"a" + "b" / "c"|]
  it "Complex with Backwards and Absolute" $
    show (Backwards (Absolute (smallNID 1)) :& Literal "test" :: ParsedPath)
      `shouldBe` [rq|~@000000000001 & "test"|]
  it "LocationFromHistory" $
    show (Directive testRange (LocationFromHistory -23) :: ParsedPath)
      `shouldBe` [rq|%history(-23)|]
  it "complex" $
    show
      ( Backwards (RegexMatch [re|[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}|])
          :/ Directive testRange (Splice "excludedLargeSystemNodes") ::
          ParsedPath
      )
      `shouldBe` [rq|~regex:"[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}" / %{excludedLargeSystemNodes}|]
