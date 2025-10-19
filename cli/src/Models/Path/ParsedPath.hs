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

pattern Backwards :: ParsedPath t -> ParsedPath t
pattern Backwards p = Backwards' p

pattern (:/) :: ParsedPath t -> ParsedPath t -> ParsedPath t
pattern l :/ r = l ::/ r

infixl 7 :/

pattern (:+) :: ParsedPath t -> ParsedPath t -> ParsedPath t
pattern l :+ r = l ::+ r

infixl 5 :+

pattern (:&) :: ParsedPath t -> ParsedPath t -> ParsedPath t
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

type TPath = ParsedPath String

-- | Tests for Show instance
spec_showPath :: Spec
spec_showPath = describe "Show Path" $ do
  it "One" $
    show (One :: TPath)
      `shouldBe` [rq|@|]
  it "Zero" $
    show (Zero :: TPath)
      `shouldBe` [rq|!|]
  it "Wild" $
    show (Wild :: TPath)
      `shouldBe` [rq|*|]
  it "Literal" $
    show (Literal "foo" :: TPath)
      `shouldBe` [rq|"foo"|]
  it "RegexMatch" $
    show (RegexMatch [re|^foo.*bar$|] :: TPath)
      `shouldBe` [rq|re"^foo.*bar$"|]
  it "Absolute" $
    show (Absolute (smallNID 2) :: TPath)
      `shouldBe` [rq|@000000000002|]
  it "Backwards One" $
    show (Backwards One :: TPath)
      `shouldBe` [rq|~@|]
  it "Backwards Literal" $
    show (Backwards (Literal "foo") :: TPath)
      `shouldBe` [rq|~"foo"|]
  it "Simple sequence" $
    show (Literal "foo" :/ Literal "bar" :: TPath)
      `shouldBe` [rq|"foo" / "bar"|]
  it "Simple union" $
    show (Literal "foo" :+ Literal "bar" :: TPath)
      `shouldBe` [rq|"foo" + "bar"|]
  it "Simple intersection" $
    show (Literal "foo" :& Literal "bar" :: TPath)
      `shouldBe` [rq|"foo" & "bar"|]
  it "Precedence: / binds tighter than &" $
    show (Literal "a" :/ Literal "b" :& Literal "c" :: TPath)
      `shouldBe` [rq|"a" / "b" & "c"|]
  it "Precedence: & binds tighter than +" $
    show (Literal "a" :& Literal "b" :+ Literal "c" :: TPath)
      `shouldBe` [rq|"a" & "b" + "c"|]
  it "Precedence: complex expression" $
    show (Literal "a" :/ Literal "b" :& Literal "c" :+ Literal "d" :: TPath)
      `shouldBe` [rq|"a" / "b" & "c" + "d"|]
  it "Backwards with higher precedence" $
    show (Backwards (Literal "foo") :/ Literal "bar" :: TPath)
      `shouldBe` [rq|~"foo" / "bar"|]
  it "Parentheses needed for lower precedence in Backwards" $
    show (Backwards (Literal "foo" :+ Literal "bar") :: TPath)
      `shouldBe` [rq|~("foo" + "bar")|]
  it "Parentheses needed for + in &" $
    show (Literal "a" :& (Literal "b" :+ Literal "c") :: TPath)
      `shouldBe` [rq|"a" & ("b" + "c")|]
  it "No parentheses needed for higher precedence" $
    show (Literal "a" :+ Literal "b" :/ Literal "c" :: TPath)
      `shouldBe` [rq|"a" + "b" / "c"|]
  it "Complex with Backwards and Absolute" $
    show (Backwards (Absolute (smallNID 1)) :& Literal "test" :: TPath)
      `shouldBe` [rq|~@000000000001 & "test"|]
  it "LocationFromHistory" $
    show (Directive testAnn (LocationFromHistory -23) :: TPath)
      `shouldBe` [rq|%history(-23)|]
  it "complex" $
    show
      ( Backwards (RegexMatch [re|[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}|])
          :/ Directive testAnn (Splice "excludedLargeSystemNodes") ::
          TPath
      )
      `shouldBe` [rq|~re"[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{12}" / %{excludedLargeSystemNodes}|]
