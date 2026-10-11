-- | For general work with regexes import either Control.Lens.Regex.Text or
-- Control.Lens.Regex.ByteString, then use the `[regex|...|]` quasiquoter
-- which produces a Traversal directly.
-- This module provides a CheckedRegex type that has a Lift instance and can
-- be compiled into a Traversal via byteStringRegexing or textRegexing, or
-- compiled into a Text.Regex.PCRE.Light.Regex via getRegex.
module MyPrelude.Regex
  ( CheckedRegex,
    compileRegex,
    getRegex,
    textRegexing,
    byteStringRegexing,
    re,
    spec_compileRegex,
  )
where

import ClassyPrelude hiding (lift)
import Control.Lens (IndexedTraversal', has)
import Control.Lens.Regex.ByteString qualified as ByteString
import Control.Lens.Regex.Text qualified as Text
import Data.Function ((&))
import Foreign.C.Types (CInt)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Text.Regex.PCRE.Light as X (PCREOption, Regex)
import Text.Regex.PCRE.Light qualified as PCRE
import Unsafe.Coerce (unsafeCoerce)
import Utils.Testing.External
import Prelude (fail)

-- | A regex pattern with options that has been validated at construction time
data CheckedRegex = UnsafeCheckedRegex
  { pat :: ByteString,
    options :: [PCREOption],
    compiled :: Regex
  }
  deriving (Generic)

instance Eq CheckedRegex where
  UnsafeCheckedRegex p1 o1 _ == UnsafeCheckedRegex p2 o2 _ =
    -- it's ok to ignore the compiled field because it's semantics is uniquely
    -- determined by the pattern and options
    p1 == p2 && o1 == o2

instance Ord CheckedRegex where
  compare (UnsafeCheckedRegex p1 o1 _) (UnsafeCheckedRegex p2 o2 _) =
    -- it's ok to ignore the compiled field because it's semantics is uniquely
    -- determined by the pattern and options
    compare (p1, o1) (p2, o2)

instance NFData CheckedRegex where
  rnf UnsafeCheckedRegex {..} =
    rnf pat `seq`
      rnf (unsafeCoerce options :: [CInt])

instance Show CheckedRegex where
  show (UnsafeCheckedRegex p _ _) =
    -- we need to avoid escaping the backslashes pattern since we don't require
    -- them when parsing
    "regex:\"" ++ asString (unpack (decodeUtf8 p)) ++ "\""

-- | Smart constructor that validates the regex pattern.
--
-- Every 'CheckedRegex' is made here (the ones of a path that a user writes,
-- and the ones of 're'), with one set of options, so two of them are equal
-- exactly when their patterns are.
--
-- The option: @$@ matches at the very end of the text and nowhere else. PCRE
-- by default lets it match before a newline that ends the text as well, so
-- @^(a|b)$@ would match the label @"a\n"@, which is neither @a@ nor @b@.
-- (@^@ is the start of the text only, without an option. A pattern that
-- turns on multiline matching itself, with @(?m)@, gets PCRE's lines for
-- both.)
compileRegex :: ByteString -> Either String CheckedRegex
compileRegex = (`compileRegex'` [PCRE.dollar_endonly])

spec_compileRegex :: Spec
spec_compileRegex = do
  let compiled :: ByteString -> CheckedRegex
      compiled = either (error . unpack) id . compileRegex
      matches :: ByteString -> Text -> Bool
      matches pat = has (textRegexing (compiled pat))
  it "takes $ for the very end of the text, also when a newline ends it" do
    ("a$" `matches` "a") `shouldBe` True
    ("a$" `matches` "a\n") `shouldBe` False
    ("^(a|b)$" `matches` "b\n") `shouldBe` False
    ("a\n$" `matches` "a\n") `shouldBe` True
  it "takes ^ for the start of the text only" do
    ("^a" `matches` "a\nb") `shouldBe` True
    ("^b" `matches` "a\nb") `shouldBe` False
  it "takes a newline in the middle of the text for no end" do
    ("a$" `matches` "a\nb") `shouldBe` False
    ("b$" `matches` "a\nb") `shouldBe` True
  it "compiles one pattern to one regex, shown as it is written" do
    compiled "^a$" `shouldBe` compiled "^a$"
    show (compiled "^a$") `shouldBe` "regex:\"^a$\""
    compare (compiled "^a$") (compiled "^a$") `shouldBe` EQ
    rnf (compiled "^a$") `shouldBe` ()

compileRegex' :: ByteString -> [PCREOption] -> Either String CheckedRegex
compileRegex' pat options =
  PCRE.compileM pat options
    & bimap pack (UnsafeCheckedRegex pat options)

re :: QuasiQuoter
re =
  QuasiQuoter
    { quoteExp = \s ->
        case compileRegex (encodeUtf8 (pack s)) of
          Left err -> fail ("Invalid regex: " ++ unpack err)
          Right cr -> lift cr,
      quotePat = error "quotePat not implemented for re",
      quoteType = error "quoteType not implemented for re",
      quoteDec = error "quoteDec not implemented for re"
    }

-- | Compile a CheckedRegex to a Regex at runtime
getRegex :: CheckedRegex -> PCRE.Regex
getRegex UnsafeCheckedRegex {..} = compiled

textRegexing :: CheckedRegex -> IndexedTraversal' Int Text Text.Match
textRegexing (UnsafeCheckedRegex _ _ r) = Text.regexing r

byteStringRegexing ::
  CheckedRegex -> IndexedTraversal' Int ByteString ByteString.Match
byteStringRegexing (UnsafeCheckedRegex _ _ r) = ByteString.regexing r

instance Lift CheckedRegex where
  lift UnsafeCheckedRegex {..} =
    [|UnsafeCheckedRegex pat options (PCRE.compile pat options)|]
  liftTyped (UnsafeCheckedRegex pat options _) =
    [||UnsafeCheckedRegex pat options (PCRE.compile pat options)||]
