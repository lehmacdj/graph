-- | What the WebAssembly build gives JavaScript, as JSON text: parsing a
-- path with its syntax problems, and completion without a graph.
--
-- The reactor's @Main@ (@wasm/src/Main.hs@, built only for WebAssembly)
-- exports these through GHC's JavaScript FFI; they are here, in the
-- native library, so the native tests cover them.
--
-- The core counts offsets in characters (code points); JavaScript strings
-- count UTF-16 code units, which differ for a character outside the Basic
-- Multilingual Plane (an emoji, say). Every offset and length this module
-- gives is in UTF-16 code units, for @String.prototype.slice@.
module Wasm.Api
  ( parsePathJson,
    completeJson,
    pathProblems,
    completions,
    completionsWith,
    utf16Length,
    utf16Offset,
    utf16SuffixLength,
    spec_utf16,
    spec_pathProblems,
    spec_completions,
    spec_json,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf16LE, encodeUtf16LE)
import Models.Completion
import Models.Path.Parse (parsePath)
import MyPrelude
import Utils.Parsing.Problems
import Utils.Testing

-- | How many UTF-16 code units the text is.
utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + bool 1 2 (ord c > 0xFFFF)) 0

-- | An offset into the text in characters, as UTF-16 code units.
utf16Offset :: Text -> Int -> Int
utf16Offset t n = utf16Length (take n t)

-- | How many UTF-16 code units the last @n@ characters of the text are.
utf16SuffixLength :: Text -> Int -> Int
utf16SuffixLength t n = utf16Length (T.takeEnd n t)

-- | The syntax problems of a path, none if it parses, with offsets in
-- UTF-16 code units.
pathProblems :: Text -> [SyntaxProblem]
pathProblems t = case parsePath t of
  Right _ -> []
  Left bundle -> map inUtf16 (toList (syntaxProblems bundle))
  where
    inUtf16 p =
      p {start = utf16Offset t p.start, end = utf16Offset t p.end}

-- | The completions without a graph of the text before the cursor
-- ('completeWithoutGraph'), with 'replaced' in UTF-16 code units.
completions :: Text -> Completions
completions = runIdentity . completionsWith withoutGraph

-- | The completions of the text before the cursor with these lookups
-- ('complete'), with 'replaced' in UTF-16 code units.
completionsWith ::
  (Monad m) => CompletionLookups m -> Text -> m Completions
completionsWith lookups before =
  complete lookups before <&> \cs ->
    cs {replaced = utf16SuffixLength before cs.replaced}

-- | @{"ok": true}@ for a path that parses; otherwise @{"ok": false,
-- "problems": [{"start", "end", "message"}]}@ ('pathProblems').
parsePathValue :: Text -> Value
parsePathValue t = case pathProblems t of
  [] -> object ["ok" .= True]
  ps -> object ["ok" .= False, "problems" .= map problem ps]
  where
    problem p =
      object ["start" .= p.start, "end" .= p.end, "message" .= p.message]

-- | @{"replaced", "candidates": [{"text", "finished"}]}@ ('completions').
completeValue :: Text -> Value
completeValue before =
  object
    [ "replaced" .= cs.replaced,
      "candidates" .= map candidate cs.candidates
    ]
  where
    cs = completions before
    candidate c = object ["text" .= c.text, "finished" .= c.finished]

parsePathJson :: Text -> Text
parsePathJson = encodeText . parsePathValue

completeJson :: Text -> Text
completeJson = encodeText . completeValue

encodeText :: Value -> Text
encodeText = decodeUtf8 . toStrict . encodeJSON

-- | A character outside the Basic Multilingual Plane, two UTF-16 code
-- units. Not a letter: a label with it is quoted.
emoji :: Text
emoji = "\x1F600"

-- | A label of an emoji, quoted: four UTF-16 code units, three characters.
quoted :: Text
quoted = "\"" <> emoji <> "\""

-- | A letter outside the Basic Multilingual Plane (MATHEMATICAL SCRIPT
-- CAPITAL A), which a label needs no quotes for.
letter :: Text
letter = "\x1D49C"

spec_utf16 :: Spec
spec_utf16 = do
  it "counts one unit for a character in the Basic Multilingual Plane" $
    utf16Length "a\233\8364" `shouldBe` 3
  it "counts two units for a character outside it" $
    utf16Length ("a" <> emoji <> "b") `shouldBe` 4
  it "converts an offset after an emoji" do
    let t = emoji <> "/" <> emoji <> "x"
    map (utf16Offset t) [0 .. 4] `shouldBe` [0, 2, 3, 5, 6]
  it "converts the length of a suffix with an emoji" do
    let t = "a/" <> emoji <> "b"
    map (utf16SuffixLength t) [0 .. 4] `shouldBe` [0, 1, 3, 4, 5]

spec_pathProblems :: Spec
spec_pathProblems = do
  it "has none for a path that parses" do
    pathProblems ("a/" <> quoted) `shouldBe` []
    pathProblems ("a/" <> letter) `shouldBe` []
  it "places a problem after an emoji in UTF-16 code units" do
    stretches (quoted <> "/)") `shouldBe` [(5, 6)]
    stretches (quoted <> "/@abc") `shouldBe` [(5, 9)]
    stretches (letter <> "/" <> quoted <> "/") `shouldBe` [(8, 8)]
  it "gives the stretch of an unquoted emoji, both of its units" $
    stretches ("a/" <> emoji) `shouldBe` [(2, 4)]
  where
    stretches t = [(p.start, p.end) | p <- pathProblems t]

spec_completions :: Spec
spec_completions = do
  it "replaces a label with a letter outside the BMP, in UTF-16 units" $
    (completions ("ls a/b" <> letter)).replaced `shouldBe` 3
  it "replaces the word after an emoji, not counting it" $
    completions ("ls " <> quoted <> " & %la")
      `shouldBe` Completions 3 [Candidate "%last" False]
  it "replaces a quote with an emoji in it, in UTF-16 units" $
    completions ("ls a/\"" <> emoji <> "b") `shouldBe` Completions 4 []
  describe "quoted candidates with characters outside the BMP" do
    -- the origin has a label that starts with an emoji and one that
    -- starts with a letter outside the BMP and has a space in it
    let lookups =
          withoutGraph
            { labelsAfter = \_ ->
                Identity [emoji <> " smile", letter <> " x", letter]
            }
        completing = runIdentity . completionsWith lookups
        -- the text before the cursor after taking each candidate, cut
        -- as JavaScript cuts it: by UTF-16 code units, two bytes each
        taken before =
          let cs = completing before
              units = encodeUtf16LE before
              kept = BS.take (BS.length units - 2 * cs.replaced) units
           in [decodeUtf16LE kept <> c.text | c <- cs.candidates]
    it "replaces a quote, counting its emoji twice" $
      completing ("ls \"" <> emoji)
        `shouldBe` Completions
          3
          [Candidate ("\"" <> emoji <> " smile\"") False]
    it "replaces a plain word, counting its letter twice" $
      completing ("ls " <> letter)
        `shouldBe` Completions
          2
          [ Candidate ("\"" <> letter <> " x\"") False,
            Candidate ("\"" <> letter <> "\"") False
          ]
    it "gives text that parses once a candidate is taken" do
      taken ("ls \"" <> emoji)
        `shouldBe` ["ls \"" <> emoji <> " smile\""]
      for_ (taken ("ls " <> letter) <> taken ("ls \"" <> letter)) \t ->
        (t, pathProblems (drop 3 t)) `shouldBe` (t, [])

spec_json :: Spec
spec_json = do
  it "says a path parses" $
    parsePathJson "a/b" `shouldBe` [rq|{"ok":true}|]
  it "gives the problems of a path that does not parse" $
    parsePathJson (quoted <> "/)")
      `shouldSatisfy` ([rq|{"ok":false,"problems":[{"end":6,|] `isPrefixOf`)
  it "gives completions" $
    completeJson "l"
      `shouldBe` [rq|{"candidates":[{"finished":true,"text":"ls"}],"replaced":1}|]
