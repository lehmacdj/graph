module MyPrelude.Regex
  ( module X,
    CheckedRegex,
    compileRegex,
    compileRegex',
    getRegex,
    regexing',
    re,
  )
where

import ClassyPrelude hiding (lift)
import Control.Lens (IndexedTraversal')
import Control.Lens.Regex.Text as X hiding (group)
import Data.Function ((&))
import Foreign.C.Types (CInt)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Text.Regex.PCRE.Light as X (PCREOption)
import Text.Regex.PCRE.Light qualified as PCRE
import Unsafe.Coerce (unsafeCoerce)
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
    "re\"" ++ asString (unpack (decodeUtf8 p)) ++ "\""

-- | Smart constructor that validates the regex pattern
compileRegex :: ByteString -> Either String CheckedRegex
compileRegex = (`compileRegex'` [])

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
getRegex :: CheckedRegex -> Regex
getRegex UnsafeCheckedRegex {..} = compiled

regexing' :: CheckedRegex -> IndexedTraversal' Int Text Match
regexing' (UnsafeCheckedRegex _ _ r) = regexing r

instance Lift CheckedRegex where
  lift UnsafeCheckedRegex {..} =
    [|UnsafeCheckedRegex pat options (PCRE.compile pat options)|]
  liftTyped (UnsafeCheckedRegex pat options _) =
    [||UnsafeCheckedRegex pat options (PCRE.compile pat options)||]
