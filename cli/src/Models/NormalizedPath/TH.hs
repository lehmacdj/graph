module Models.NormalizedPath.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Models.NormalizedPath.Parse
import MyPrelude
import Utils.Parsing (transition)
import Utils.Parsing.Common

npath :: QuasiQuoter
npath =
  QuasiQuoter
    { quoteExp = npathExp,
      quotePat = const $ fail "npath quasi-quoter can only be used in expression contexts",
      quoteType = const $ fail "npath quasi-quoter can only be used in expression contexts",
      quoteDec = const $ fail "npath quasi-quoter can only be used in expression contexts"
    }

npathExp :: String -> Q Exp
npathExp s = case runParser (pNormalizedPath transition <* eof) "" (pack s) of
  Left err -> fail $ "Parse error in npath quasi-quoter: " ++ show err
  Right result -> [|result|]
