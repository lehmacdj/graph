module Lang.Path.TH where

import Control.Monad.Fail (MonadFail (fail))
import Lang.Parsing (transition)
import Lang.Path.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import MyPrelude
import Text.Megaparsec (eof, parse)

path :: QuasiQuoter
path =
  QuasiQuoter
    { quoteExp = pathExp,
      quotePat = const $ fail "path quasi-quoter can only be used in expression contexts",
      quoteType = const $ fail "path quasi-quoter can only be used in expression contexts",
      quoteDec = const $ fail "path quasi-quoter can only be used in expression contexts"
    }

pathExp :: String -> Q Exp
pathExp s = case parse (pPath transition <* eof) "" s of
  Left err -> fail $ "Parse error in path quasi-quoter: " ++ show err
  Right result -> [|result|]
