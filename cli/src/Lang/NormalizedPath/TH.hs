{-# LANGUAGE TemplateHaskell #-}

module Lang.NormalizedPath.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Lang.NormalizedPath.Parse
import Lang.Parsing
import Models.NormalizedPath
import Text.Megaparsec (parse, eof)
import MyPrelude
import Control.Monad.Fail (MonadFail(fail))

npath :: QuasiQuoter
npath = QuasiQuoter
  { quoteExp = npathExp
  , quotePat = const $ fail "npath quasi-quoter can only be used in expression contexts"
  , quoteType = const $ fail "npath quasi-quoter can only be used in expression contexts"
  , quoteDec = const $ fail "npath quasi-quoter can only be used in expression contexts"
  }

npathExp :: String -> Q Exp
npathExp s = case parse (pNormalizedPath stringLiteral <* eof) "" s of
  Left err -> fail $ "Parse error in npath quasi-quoter: " ++ show err
  Right result -> [| result |]