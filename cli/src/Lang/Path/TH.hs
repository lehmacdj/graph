{-# LANGUAGE TemplateHaskell #-}

module Lang.Path.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Lang.Path.Parse
import Lang.Path
import Lang.Parsing
import Text.Megaparsec (parse, eof)
import MyPrelude
import Control.Monad.Fail (MonadFail(fail))

path :: QuasiQuoter
path = QuasiQuoter
  { quoteExp = pathExp
  , quotePat = const $ fail "path quasi-quoter can only be used in expression contexts"
  , quoteType = const $ fail "path quasi-quoter can only be used in expression contexts"
  , quoteDec = const $ fail "path quasi-quoter can only be used in expression contexts"
  }

pathExp :: String -> Q Exp
pathExp s = case parse (pPath stringLiteral <* eof) "" s of
  Left err -> fail $ "Parse error in path quasi-quoter: " ++ show err
  Right result -> [| result |]