module Models.Path.TH where

import Language.Haskell.Meta.Parse (parseExpWithExts)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Models.Path.Parse
import Models.Path.Simple
import MyPrelude
import Utils.Parsing (ttransition, whitespace)
import Utils.Parsing.Common

path :: QuasiQuoter
path =
  QuasiQuoter
    { quoteExp = pathExp,
      quotePat = const $ fail "path quasi-quoter only supports expressions",
      quoteType = const $ fail "path quasi-quoter only supports expressions",
      quoteDec = const $ fail "path quasi-quoter only supports expressions"
    }

pathExp :: String -> Q Exp
pathExp s = case runParser (whitespace *> pPath ttransition <* eof) "" (pack s) of
  Left err ->
    fail $ "Parse error in path quasi-quoter: " ++ errorBundlePretty err
  Right result -> handleDirectivesQ interpretSplice result
  where
    -- Interpret Splice directives as TH expressions
    interpretSplice :: SourceRange -> Directive 'WithDirectives -> Q Exp
    interpretSplice _ = \case
      LocationFromHistory _ ->
        fail "LocationFromHistory directive cannot be used in path quasi-quoter"
      Targets _ -> fail "Targets directive cannot be used in path quasi-quoter"
      Splice expr -> do
        extensions <- extsEnabled
        case parseExpWithExts extensions expr of
          Left (line, col, err) ->
            fail $
              "Parse error in splice: "
                ++ (show line ++ ":" ++ show col ++ ":\n")
                ++ err
          Right e -> pure e
