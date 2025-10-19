module Models.NormalizedPath.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Models.NID
import Models.NormalizedPath
import Models.NormalizedPath.Parse
import MyPrelude
import Utils.Parsing (ttransition, whitespace)
import Utils.Parsing.Common

npath :: QuasiQuoter
npath =
  QuasiQuoter
    { quoteExp = unTypeCode . npathExp' pure,
      quotePat = const $ fail "npath quasi-quoter can only be used in expression contexts",
      quoteType = const $ fail "npath quasi-quoter can only be used in expression contexts",
      quoteDec = const $ fail "npath quasi-quoter can only be used in expression contexts"
    }

npathNID :: QuasiQuoter
npathNID =
  QuasiQuoter
    { quoteExp = \s ->
        let justSpecific = \case
              Specific nid -> Just nid
              _ -> Nothing
            p = unTypeCode $ npathExp' justSpecific s
         in [|$p :: NormalizedPath NID Text|],
      quotePat = const $ fail "npathNID quasi-quoter can only be used in expression contexts",
      quoteType = const $ fail "npathNID quasi-quoter can only be used in expression contexts",
      quoteDec = const $ fail "npathNID quasi-quoter can only be used in expression contexts"
    }

npathExp' ::
  forall a m.
  (Ord a, Lift a, Quote m, MonadFail m) =>
  (Anchor -> Maybe a) ->
  String ->
  Code m (NormalizedPath a Text)
npathExp' specializeAnchors s = do
  case runParser (whitespace *> pNormalizedPath ttransition <* eof) "" (pack s) of
    Left err ->
      Code . fail $
        "Parse error in npath quasi-quoter: " ++ errorBundlePretty err
    Right result ->
      case traverseAnchors specializeAnchors result of
        Nothing ->
          Code . fail $
            "Failed to specialize anchors in npath quasi-quoter"
        Just result' -> [||result'||]
