module MyPrelude.URI
  ( module MyPrelude.URI,
    module X,
  )
where

import ClassyPrelude
import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Megaparsec
import Text.URI as X (URI (..), emptyURI)
import Text.URI qualified as URI
import Text.URI.QQ as X hiding (uri)
import Text.URI.QQ qualified as URI
import Prelude (ShowS)

quri :: QuasiQuoter
quri = URI.uri

renderURI :: URI -> Text
renderURI = URI.render

renderURIString :: URI -> String
renderURIString = URI.renderStr

renderURIShowS :: URI -> ShowS
renderURIShowS = URI.renderStr'

pURI :: (MonadParsec e Text m) => m URI
pURI = URI.parser
