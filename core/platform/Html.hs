module Html (
  Html,
  html,
) where

import IHP.HSX.QQ qualified
import Language.Haskell.TH.Quote qualified
import Text.Blaze.Html (Html)


html :: Language.Haskell.TH.Quote.QuasiQuoter
html = IHP.HSX.QQ.hsx