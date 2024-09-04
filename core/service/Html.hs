module Html (
  Html,
  html,
) where

import Basics
import Language.Haskell.TH.Quote (QuasiQuoter)


data Html


html :: QuasiQuoter
html = dieWith "not implemented"