module Core.DocTestSpec where

import Test.DocTest
import Test.Hspec
import Prelude


spec :: Spec
spec =
  describe "core doctests" $
    it "should pass all doctests" $
      doctest
        [ "-isrc",
          "core/Array.hs"
        ]
