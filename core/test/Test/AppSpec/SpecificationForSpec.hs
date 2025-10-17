module Test.AppSpec.SpecificationForSpec where

import Core
import Test


spec :: Spec Unit
spec =
  describe "specification for" do
    it "fails" \_ -> do
      fail "not implemented"