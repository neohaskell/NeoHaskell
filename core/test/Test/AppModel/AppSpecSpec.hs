module Test.AppModel.AppSpecSpec where

import Core
import Test


spec :: Spec Unit
spec =
  describe "AppSpec" do
    describe "verify" do
      it "fails" \_ -> do
        fail "not implemented"