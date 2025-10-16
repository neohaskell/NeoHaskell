module Test.AppModel.AppSpecSpec where

import Core
import Test
import Test.AppSpec.Core (AppSpec (..))
import Test.AppSpec.Verify qualified as Verify


spec :: Spec Unit
spec =
  describe "AppSpec" do
    describe "verify" do
      it "does not verify any scenario if there aren't any" \_ -> do
        let appSpec = AppSpec @MyAppModel

        _ <- Verify.run mockVerifyOps appSpec

        fail "not implemented"


data MyAppModel = MyAppModel


mockVerifyOps :: Verify.Ops
mockVerifyOps = Verify.Ops