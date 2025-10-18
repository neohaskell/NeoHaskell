module Test.AppSpec.SpecificationForSpec where

import Core
import Test
import Test.AppSpec.Core (AppSpec (..))


data TestAppModel = TestAppModel


testAppModel :: TestAppModel
testAppModel = TestAppModel


spec :: Spec Unit
spec =
  describe "specification for" do
    it "can build an empty app spec" \_ -> do
      let emptyAppSpec = AppSpec @TestAppModel

      let actualAppSpec = specificationFor testAppModel noScenarios

      actualAppSpec |> shouldBe emptyAppSpec