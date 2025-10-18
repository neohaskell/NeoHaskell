module Test.AppSpec.SpecificationForSpec where

import Core
import Test
import Test.AppSpec (ScenarioDef (..))
import Test.AppSpec.Core (AppSpec (..))


data TestAppModel = TestAppModel


testAppModel :: TestAppModel
testAppModel = TestAppModel


spec :: Spec Unit
spec =
  describe "specification for" do
    it "can build an empty app spec" \_ -> do
      let expectedAppSpec = AppSpec @TestAppModel

      let actualAppSpec = specificationFor testAppModel noScenarios

      actualAppSpec |> shouldBe expectedAppSpec

    it "can build an app spec with one scenario" \_ -> do
      let expectedAppSpec :: AppSpec TestAppModel =
            AppSpec
              { scenarios =
                  Array.fromList
                    [ ScenarioDef
                        { name = "test scenario"
                        }
                    ]
              }

      let actualAppSpec =
            specificationFor testAppModel do
              scenario "test scenario" emptyScenario

      actualAppSpec |> shouldBe expectedAppSpec