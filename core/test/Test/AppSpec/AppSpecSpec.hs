module Test.AppSpec.AppSpecSpec where

import Array qualified
import Core
import Test
import Test.AppSpec.AppSpec (AppSpec (..))
import Test.AppSpec.Scenario (Scenario (..))
import Test.AppSpec.Scenario qualified as Scenario


data TestAppModel = TestAppModel


testAppModel :: TestAppModel
testAppModel = TestAppModel


spec :: Spec Unit
spec =
  describe "specification for" do
    it "can build an empty app spec" \_ -> do
      let expectedAppSpec :: AppSpec TestAppModel =
            AppSpec
              { scenarios = Array.empty
              }

      let actualAppSpec = specificationFor testAppModel noScenarios

      actualAppSpec |> shouldBe expectedAppSpec

    it "can build an app spec with one scenario" \_ -> do
      let expectedAppSpec :: AppSpec TestAppModel =
            AppSpec
              { scenarios =
                  Array.fromLinkedList
                    [ Scenario
                        { name = "test scenario",
                          steps = Scenario.empty
                        }
                    ]
              }

      let actualAppSpec =
            specificationFor testAppModel do
              scenario "test scenario" emptyScenario

      actualAppSpec |> shouldBe expectedAppSpec

    it "can build an app spec with two scenarios" \_ -> do
      let expectedAppSpec :: AppSpec TestAppModel =
            AppSpec
              { scenarios =
                  Array.fromLinkedList
                    [ Scenario
                        { name = "test scenario 1",
                          steps = Scenario.empty
                        },
                      Scenario
                        { name = "test scenario 2",
                          steps = Scenario.empty
                        }
                    ]
              }

      let actualAppSpec =
            specificationFor testAppModel do
              scenario "test scenario 1" emptyScenario
              scenario "test scenario 2" emptyScenario

      actualAppSpec |> shouldBe expectedAppSpec