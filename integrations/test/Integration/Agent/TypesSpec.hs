module Integration.Agent.TypesSpec (spec) where

import Basics

import Integration.Agent.Types (CommandTool (..), Config (..), defaultConfig)
import Json qualified
import Maybe (Maybe (..))
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do
  describe "Integration.Agent.Types" do
    describe "defaultConfig" do
      it "defaults match contract: systemPrompt Nothing, temperature Nothing, maxTokens Nothing, timeoutSeconds 120" do
        let config = defaultConfig
        config.systemPrompt `shouldBe` Nothing
        config.temperature `shouldBe` Nothing
        config.maxTokens `shouldBe` Nothing
        config.timeoutSeconds `shouldBe` 120

      it "timeout default is 120 seconds" do
        defaultConfig.timeoutSeconds `shouldBe` 120

      it "optional tuning fields default to Nothing" do
        defaultConfig.systemPrompt `shouldBe` (Nothing :: Maybe Text)
        defaultConfig.temperature `shouldBe` (Nothing :: Maybe Float)
        defaultConfig.maxTokens `shouldBe` (Nothing :: Maybe Int)

      it "pure value is stable across calls" do
        (defaultConfig == defaultConfig) `shouldBe` True

    describe "CommandTool" do
      describe "Eq" do
        it "Eq true for identical values" do
          let toolA = CommandTool
                { toolName = "AddItem"
                , toolDescription = "Add an item"
                , toolDefinition = Json.object [("type", Json.toJSON ("function" :: Text))]
                }
          let toolAClone = CommandTool
                { toolName = "AddItem"
                , toolDescription = "Add an item"
                , toolDefinition = Json.object [("type", Json.toJSON ("function" :: Text))]
                }
          (toolA == toolAClone) `shouldBe` True

        it "Eq false when toolName differs" do
          let toolA = CommandTool
                { toolName = "AddItem"
                , toolDescription = "desc"
                , toolDefinition = Json.null
                }
          let toolDifferentName = CommandTool
                { toolName = "RemoveItem"
                , toolDescription = "desc"
                , toolDefinition = Json.null
                }
          (toolA == toolDifferentName) `shouldBe` False

        it "Eq false when toolDefinition differs" do
          let toolA = CommandTool
                { toolName = "AddItem"
                , toolDescription = "desc"
                , toolDefinition = Json.object [("type", Json.toJSON ("function" :: Text))]
                }
          let toolDifferentDef = CommandTool
                { toolName = "AddItem"
                , toolDescription = "desc"
                , toolDefinition = Json.null
                }
          (toolA == toolDifferentDef) `shouldBe` False

        it "Show includes stable field labels" do
          let toolA = CommandTool
                { toolName = "AddItem"
                , toolDescription = "desc"
                , toolDefinition = Json.null
                }
          let shown = show toolA
          Text.contains "toolName" (Text.fromLinkedList shown)
            `shouldBe` True
          Text.contains "toolDescription" (Text.fromLinkedList shown)
            `shouldBe` True
          Text.contains "toolDefinition" (Text.fromLinkedList shown)
            `shouldBe` True
