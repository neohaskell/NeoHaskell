module Integration.Agent.Spec (spec) where

import Array qualified
import Basics
import Integration.Agent qualified as Agent
import Integration.Agent.TestFixtures
import Integration.Agent.Types (CommandTool (..), Request (..), defaultConfig)
import Json qualified


import Test.Hspec

import Text qualified


spec :: Spec
spec = do
  describe "Integration.Agent" do
    describe "commandTool" do
      it "tool name comes from NameOf instance" do
        let tool = Agent.commandTool @AddItemCommand
        tool.toolName `shouldBe` "AddItem"

      it "description comes from Documented instance" do
        let tool = Agent.commandTool @AddItemCommand
        tool.toolDescription `shouldBe` "Add an item to the shopping cart"

      it "empty description falls back to type name" do
        let tool = Agent.commandTool @ClearCartCommand
        tool.toolDescription `shouldBe` "ClearCart"

      it "toolDefinition has OpenRouter function wrapper format" do
        let tool = Agent.commandTool @AddItemCommand
        let encodedType = Json.encodeText tool.toolDefinition
        Text.contains "\"type\"" encodedType `shouldBe` True
        Text.contains "\"function\"" encodedType `shouldBe` True

      it "parameters schema equals toJsonSchema output" do
        let tool = Agent.commandTool @AddItemCommand
        let encodedDef = Json.encodeText tool.toolDefinition
        -- The definition should contain a "parameters" key
        Text.contains "\"parameters\"" encodedDef `shouldBe` True

      it "commandTool is deterministic - two calls produce equal values" do
        let tool1 = Agent.commandTool @AddItemCommand
        let tool2 = Agent.commandTool @AddItemCommand
        (tool1 == tool2) `shouldBe` True

    describe "agent" do
      it "smart constructor fills all fields with defaultConfig" do
        let tool = Agent.commandTool @AddItemCommand
        let req = Agent.agent
              "What should I add?"
              (Array.fromLinkedList [tool])
              "anthropic/claude-3.5-sonnet"
              (\err -> CommandError err)
        req.prompt `shouldBe` "What should I add?"
        req.model `shouldBe` "anthropic/claude-3.5-sonnet"
        (req.config == defaultConfig) `shouldBe` True
        Array.length req.tools `shouldBe` 1

      it "onError callback is preserved and callable" do
        let req = Agent.agent
              "prompt"
              Array.empty
              "model"
              (\err -> CommandError err)
        let result = req.onError "boom"
        result `shouldBe` CommandError "boom"

      it "empty tools list is preserved by constructor" do
        let req = Agent.agent
              "p"
              Array.empty
              "model"
              (\err -> CommandError err)
        Array.length req.tools `shouldBe` 0

      it "empty prompt and model are not mutated" do
        let tool = Agent.commandTool @AddItemCommand
        let req = Agent.agent
              ""
              (Array.fromLinkedList [tool])
              ""
              (\err -> CommandError err)
        req.prompt `shouldBe` ""
        req.model `shouldBe` ""
