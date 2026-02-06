module Config.CoreSpec where

import Config.Core (ConfigError (..), FieldDef (..), FieldModifier (..), validateFieldDef)
import Core
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList qualified
import Test


-- | Helper to create a minimal FieldDef for testing
makeFieldDef :: Text -> [FieldModifier] -> FieldDef
makeFieldDef name mods =
  FieldDef
    { fieldName = name
    , fieldType = TH.ConT (TH.mkName "Int")
    , fieldModifiers = mods
    }


-- | Helper to create a ModDefault with a lifted Int value
-- Uses TH.lift to create the Q Exp required by ModDefault
makeDefaultMod :: Int -> FieldModifier
makeDefaultMod val = ModDefault (TH.lift val)


-- | Helper to check if a list contains an element
listContains :: forall element. (Eq element) => element -> [element] -> Bool
listContains item list = LinkedList.member item list


spec :: Spec Unit
spec = do
  describe "Config.Core" do
    describe "validateFieldDef" do
      it "returns MissingDoc when doc is missing" \_ -> do
        let fd = makeFieldDef "port" [ModRequired]
        let errors = validateFieldDef fd
        listContains (MissingDoc "port") errors |> shouldBe True

      it "returns MissingDefaultOrRequired when neither is present" \_ -> do
        let fd = makeFieldDef "port" [ModDoc "test"]
        let errors = validateFieldDef fd
        listContains (MissingDefaultOrRequired "port") errors |> shouldBe True

      it "returns BothDefaultAndRequired when both are present" \_ -> do
        let fd = makeFieldDef "port" [ModDoc "test", ModRequired, makeDefaultMod 8080]
        let errors = validateFieldDef fd
        listContains (BothDefaultAndRequired "port") errors |> shouldBe True

      it "returns empty list for valid field with default" \_ -> do
        let fd = makeFieldDef "port" [ModDoc "test", makeDefaultMod 8080]
        let errors = validateFieldDef fd
        errors |> shouldBe []

      it "returns empty list for valid field with required" \_ -> do
        let fd = makeFieldDef "port" [ModDoc "test", ModRequired]
        let errors = validateFieldDef fd
        errors |> shouldBe []

      it "can return multiple errors at once" \_ -> do
        let fd = makeFieldDef "port" []
        let errors = validateFieldDef fd
        -- Should have MissingDoc and MissingDefaultOrRequired
        LinkedList.length errors |> shouldBe 2

    describe "ConfigError" do
      it "MissingDoc contains field name" \_ -> do
        let err = MissingDoc "myField"
        case err of
          MissingDoc name -> name |> shouldBe "myField"
          _ -> fail "Expected MissingDoc"

      it "MissingDefaultOrRequired contains field name" \_ -> do
        let err = MissingDefaultOrRequired "myField"
        case err of
          MissingDefaultOrRequired name -> name |> shouldBe "myField"
          _ -> fail "Expected MissingDefaultOrRequired"

      it "BothDefaultAndRequired contains field name" \_ -> do
        let err = BothDefaultAndRequired "myField"
        case err of
          BothDefaultAndRequired name -> name |> shouldBe "myField"
          _ -> fail "Expected BothDefaultAndRequired"

      it "ConfigError has Eq instance" \_ -> do
        let err1 = MissingDoc "field1"
        let err2 = MissingDoc "field1"
        let err3 = MissingDoc "field2"
        (err1 == err2) |> shouldBe True
        (err1 == err3) |> shouldBe False
