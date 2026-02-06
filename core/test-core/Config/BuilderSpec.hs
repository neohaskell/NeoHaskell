{-# LANGUAGE LambdaCase #-}

module Config.BuilderSpec where

import Config.Builder (cliLong, cliShort, defaultsTo, doc, envPrefix, envVar, field, required, secret)
import Config.Core (FieldDef (..), FieldModifier (..))
import Core
import Data.Maybe qualified as GhcMaybe
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList qualified
import Test


-- | Helper to check if a modifier is present in a FieldDef
hasModifier :: (FieldModifier -> Bool) -> FieldDef -> Bool
hasModifier predicate fd = LinkedList.any predicate fd.fieldModifiers


-- | Helper to find a specific modifier
findModifier :: (FieldModifier -> Maybe value) -> FieldDef -> Maybe value
findModifier extract fd = do
  let results = GhcMaybe.mapMaybe extract fd.fieldModifiers
  case results of
    [] -> Nothing
    (x : _) -> Just x


spec :: Spec Unit
spec = do
  describe "Config.Builder" do
    describe "field" do
      it "creates a FieldDef with the given name" \_ -> do
        let fd = field @Int "port"
        fd.fieldName |> shouldBe "port"

      it "creates a FieldDef with empty modifiers" \_ -> do
        let fd = field @Int "port"
        LinkedList.length fd.fieldModifiers |> shouldBe 0

      it "captures the type as TH.Type" \_ -> do
        let fd = field @Int "port"
        -- The fieldType should be a TH.ConT with the type name
        case fd.fieldType of
          TH.ConT _ -> True |> shouldBe True
          _ -> fail "Expected TH.ConT"

    describe "doc" do
      it "adds ModDoc modifier" \_ -> do
        let fd = field @Int "port" |> doc "HTTP port"
        let isDoc = \case
              ModDoc _ -> True
              _ -> False
        hasModifier isDoc fd |> shouldBe True

      it "preserves the documentation text" \_ -> do
        let fd = field @Int "port" |> doc "HTTP port"
        let extractDoc = \case
              ModDoc txt -> Just txt
              _ -> Nothing
        findModifier extractDoc fd |> shouldBe (Just "HTTP port")

    describe "envVar" do
      it "adds ModEnvVar modifier" \_ -> do
        let fd = field @Int "port" |> envVar "PORT"
        let isEnvVar = \case
              ModEnvVar _ -> True
              _ -> False
        hasModifier isEnvVar fd |> shouldBe True

      it "preserves the env var name" \_ -> do
        let fd = field @Int "port" |> envVar "MY_PORT"
        let extractEnvVar = \case
              ModEnvVar name -> Just name
              _ -> Nothing
        findModifier extractEnvVar fd |> shouldBe (Just "MY_PORT")

    describe "defaultsTo" do
      it "adds ModDefault modifier" \_ -> do
        let fd = field @Int "port" |> defaultsTo (8080 :: Int)
        let isDefault = \case
              ModDefault _ -> True
              _ -> False
        hasModifier isDefault fd |> shouldBe True

    describe "required" do
      it "adds ModRequired modifier" \_ -> do
        let fd = field @Int "port" |> required
        let isRequired = \case
              ModRequired -> True
              _ -> False
        hasModifier isRequired fd |> shouldBe True

    describe "secret" do
      it "adds ModSecret modifier" \_ -> do
        let fd = field @Text "apiKey" |> secret
        let isSecret = \case
              ModSecret -> True
              _ -> False
        hasModifier isSecret fd |> shouldBe True

    describe "cliLong" do
      it "adds ModCliLong modifier" \_ -> do
        let fd = field @Int "port" |> cliLong "port"
        let isCliLong = \case
              ModCliLong _ -> True
              _ -> False
        hasModifier isCliLong fd |> shouldBe True

      it "preserves the long option name" \_ -> do
        let fd = field @Int "port" |> cliLong "http-port"
        let extractCliLong = \case
              ModCliLong name -> Just name
              _ -> Nothing
        findModifier extractCliLong fd |> shouldBe (Just "http-port")

    describe "cliShort" do
      it "adds ModCliShort modifier" \_ -> do
        let fd = field @Int "port" |> cliShort 'p'
        let isCliShort = \case
              ModCliShort _ -> True
              _ -> False
        hasModifier isCliShort fd |> shouldBe True

      it "preserves the short option character" \_ -> do
        let fd = field @Int "port" |> cliShort 'p'
        let extractCliShort = \case
              ModCliShort c -> Just c
              _ -> Nothing
        findModifier extractCliShort fd |> shouldBe (Just 'p')

    describe "envPrefix" do
      it "adds ModEnvPrefix modifier" \_ -> do
        let fd = field @Int "database" |> envPrefix "DB_"
        let isEnvPrefix = \case
              ModEnvPrefix _ -> True
              _ -> False
        hasModifier isEnvPrefix fd |> shouldBe True

      it "preserves the prefix" \_ -> do
        let fd = field @Int "database" |> envPrefix "DB_"
        let extractEnvPrefix = \case
              ModEnvPrefix prefix -> Just prefix
              _ -> Nothing
        findModifier extractEnvPrefix fd |> shouldBe (Just "DB_")

    describe "pipe composition" do
      it "allows chaining multiple modifiers" \_ -> do
        let fd =
              field @Int "port"
                |> doc "HTTP port"
                |> envVar "PORT"
                |> defaultsTo (8080 :: Int)
                |> cliLong "port"
                |> cliShort 'p'
        -- Should have 5 modifiers
        LinkedList.length fd.fieldModifiers |> shouldBe 5

      it "preserves all modifiers in chain" \_ -> do
        let fd =
              field @Int "port"
                |> doc "HTTP port"
                |> required
                |> cliLong "port"
        let hasDoc' = hasModifier (\case ModDoc _ -> True; _ -> False) fd
        let hasRequired' = hasModifier (\case ModRequired -> True; _ -> False) fd
        let hasCliLong' = hasModifier (\case ModCliLong _ -> True; _ -> False) fd
        hasDoc' |> shouldBe True
        hasRequired' |> shouldBe True
        hasCliLong' |> shouldBe True

      it "field name is preserved through chain" \_ -> do
        let fd =
              field @Int "myPort"
                |> doc "test"
                |> required
        fd.fieldName |> shouldBe "myPort"
