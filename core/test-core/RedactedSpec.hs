module RedactedSpec where

import Core
import Data.Aeson qualified as Aeson
import Data.Either (Either (..))
import Redacted qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Redacted" do
    describe "wrap and unwrap" do
      it "wraps and unwraps a value" \_ -> do
        let secret = Redacted.wrap ("my-secret" :: Text)
        Redacted.unwrap secret |> shouldBe "my-secret"

      it "preserves the wrapped value type" \_ -> do
        let secretInt = Redacted.wrap (42 :: Int)
        Redacted.unwrap secretInt |> shouldBe 42

    describe "Show instance" do
      it "shows <redacted> for wrapped values" \_ -> do
        let secret = Redacted.wrap ("super-secret-key" :: Text)
        let shown = toText secret
        shown |> shouldBe "<redacted>"

      it "does not reveal the actual value in Show output" \_ -> do
        let secret = Redacted.wrap ("DO_NOT_SHOW_THIS" :: Text)
        let shown = toText secret
        shown |> shouldSatisfy (\s -> not (Text.contains "DO_NOT_SHOW_THIS" s))

    describe "labeled" do
      it "wraps with a label" \_ -> do
        let secret = Redacted.labeled "api-key" ("sk-xxx" :: Text)
        Redacted.unwrap secret |> shouldBe "sk-xxx"

      it "shows <redacted: label> for labeled values" \_ -> do
        let secret = Redacted.labeled "api-key" ("sk-xxx" :: Text)
        let shown = toText secret
        shown |> shouldBe "<redacted: api-key>"

      it "does not reveal the actual value in labeled Show output" \_ -> do
        let secret = Redacted.labeled "key" ("SUPER_SECRET" :: Text)
        let shown = toText secret
        shown |> shouldSatisfy (\s -> not (Text.contains "SUPER_SECRET" s))

    describe "empty" do
      it "creates an empty Redacted Text" \_ -> do
        let emptySecret = Redacted.empty @Text
        Redacted.unwrap emptySecret |> shouldBe ""

    describe "FromJSON instance" do
      it "parses JSON values into Redacted" \_ -> do
        let jsonStr = "\"secret-value\""
        case Aeson.eitherDecodeStrict @(Redacted Text) jsonStr of
          Right secret -> Redacted.unwrap secret |> shouldBe "secret-value"
          Left _ -> True |> shouldBe False -- Force test failure

      it "parses JSON numbers into Redacted" \_ -> do
        let jsonStr = "42"
        case Aeson.eitherDecodeStrict @(Redacted Int) jsonStr of
          Right secret -> Redacted.unwrap secret |> shouldBe 42
          Left _ -> True |> shouldBe False -- Force test failure

    describe "security properties" do
      it "prevents accidental exposure in string interpolation" \_ -> do
        let apiKey = Redacted.wrap ("sk-live-xxx" :: Text)
        let logMessage = [fmt|Using API key: #{apiKey}|]
        logMessage |> shouldSatisfy (\s -> Text.contains "<redacted>" s)
        logMessage |> shouldSatisfy (\s -> not (Text.contains "sk-live-xxx" s))

      it "prevents exposure when converting to list of chars" \_ -> do
        let secret = Redacted.wrap ("password123" :: Text)
        let asString = toText secret
        asString |> shouldSatisfy (\s -> not (Text.contains "password123" s))
