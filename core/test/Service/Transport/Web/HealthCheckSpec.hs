module Service.Transport.Web.HealthCheckSpec where

import Core
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as GhcKeyMap
import Data.ByteString.Lazy qualified as GhcLBS
import Prelude (Either (..), String)
import Array qualified
import Service.Transport.Web (HealthCheckConfig (..), IntegrationStatus (..), WebTransport (..), buildHealthResponse, isHealthCheckPath, server)
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Health Check" do
    describe "default configuration" do
      it "server has health check enabled by default" \_ -> do
        case server.healthCheck of
          Nothing -> fail "Expected health check to be enabled by default"
          Just _ -> pass

      it "default health path is 'health'" \_ -> do
        case server.healthCheck of
          Nothing -> fail "Expected health check to be enabled"
          Just config -> config.healthPath |> shouldBe "health"

    describe "isHealthCheckPath" do
      it "returns True when path matches configured health path" \_ -> do
        let transport = server
        isHealthCheckPath "health" transport |> shouldBe True

      it "returns False when path does not match" \_ -> do
        let transport = server
        isHealthCheckPath "other" transport |> shouldBe False

      it "returns False when health check is disabled" \_ -> do
        let transport = server {healthCheck = Nothing}
        isHealthCheckPath "health" transport |> shouldBe False

      it "matches custom health path" \_ -> do
        let transport = server {healthCheck = Just HealthCheckConfig {healthPath = "_health"}}
        isHealthCheckPath "_health" transport |> shouldBe True

      it "does not match default path when custom path is set" \_ -> do
        let transport = server {healthCheck = Just HealthCheckConfig {healthPath = "_health"}}
        isHealthCheckPath "health" transport |> shouldBe False

    describe "buildHealthResponse" do
      it "is valid JSON" \_ -> do
        let body = buildHealthResponse server
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right _ -> pass

      it "contains status ok" \_ -> do
        let body = buildHealthResponse server
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right val -> do
            case val of
              Aeson.Object obj -> do
                case GhcKeyMap.lookup "status" obj of
                  Nothing -> fail "Missing 'status' key"
                  Just (Aeson.String s) -> s |> shouldBe "ok"
                  Just _ -> fail "Expected string value for 'status'"
              _ -> fail "Expected JSON object"

    describe "security" do
      it "health response body does not contain version info" \_ -> do
        let bodyText = GhcLBS.toStrict (buildHealthResponse server)
        bodyText |> shouldBe "{\"status\":\"ok\"}"

      it "health response body is minimal for server with no integration status" \_ -> do
        let body = buildHealthResponse server
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left _ -> fail "Not valid JSON"
          Right (Aeson.Object obj) -> do
            let keyCount = GhcKeyMap.size obj
            keyCount |> shouldBe 1
          Right _ -> fail "Expected JSON object"

    describe "integration status reporting (ADR-0055)" do
      it "omits the 'integrations' key in real mode (integrationStatus = Nothing)" \_ -> do
        let transport = server {integrationStatus = Nothing}
        let body = buildHealthResponse transport
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right (Aeson.Object obj) ->
            case GhcKeyMap.lookup "integrations" obj of
              Nothing -> pass
              Just _ -> fail "Expected 'integrations' key to be absent in real mode"
          Right _ -> fail "Expected JSON object"

      it "includes integrations.mode=fake when all integrations are faked" \_ -> do
        let status = IntegrationStatus {mode = "fake", fakes = Array.fromLinkedList ["Sendgrid", "Stripe"]}
        let transport = server {integrationStatus = Just status}
        let body = buildHealthResponse transport
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right (Aeson.Object obj) ->
            case GhcKeyMap.lookup "integrations" obj of
              Nothing -> fail "Expected 'integrations' key in fake mode"
              Just (Aeson.Object inner) -> do
                case GhcKeyMap.lookup "mode" inner of
                  Just (Aeson.String m) -> m |> shouldBe "fake"
                  _ -> fail "Expected string 'mode' inside integrations"
                case GhcKeyMap.lookup "fakes" inner of
                  Just (Aeson.Array arr) ->
                    Aeson.encode (Aeson.Array arr) |> GhcLBS.toStrict |> shouldBe "[\"Sendgrid\",\"Stripe\"]"
                  _ -> fail "Expected array 'fakes'"
              Just _ -> fail "Expected integrations to be an object"
          Right _ -> fail "Expected JSON object"

      it "includes integrations.mode=hybrid with named fakes" \_ -> do
        let status = IntegrationStatus {mode = "hybrid", fakes = Array.fromLinkedList ["Sendgrid"]}
        let transport = server {integrationStatus = Just status}
        let body = buildHealthResponse transport
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right (Aeson.Object obj) ->
            case GhcKeyMap.lookup "integrations" obj of
              Nothing -> fail "Expected 'integrations' key in hybrid mode"
              Just (Aeson.Object inner) -> do
                case GhcKeyMap.lookup "mode" inner of
                  Just (Aeson.String m) -> m |> shouldBe "hybrid"
                  _ -> fail "Expected string 'mode'"
                case GhcKeyMap.lookup "fakes" inner of
                  Just (Aeson.Array arr) ->
                    Aeson.encode (Aeson.Array arr) |> GhcLBS.toStrict |> shouldBe "[\"Sendgrid\"]"
                  _ -> fail "Expected array 'fakes'"
              Just _ -> fail "Expected integrations to be an object"
          Right _ -> fail "Expected JSON object"

      it "integrations.fakes is an empty array when hybrid fakes list is empty" \_ -> do
        let status = IntegrationStatus {mode = "hybrid", fakes = Array.fromLinkedList []}
        let transport = server {integrationStatus = Just status}
        let body = buildHealthResponse transport
        let decoded = Aeson.eitherDecode body :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right (Aeson.Object obj) ->
            case GhcKeyMap.lookup "integrations" obj of
              Nothing -> fail "Expected 'integrations' key in hybrid mode"
              Just (Aeson.Object inner) ->
                case GhcKeyMap.lookup "fakes" inner of
                  Just (Aeson.Array arr) ->
                    Aeson.encode (Aeson.Array arr) |> GhcLBS.toStrict |> shouldBe "[]"
                  _ -> fail "Expected array 'fakes'"
              Just _ -> fail "Expected integrations to be an object"
          Right _ -> fail "Expected JSON object"

      it "correctly escapes special characters in fake names (JSON injection guard)" \_ -> do
        let status = IntegrationStatus {mode = "hybrid", fakes = Array.fromLinkedList ["Bad\"Name"]}
        let transport = server {integrationStatus = Just status}
        let body = buildHealthResponse transport
        case Aeson.eitherDecode body :: Either String Aeson.Value of
          Left err -> fail (Text.fromLinkedList err)
          Right (Aeson.Object obj) ->
            case GhcKeyMap.lookup "integrations" obj of
              Nothing -> fail "Expected 'integrations' key"
              Just (Aeson.Object inner) ->
                case GhcKeyMap.lookup "fakes" inner of
                  Just (Aeson.Array arr) ->
                    Aeson.encode (Aeson.Array arr) |> GhcLBS.toStrict |> shouldBe "[\"Bad\\\"Name\"]"
                  _ -> fail "Expected array 'fakes'"
              Just _ -> fail "Expected integrations to be an object"
          Right _ -> fail "Expected JSON object"
