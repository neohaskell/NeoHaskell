module Service.Transport.Web.HealthCheckSpec where

import Core
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as GhcKeyMap
import Data.ByteString.Lazy qualified as GhcLBS
import Prelude (Either (..), String)
import Service.Transport.Web (HealthCheckConfig (..), WebTransport (..), healthResponseBody, isHealthCheckPath, server)
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Health Check" do
    describe "default configuration" do
      it "server has health check enabled by default" \_ -> do
        case server.healthCheck of
          Nothing -> fail "Expected health check to be enabled by default"
          Just config -> config.healthPath |> shouldBe "health"

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

    describe "healthResponseBody" do
      it "is valid JSON" \_ -> do
        let decoded = Aeson.eitherDecode healthResponseBody :: Either String Aeson.Value
        case decoded of
          Left err -> fail (Text.fromLinkedList err)
          Right _ -> pass

      it "contains status ok" \_ -> do
        let decoded = Aeson.eitherDecode healthResponseBody :: Either String Aeson.Value
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
        let bodyText = GhcLBS.toStrict healthResponseBody
        bodyText |> shouldBe "{\"status\":\"ok\"}"

      it "health response body is minimal (no extra fields)" \_ -> do
        let decoded = Aeson.eitherDecode healthResponseBody :: Either String Aeson.Value
        case decoded of
          Left _ -> fail "Not valid JSON"
          Right (Aeson.Object obj) -> do
            let keyCount = GhcKeyMap.size obj
            keyCount |> shouldBe 1
          Right _ -> fail "Expected JSON object"
