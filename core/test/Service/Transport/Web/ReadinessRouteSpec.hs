module Service.Transport.Web.ReadinessRouteSpec where

import Core
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as GhcLBS
import Network.HTTP.Types.Status qualified as HTTP
import Service.Query.Subscriber (Readiness (..))
import Service.Transport.Web (WebTransport (..))
import Service.Transport.Web qualified as Web
import Service.Transport.Web.Readiness (ReadinessConfig (..))
import Test


spec :: Spec Unit
spec = do
  describe "isReadinessPath" do
    it "returns True when path matches configured readiness path" \_ -> do
      let transport = Web.server
            { readinessConfig = Just ReadinessConfig {readinessPath = "ready", includeQueryStatus = True}
            }
      Web.isReadinessPath "ready" transport |> shouldBe True

    it "returns False when path does not match" \_ -> do
      let transport = Web.server
            { readinessConfig = Just ReadinessConfig {readinessPath = "ready", includeQueryStatus = True}
            }
      Web.isReadinessPath "other" transport |> shouldBe False

    it "returns False when readinessConfig is Nothing" \_ -> do
      let transport = Web.server {readinessConfig = Nothing}
      Web.isReadinessPath "ready" transport |> shouldBe False

    it "matches custom readiness path" \_ -> do
      let transport = Web.server
            { readinessConfig = Just ReadinessConfig {readinessPath = "_ready", includeQueryStatus = True}
            }
      Web.isReadinessPath "_ready" transport |> shouldBe True

  describe "renderReadiness" do
    it "returns 200 with {status:ready} when Ready" \_ -> do
      let (status, body) = Web.renderReadiness Ready
      status |> shouldBe HTTP.status200
      GhcLBS.toStrict body |> shouldBe "{\"status\":\"ready\"}"

    it "returns 503 with {status:rebuilding} when Rebuilding" \_ -> do
      let (status, body) = Web.renderReadiness Rebuilding
      status |> shouldBe HTTP.status503
      GhcLBS.toStrict body |> shouldBe "{\"status\":\"rebuilding\"}"

    it "returns 503 when Failed" \_ -> do
      let (status, _body) = Web.renderReadiness (Failed "db connection lost")
      status |> shouldBe HTTP.status503

    it "Failed body parses as exactly {status:failed,reason:<reason>} per ADR-0059" \_ -> do
      -- Assert the full JSON shape, not just substring matches — the body is an
      -- externally-observable contract for k8s/load-balancer probes.
      let (_status, body) = Web.renderReadiness (Failed "db connection lost")
      case Aeson.decode body :: Maybe Aeson.Value of
        Nothing -> fail [fmt|Failed body is not valid JSON: #{toText (show body)}|]
        Just parsed -> do
          let expected =
                Aeson.object
                  [ "status" Aeson..= ("failed" :: Text)
                  , "reason" Aeson..= ("db connection lost" :: Text)
                  ]
          parsed |> shouldBe expected
