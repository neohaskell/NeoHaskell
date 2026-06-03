module Service.Transport.Web.ReadinessRouteSpec where

import Core
import Data.ByteString.Lazy qualified as GhcLBS
import Data.ByteString qualified as GhcBS
import Network.HTTP.Types.Status qualified as HTTP
import Service.Query.Subscriber (Readiness (..))
import Service.Transport.Web (
  WebTransport (..),
  isReadinessPath,
  renderReadiness,
  server,
  )
import Service.Transport.Web.Readiness (ReadinessConfig (..))
import Test


spec :: Spec Unit
spec = do
  describe "isReadinessPath" do
    it "returns True when path matches configured readiness path" \_ -> do
      let transport = server
            { readinessConfig = Just ReadinessConfig {readinessPath = "ready", includeQueryStatus = True}
            }
      isReadinessPath "ready" transport |> shouldBe True

    it "returns False when path does not match" \_ -> do
      let transport = server
            { readinessConfig = Just ReadinessConfig {readinessPath = "ready", includeQueryStatus = True}
            }
      isReadinessPath "other" transport |> shouldBe False

    it "returns False when readinessConfig is Nothing" \_ -> do
      let transport = server {readinessConfig = Nothing}
      isReadinessPath "ready" transport |> shouldBe False

    it "matches custom readiness path" \_ -> do
      let transport = server
            { readinessConfig = Just ReadinessConfig {readinessPath = "_ready", includeQueryStatus = True}
            }
      isReadinessPath "_ready" transport |> shouldBe True

  describe "renderReadiness" do
    it "returns 200 with {status:ready} when Ready" \_ -> do
      let (status, body) = renderReadiness Ready
      status |> shouldBe HTTP.status200
      GhcLBS.toStrict body |> shouldBe "{\"status\":\"ready\"}"

    it "returns 503 with {status:rebuilding} when Rebuilding" \_ -> do
      let (status, body) = renderReadiness Rebuilding
      status |> shouldBe HTTP.status503
      GhcLBS.toStrict body |> shouldBe "{\"status\":\"rebuilding\"}"

    it "returns 503 when Failed" \_ -> do
      let (status, _body) = renderReadiness (Failed "db connection lost")
      status |> shouldBe HTTP.status503

    it "includes the failure reason in the Failed response body" \_ -> do
      let (_status, body) = renderReadiness (Failed "db connection lost")
      let bodyStrict = GhcLBS.toStrict body
      bodyStrict |> GhcBS.isInfixOf "failed" |> shouldBe True
      bodyStrict |> GhcBS.isInfixOf "db connection lost" |> shouldBe True
