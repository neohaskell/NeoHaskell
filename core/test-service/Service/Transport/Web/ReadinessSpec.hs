module Service.Transport.Web.ReadinessSpec where

import Core
import Service.Transport.Web.Readiness (ReadinessConfig (..))
import Service.Transport.Web.Readiness qualified as WebReadiness
import Test


-- | Default readiness config used across tests.
defaultReadinessConfig :: ReadinessConfig
defaultReadinessConfig = ReadinessConfig
  { readinessPath = "ready"
  , includeQueryStatus = True
  }


spec :: Spec Unit
spec = do
  describe "HTTP GET /ready" do
    it "returns 200 OK with status:ready when all queries are caught up" \_ -> do
      -- When readinessOf returns Ready, the handler must emit HTTP 200.
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready 200 ready: not implemented — stub must fail"

    it "returns 503 Service Unavailable with status:rebuilding when any query is replaying" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready 503 rebuilding: not implemented — stub must fail"

    it "includes per-query lag and position in rebuilding response" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready lag/position in rebuilding response: not implemented — stub must fail"

    it "includes estimatedSecondsRemaining when rebuilding" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready estimatedSecondsRemaining: not implemented — stub must fail"

    it "returns 503 with status:failed and failedQueries list when any query has failed" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready 503 failed: not implemented — stub must fail"

    it "includes all failed queries in the failedQueries array" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready all failedQueries: not implemented — stub must fail"

    it "does not include rebuilding queries in failedQueries when ready/rebuilding state" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready no overlap in failedQueries: not implemented — stub must fail"

    it "returns 404 if /ready endpoint is disabled via withoutReadinessEndpoint" \_ -> do
      let _handler = WebReadiness.handleReadinessRequest
      fail "GET /ready 404 when disabled: not implemented — stub must fail"

  describe "HTTP GET /queries/{name}" do
    it "returns 200 OK with query results when the query is Ready" \_ -> do
      let _handler = WebReadiness.handleQueryReadinessRequest
      fail "GET /queries/{name} 200: not implemented — stub must fail"

    it "returns 503 Service Unavailable with X-Query-Status: rebuilding when query is rebuilding" \_ -> do
      let _handler = WebReadiness.handleQueryReadinessRequest
      fail "GET /queries/{name} 503 rebuilding header: not implemented — stub must fail"

    it "includes lag in the 503 rebuilding response" \_ -> do
      let _handler = WebReadiness.handleQueryReadinessRequest
      fail "GET /queries/{name} lag in 503: not implemented — stub must fail"

    it "returns 503 Service Unavailable with X-Query-Status: failed when query has failed" \_ -> do
      let _handler = WebReadiness.handleQueryReadinessRequest
      fail "GET /queries/{name} 503 failed header: not implemented — stub must fail"

    it "returns 404 Not Found when the query is not registered" \_ -> do
      let _handler = WebReadiness.handleQueryReadinessRequest
      fail "GET /queries/{name} 404 unregistered: not implemented — stub must fail"

    it "does not cache the readiness state (fresh read on every request)" \_ -> do
      let _handler = WebReadiness.handleQueryReadinessRequest
      fail "GET /queries/{name} no-cache H4: not implemented — stub must fail"

  describe "ReadinessConfig" do
    it "ReadinessConfig round-trips correctly" \_ -> do
      -- toJSON >> fromJSON should return the same ReadinessConfig value.
      fail "ReadinessConfig round-trip: not implemented — stub must fail"
