module Service.Application.ReadinessBuilderSpec where

import Core
import Service.Application qualified as Application
import Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  )
import Test


-- | Minimal Postgres config for builder tests.
testPostgresConfig :: PostgresQueryObjectStoreConfig
testPostgresConfig =
  PostgresQueryObjectStoreConfig
    { host = "localhost"
    , databaseName = "neohaskell"
    , user = "neohaskell"
    , password = "neohaskell"
    , port = 5432
    }


spec :: Spec Unit
spec = do
  describe "useQueryObjectStore" do
    it "wires the QueryObjectStore into the subscriber and returns updated Application" \_ -> do
      let _app =
            Application.new
              |> Application.useQueryObjectStore testPostgresConfig
      -- Stub: useQueryObjectStore throws error "not implemented", so this panics.
      -- The test must fail, not pass.
      fail "useQueryObjectStore wiring: not implemented — stub must fail"

    it "allows chaining with other builder methods (withQuery, withEventStore, etc.)" \_ -> do
      -- Builder chain:
      --   Application.new |> useQueryObjectStore config |> ...
      -- Stub panics before the chain completes.
      fail "useQueryObjectStore chaining: not implemented — stub must fail"

    it "fails at runtime if the QueryObjectStore config is invalid" \_ -> do
      let badConfig = testPostgresConfig { host = "unreachable.invalid" }
      let _app =
            Application.new
              |> Application.useQueryObjectStore badConfig
      -- Stub: not implemented; must fail.
      fail "useQueryObjectStore invalid config: not implemented — stub must fail"

    it "accepts multiple store backends if they implement QueryObjectStoreConfig" \_ -> do
      -- Both InMemory (via existing withQueryObjectStore) and Postgres backends
      -- must compile and be accepted by the builder.
      fail "useQueryObjectStore multiple backends: not implemented — stub must fail"

  describe "useReadinessEndpoint" do
    it "enables the /ready HTTP endpoint and returns updated Application" \_ -> do
      let _app =
            Application.new
              |> Application.useReadinessEndpoint
      fail "useReadinessEndpoint enables /ready: not implemented — stub must fail"

    it "is enabled by default (omitting the call still activates /ready)" \_ -> do
      -- Application.new without explicit useReadinessEndpoint should still have /ready.
      fail "useReadinessEndpoint default on: not implemented — stub must fail"

    it "allows chaining with Application.withoutReadinessEndpoint to disable /ready" \_ -> do
      let _app =
            Application.new
              |> Application.useReadinessEndpoint
              |> Application.withoutReadinessEndpoint
      fail "useReadinessEndpoint then withoutReadinessEndpoint: not implemented — stub must fail"

    it "returns Application suitable for Application.run" \_ -> do
      let _app =
            Application.new
              |> Application.useReadinessEndpoint
      -- Would call Application.run but that requires full infrastructure;
      -- stub must fail before we get there.
      fail "useReadinessEndpoint runnable: not implemented — stub must fail"
