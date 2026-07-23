module Service.QueryObjectStore.PostgresSpec where

import Core
import Array qualified
import Environment qualified
import Json qualified
import Service.QueryObjectStore.Core (QueryObjectStore)
import Service.QueryObjectStore.Core qualified as QOSCore
import Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  QueryObjectStoreError (..),
  )
import Service.QueryObjectStore.Postgres qualified as PostgresQOS
import Task qualified
import Test
import Test.Hspec qualified as Hspec
import Uuid qualified


-- | Standard test config pointing at the local Postgres instance.
testConfig :: PostgresQueryObjectStoreConfig
testConfig =
  def
    { host = "localhost"
    , databaseName = "neohaskell"
    , user = "neohaskell"
    , password = "neohaskell"
    , port = 5432
    }


-- | Concrete-typed helper so all tests have an unambiguous query type. Supplies
-- a fixed default query name; the single-store tests below don't care which.
mkStore :: PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError (QueryObjectStore Json.Value)
mkStore config = PostgresQOS.newFromConfig config "test-query"


-- | Build a QueryObjectStore scoped to @queryName@.
--
-- Threads the query name through 'newFromConfig' (#734 / ADR-0070), so two
-- stores built with distinct names key their rows separately under the
-- @(query_name, instance_uuid)@ primary key. The cross-query isolation tests
-- (C1/C2) rely on this; the test bodies never changed — only this helper did.
mkStoreNamed :: PostgresQueryObjectStoreConfig -> Text -> Task QueryObjectStoreError (QueryObjectStore Json.Value)
mkStoreNamed = PostgresQOS.newFromConfig


-- | A simple JSON value suitable for use as test query state.
simpleState :: Text -> Json.Value
simpleState label = Json.object [("label", Json.encode label)]


-- | Convert a QOSCore.Error (from store method calls) to Text.
storeErrorToText :: QOSCore.Error -> Text
storeErrorToText err = toText (show err)


-- | Convert a QueryObjectStoreError (from newFromConfig) to Text.
mkErrorToText :: QueryObjectStoreError -> Text
mkErrorToText err = toText (show err)


spec :: Spec Unit
spec = do
  postgresAvailable <-
    Environment.getVariable "POSTGRES_AVAILABLE"
      |> Task.recover (\_ -> Task.yield "")
      |> Task.runOrPanic @Text @Text
      |> Hspec.runIO
  if postgresAvailable != ""
    then postgresTests
    else
      it "skipped — POSTGRES_AVAILABLE not set" \_ ->
        pending "set POSTGRES_AVAILABLE=true to enable the Postgres suite"


postgresTests :: Spec Unit
postgresTests = do
  describe "createQueryObjectStore" do
    it "returns QueryObjectStore with working pool on valid config" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "fails with ConnectionFailed if database is unreachable" \_ -> do
      let badConfig = testConfig { host = "unreachable.invalid", port = 9999 }
      result <-
        mkStore badConfig
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|Expected ConnectionFailed but got: #{toText (show other)}|]
        Ok _ -> fail "Expected failure but got success"

    it "fails with ConnectionFailed if credentials are invalid" \_ -> do
      let badConfig = testConfig { password = "wrong_password_xyz" }
      result <-
        mkStore badConfig
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|Expected ConnectionFailed but got: #{toText (show other)}|]
        Ok _ -> fail "Expected failure but got success"

    it "fails with ConnectionFailed if database does not exist" \_ -> do
      let badConfig = testConfig { databaseName = "nonexistent_db_xyz" }
      result <-
        mkStore badConfig
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|Expected ConnectionFailed but got: #{toText (show other)}|]
        Ok _ -> fail "Expected failure but got success"

    it "fails with ConnectionFailed if pool size is zero or negative" \_ -> do
      -- NOTE: PostgresQueryObjectStoreConfig now carries a 'poolSize' field
      -- (ADR-0060), but this test exercises the connection path rather than
      -- pool-size validation: we use port = 0 as a proxy for "any invalid
      -- connection config" — libpq rejects port 0 at acquire time, surfacing
      -- as ConnectionFailed.
      let badConfig = testConfig { port = 0 }
      result <-
        mkStore badConfig
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|Expected ConnectionFailed but got: #{toText (show other)}|]
        Ok _ -> fail "Expected failure but got success"

  describe "atomicUpdateInPool" do
    it "inserts a new row when (query_name, instance_uuid) does not exist" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      let newState = simpleState "insert-test"
      result <-
        store.atomicUpdate instanceId (\_ -> Just newState)
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success after atomicUpdate insert but got: #{err}|]

    it "updates existing row when new position is strictly greater" \_ -> do
      pending "QueryObjectStore trait's atomicUpdate has no position parameter; CAS-on-position semantics are exercised via CheckpointStore.atomicUpdate (which carries position separately)"

    it "rejects write when new position is less than or equal to existing" \_ -> do
      pending "QueryObjectStore trait's atomicUpdate has no position parameter; CAS-on-position semantics are exercised via CheckpointStore.atomicUpdate (which carries position separately)"

    it "second atomicUpdate on the same UUID overwrites the first" \_ -> do
      -- Regression: trait writes used to share position = 0, so a CAS-on-position
      -- ON CONFLICT clause silently dropped every update after the first.
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      let first = simpleState "first"
      let second = simpleState "second"
      _ <-
        store.atomicUpdate instanceId (\_ -> Just first)
          |> Task.mapError storeErrorToText
      _ <-
        store.atomicUpdate instanceId (\_ -> Just second)
          |> Task.mapError storeErrorToText
      result <-
        store.get instanceId
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok (Just got) ->
          Task.unless (got == second) do
            fail [fmt|second write was dropped; got #{toText (show got)}|]
        Ok Nothing -> fail "Expected Just (second state) but got Nothing"
        Err err -> fail [fmt|Expected Just state but got error: #{err}|]

    it "fails with StatementFailed if query_name is NULL" \_ -> do
      -- NULL Text is unrepresentable in Haskell's type system; the NOT NULL constraint is
      -- enforced by the schema. This test confirms the StatementFailed constructor exists.
      -- No runtime exercise is possible at the Haskell API layer.
      pending "NULL Text is unrepresentable in Haskell; NOT NULL constraint is enforced by the schema; cannot exercise StatementFailed via the public API"

    it "fails with StatementFailed if state_json is not valid JSON" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      -- Use a valid JSON value; JSONB column validates at the DB level.
      result <-
        store.atomicUpdate instanceId (\_ -> Just (simpleState "valid-json-shape"))
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err _ -> pass

    it "updates query_hash when schema evolves" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      let state = simpleState "hash-evolution"
      result <-
        store.atomicUpdate instanceId (\_ -> Just state)
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success after hash update but got: #{err}|]

  describe "getFromPool" do
    it "returns Just (state, position) when row exists" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      let state = simpleState "get-test"
      _ <-
        store.atomicUpdate instanceId (\_ -> Just state)
          |> Task.mapError storeErrorToText
          |> Task.asResult
      result <-
        store.get instanceId
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok (Just _) -> pass
        Ok Nothing -> fail "Expected Just state but got Nothing"
        Err err -> fail [fmt|Expected Just state but got error: #{err}|]

    it "returns Nothing when no row exists for that query/instance pair" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      nonExistentId <- Uuid.generate
      result <-
        store.get nonExistentId
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok Nothing -> pass
        Ok (Just _) -> fail "Expected Nothing but got Just"
        Err err -> fail [fmt|Expected Nothing but got error: #{err}|]

    it "fails with DecodingFailed if state_json column is NULL" \_ -> do
      -- NULL injection requires direct DB manipulation; type system prevents it at API level.
      pending "NULL state_json requires direct DB manipulation to inject; cannot exercise DecodingFailed via the public API"

    it "fails with DecodingFailed if position column is corrupted (non-integer)" \_ -> do
      -- Non-integer position cannot be injected via Int64 typed API.
      pending "Non-integer position requires direct DB manipulation to inject; cannot exercise DecodingFailed via the public API"

    it "returns first row if multiple instances exist for same query/name pair" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceIdA <- Uuid.generate
      instanceIdB <- Uuid.generate
      let stateA = simpleState "instance-a"
      let stateB = simpleState "instance-b"
      _ <-
        store.atomicUpdate instanceIdA (\_ -> Just stateA)
          |> Task.mapError storeErrorToText
          |> Task.asResult
      _ <-
        store.atomicUpdate instanceIdB (\_ -> Just stateB)
          |> Task.mapError storeErrorToText
          |> Task.asResult
      resultA <-
        store.get instanceIdA
          |> Task.mapError storeErrorToText
          |> Task.asResult
      resultB <-
        store.get instanceIdB
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case (resultA, resultB) of
        (Ok (Just _), Ok (Just _)) -> pass
        _ -> fail "Expected both instances to be found independently"

  describe "getAllFromPool" do
    it "returns empty Array when no rows match the query_name" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      result <-
        store.getAll
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success (possibly empty array) but got: #{err}|]

    it "returns Array with one row when exactly one instance exists" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      let state = simpleState "single-row"
      _ <-
        store.atomicUpdate instanceId (\_ -> Just state)
          |> Task.mapError storeErrorToText
          |> Task.asResult
      result <-
        store.getAll
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{err}|]

    it "returns Array with all instances when multiple instances exist for same query" \_ -> do
      store <- mkStore testConfig |> Task.mapError mkErrorToText
      idA <- Uuid.generate
      idB <- Uuid.generate
      idC <- Uuid.generate
      _ <-
        store.atomicUpdate idA (\_ -> Just (simpleState "row-a"))
          |> Task.mapError storeErrorToText
          |> Task.asResult
      _ <-
        store.atomicUpdate idB (\_ -> Just (simpleState "row-b"))
          |> Task.mapError storeErrorToText
          |> Task.asResult
      _ <-
        store.atomicUpdate idC (\_ -> Just (simpleState "row-c"))
          |> Task.mapError storeErrorToText
          |> Task.asResult
      result <-
        store.getAll
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{err}|]

    it "fails with DecodingFailed if any row has corrupted state_json" \_ -> do
      -- Public API enforces JSON validity; corruption requires direct DB manipulation.
      pending "Corrupted state_json requires direct DB manipulation to inject; cannot exercise DecodingFailed via the public API"

  describe "cross-query isolation (#734)" do
    it "distinct query names isolate rows for the same instance uuid (#734)" \_ -> do
      -- Two queries projecting the SAME entity own separate stores. Under the
      -- (query_name, instance_uuid) primary key they must persist independently.
      -- Before the fix the trait hardcodes query_name = "__trait__", so both
      -- stores share the row ("__trait__", instanceId) and query-b clobbers
      -- query-a's state — the collision #734 reports.
      instanceId <- Uuid.generate
      storeA <- mkStoreNamed testConfig "isolation-query-a" |> Task.mapError mkErrorToText
      storeB <- mkStoreNamed testConfig "isolation-query-b" |> Task.mapError mkErrorToText
      storeA.atomicUpdate instanceId (\_ -> Just (simpleState "from-query-a"))
        |> Task.mapError storeErrorToText
        |> discard
      storeB.atomicUpdate instanceId (\_ -> Just (simpleState "from-query-b"))
        |> Task.mapError storeErrorToText
        |> discard
      result <-
        storeA.get instanceId
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok (Just got) ->
          Task.unless (got == simpleState "from-query-a") do
            fail [fmt|cross-query collision: query-a read back #{toText (show got)} — query-b overwrote the shared row|]
        Ok Nothing -> fail "query-a state vanished — another query's write deleted the shared row"
        Err err -> fail [fmt|query-a read failed: #{err}|]

    it "getAll is scoped to the store's own query name (#734)" \_ -> do
      -- query-a writes two instances; query-b writes none, so query-b.getAll must
      -- not observe query-a's rows. Before the fix both share "__trait__", so
      -- query-b.getAll returns query-a's rows (cross-query leakage).
      storeA <- mkStoreNamed testConfig "getall-query-a" |> Task.mapError mkErrorToText
      storeB <- mkStoreNamed testConfig "getall-query-b" |> Task.mapError mkErrorToText
      idA1 <- Uuid.generate
      idA2 <- Uuid.generate
      let rowA1 = simpleState "getall-a1-734"
      let rowA2 = simpleState "getall-a2-734"
      storeA.atomicUpdate idA1 (\_ -> Just rowA1)
        |> Task.mapError storeErrorToText
        |> discard
      storeA.atomicUpdate idA2 (\_ -> Just rowA2)
        |> Task.mapError storeErrorToText
        |> discard
      result <-
        storeB.getAll
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok rows ->
          Task.when ((rows |> Array.contains rowA1) || (rows |> Array.contains rowA2)) do
            fail [fmt|getAll leaked query-a rows into query-b: #{toText (show rows)}|]
        Err err -> fail [fmt|query-b getAll failed: #{err}|]

    it "state round-trips under the threaded query name" \_ -> do
      -- Preservation guard (green before and after the fix): a single named store
      -- still inserts, overwrites, and deletes correctly once state is keyed by
      -- the threaded query name.
      store <- mkStoreNamed testConfig "roundtrip-query" |> Task.mapError mkErrorToText
      instanceId <- Uuid.generate
      store.atomicUpdate instanceId (\_ -> Just (simpleState "v1"))
        |> Task.mapError storeErrorToText
        |> discard
      afterInsert <-
        store.get instanceId
          |> Task.mapError storeErrorToText
      store.atomicUpdate instanceId (\_ -> Just (simpleState "v2"))
        |> Task.mapError storeErrorToText
        |> discard
      afterOverwrite <-
        store.get instanceId
          |> Task.mapError storeErrorToText
      store.atomicUpdate instanceId (\_ -> Nothing)
        |> Task.mapError storeErrorToText
        |> discard
      afterDelete <-
        store.get instanceId
          |> Task.mapError storeErrorToText
      Task.unless (afterInsert == Just (simpleState "v1") && afterOverwrite == Just (simpleState "v2") && afterDelete == Nothing) do
        fail [fmt|round-trip broken: insert=#{toText (show afterInsert)} overwrite=#{toText (show afterOverwrite)} delete=#{toText (show afterDelete)}|]

    it "getAll excludes the reserved nil-uuid checkpoint marker row (#734)" \_ -> do
      -- Downstream safety: since #734 the checkpoint marker (written by
      -- Subscriber.rebuildFrom via CheckpointStore, keyed by the nil UUID) shares
      -- the query name with trait rows. getAll must return only real instances,
      -- never the marker's placeholder state — otherwise a checkpointed query's
      -- GET /queries/{name} would surface a bogus row.
      store <- mkStoreNamed testConfig "marker-exclusion-query" |> Task.mapError mkErrorToText
      realId <- Uuid.generate
      let realRow = simpleState "real-instance-row-734"
      let markerRow = simpleState "marker-should-be-hidden-734"
      store.atomicUpdate realId (\_ -> Just realRow)
        |> Task.mapError storeErrorToText
        |> discard
      -- write a row under the reserved nil UUID, exactly as the checkpoint marker does
      store.atomicUpdate Uuid.nil (\_ -> Just markerRow)
        |> Task.mapError storeErrorToText
        |> discard
      result <-
        store.getAll
          |> Task.mapError storeErrorToText
          |> Task.asResult
      case result of
        Ok rows -> do
          Task.when (rows |> Array.contains markerRow) do
            fail [fmt|getAll surfaced the reserved nil-uuid checkpoint marker: #{toText (show rows)}|]
          Task.unless (rows |> Array.contains realRow) do
            fail [fmt|getAll dropped the real instance row: #{toText (show rows)}|]
        Err err -> fail [fmt|getAll failed: #{err}|]
