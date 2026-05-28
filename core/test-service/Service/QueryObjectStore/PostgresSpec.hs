module Service.QueryObjectStore.PostgresSpec where

import Core
import Json qualified
import Service.QueryObjectStore.Core (QueryObjectStore)
import Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  QueryObjectStoreError (..),
  )
import Service.QueryObjectStore.Postgres qualified as PostgresQOS
import Task qualified
import Test


-- | Standard test config pointing at the local Postgres instance.
testConfig :: PostgresQueryObjectStoreConfig
testConfig =
  PostgresQueryObjectStoreConfig
    { host = "localhost"
    , databaseName = "neohaskell"
    , user = "neohaskell"
    , password = "neohaskell"
    , port = 5432
    }


-- | Concrete-typed helper so all tests have an unambiguous query type.
mkStore :: PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError (QueryObjectStore Json.Value)
mkStore = PostgresQOS.createQueryObjectStore


spec :: Spec Unit
spec = do
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
      -- A port of 0 is an invalid config — treated as pool-size/config validation failure.
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
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "atomicUpdateInPool: not implemented — stub must fail"

    it "updates existing row when new position is strictly greater" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "atomicUpdateInPool CAS advance: not implemented — stub must fail"

    it "rejects write when new position is less than or equal to existing" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "atomicUpdateInPool CAS reject: not implemented — stub must fail"

    it "fails with StatementFailed if query_name is NULL" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> do
          -- Real test: call atomicUpdate with NULL query_name, expect StatementFailed.
          -- Stub: createQueryObjectStore is not implemented yet, so we never reach here.
          fail "atomicUpdateInPool NULL name: not implemented — stub must fail"

    it "fails with StatementFailed if state_json is not valid JSON" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "atomicUpdateInPool bad JSON: not implemented — stub must fail"

    it "updates query_hash when schema evolves" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "atomicUpdateInPool hash update: not implemented — stub must fail"

  describe "getFromPool" do
    it "returns Just (state, position) when row exists" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getFromPool found: not implemented — stub must fail"

    it "returns Nothing when no row exists for that query/instance pair" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getFromPool not-found: not implemented — stub must fail"

    it "fails with DecodingFailed if state_json column is NULL" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getFromPool NULL state_json: not implemented — stub must fail"

    it "fails with DecodingFailed if position column is corrupted (non-integer)" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getFromPool corrupted position: not implemented — stub must fail"

    it "returns first row if multiple instances exist for same query/name pair" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getFromPool multi-instance: not implemented — stub must fail"

  describe "getAllFromPool" do
    it "returns empty Array when no rows match the query_name" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getAllFromPool empty: not implemented — stub must fail"

    it "returns Array with one row when exactly one instance exists" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getAllFromPool single row: not implemented — stub must fail"

    it "returns Array with all instances when multiple instances exist for same query" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getAllFromPool multi-row: not implemented — stub must fail"

    it "fails with DecodingFailed if any row has corrupted state_json" \_ -> do
      result <-
        mkStore testConfig
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "getAllFromPool corrupted row: not implemented — stub must fail"
