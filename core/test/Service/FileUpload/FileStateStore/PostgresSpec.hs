module Service.FileUpload.FileStateStore.PostgresSpec (spec) where

import Core
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import Result qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
import Service.FileUpload.Web (FileStateStore (..))
import Task qualified
import Test
import Test.Service.FileUpload.FileStateStore qualified as FileStateStoreSpec


-- | Test configuration - same as other Postgres tests in the codebase
testConfig :: PostgresEventStore
testConfig =
  PostgresEventStore
    { host = "localhost"
    , databaseName = "neohaskell"
    , user = "neohaskell"
    , password = "neohaskell"
    , port = 5432
    }


spec :: Spec Unit
spec = do
  describe "PostgresFileStateStore" do
    -- Create store once per test. This follows the same pattern as
    -- Service.EventStore.PostgresSpec which also creates new stores per test.
    -- 
    -- Note: newWithCleanup creates a pool internally. The pool is explicitly
    -- released after use to prevent connection leaks in tests.
    let newStore = do
          let withFreshStore operation = do
                (store, pool) <- PostgresFileStore.newWithCleanup testConfig
                result <- operation store |> Task.asResult
                HasqlPool.release pool |> Task.fromIO
                case result of
                  Err err -> Task.throw err
                  Ok value -> Task.yield value
          -- Reset table for test isolation using a short-lived pool
          (_, setupPool) <- PostgresFileStore.newWithCleanup testConfig
          dropTable setupPool
          HasqlPool.release setupPool |> Task.fromIO
          let getState fileRef =
                withFreshStore \store ->
                  store.getState fileRef
          let setState fileRef state =
                withFreshStore \store ->
                  store.setState fileRef state
          let updateState fileRef event =
                withFreshStore \store ->
                  store.updateState fileRef event
          Task.yield FileStateStore {getState, setState, updateState}
    FileStateStoreSpec.spec newStore


-- | Drop table for test isolation
dropTable :: Pool -> Task Text ()
dropTable pool = do
  let session = Session.sql "DROP TABLE IF EXISTS file_upload_state CASCADE"
  result <- HasqlPool.use pool session
    |> Task.fromIO
    |> Task.map Result.fromEither
  case result of
    Err err -> Task.throw [fmt|Failed to drop table: #{show err}|]
    Ok _ -> Task.yield ()
