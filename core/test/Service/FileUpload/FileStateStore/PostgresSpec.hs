module Service.FileUpload.FileStateStore.PostgresSpec (spec) where

import Core
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import Result qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
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
    -- Note: newWithCleanup creates a pool internally. The pool is released
    -- when garbage collected after each test. This is acceptable for tests
    -- (not production) and matches the EventStore test patterns.
    let newStore = do
          -- Use newWithCleanup to get the pool, then drop and recreate table
          (_, pool) <- PostgresFileStore.newWithCleanup testConfig
          dropTable pool
          -- Create fresh store with new table
          PostgresFileStore.new testConfig
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
