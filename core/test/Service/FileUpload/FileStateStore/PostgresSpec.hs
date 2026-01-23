module Service.FileUpload.FileStateStore.PostgresSpec (spec) where

import Core
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Session qualified as Session
import Result qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
import Task qualified
import Test
import Test.Service.FileUpload.FileStateStore qualified as FileStateStoreSpec


spec :: Spec Unit
spec = do
  describe "PostgresFileStateStore" do
    let config =
          PostgresEventStore
            { host = "localhost"
            , databaseName = "neohaskell"
            , user = "neohaskell"
            , password = "neohaskell"
            , port = 5432
            }
    let newStore = do
          -- Drop and recreate table for clean slate
          dropTable config
          PostgresFileStore.new config
    FileStateStoreSpec.spec newStore


-- | Drop the file_upload_state table for test isolation
dropTable :: PostgresEventStore -> Task Text ()
dropTable config = do
  pool <- createPoolForCleanup config
  runDropTable pool
    |> Task.mapError (\err -> [fmt|Failed to drop table: #{show err}|])


-- | Create a connection pool for cleanup operations
createPoolForCleanup :: PostgresEventStore -> Task Text HasqlPool.Pool
createPoolForCleanup cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host
          , Param.port (fromIntegral cfg.port)
          , Param.dbname cfg.databaseName
          , Param.user cfg.user
          , Param.password cfg.password
          ]
  let settings = [params |> ConnectionSetting.connection]
  let poolConfig = [HasqlPoolConfig.staticConnectionSettings settings] |> HasqlPoolConfig.settings
  HasqlPool.acquire poolConfig
    |> Task.fromIO


-- | Run DROP TABLE statement
runDropTable :: HasqlPool.Pool -> Task Text ()
runDropTable pool = do
  let session = Session.sql "DROP TABLE IF EXISTS file_upload_state CASCADE"
  result <- HasqlPool.use pool session
    |> Task.fromIO
    |> Task.map Result.fromEither
  case result of
    Err err -> Task.throw [fmt|Pool error: #{show err}|]
    Ok _ -> Task.yield ()
