module Service.FileUpload.FileStateStore.PostgresSpec (spec) where

import Core
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import LinkedList qualified
import Maybe qualified
import Result qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (FileStateStoreBackend (..))
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
import Service.FileUpload.Web (FileStateStore (..))
import Service.Infra.Postgres.ConnectionConfig (ConnectionParams (..), SslMode (..))
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Task qualified
import Test
import Test.Service.FileUpload.FileStateStore qualified as FileStateStoreSpec


-- | Test configuration - same as other Postgres tests in the codebase
-- | Full record construction (not a @def { .. }@ update): this module imports
-- both PostgresEventStore and ConnectionParams field sets, so a bare-field
-- record UPDATE would be ambiguous under -XDuplicateRecordFields. Constructing
-- via the named constructor disambiguates. Values mirror the old def-based
-- fixture (poolSize/sslMode/sslRootCert at their PostgresEventStore defaults).
testConfig :: PostgresEventStore
testConfig =
  PostgresEventStore
    { host = "localhost"
    , databaseName = "neohaskell"
    , user = "neohaskell"
    , password = "neohaskell"
    , port = 5432
    , poolSize = 6
    , sslMode = SslModeUnset
    , sslRootCert = Nothing
    }


-- WI-5 (#684) regression: the FileUpload state-store pool must run on the same
-- TLS posture as the EventStore pool. 'toEventStoreConfig' is the production
-- mapping that 'makeFileUploadConfig' / Application.run go through, so asserting
-- the operator's pgSslMode/pgSslRootCert reach the inspectable ResolvedParams
-- guards against a silent FileUpload TLS downgrade (ADR-0064). Pure: no DB.
configFor :: SslMode -> Maybe Text -> PostgresEventStore
configFor mode rootCert =
  PostgresStateStore
    { pgHost = "localhost"
    , pgPort = 5432
    , pgDatabase = "neohaskell"
    , pgUser = "neohaskell"
    , pgPassword = "neohaskell"
    , pgSslMode = mode
    , pgSslRootCert = rootCert
    }
    |> PostgresFileStore.toEventStoreConfig
    |> Maybe.withDefault def


paramsFor :: PostgresEventStore -> ConnectionParams
paramsFor cfg =
  ConnectionParams
    { host = cfg.host
    , databaseName = cfg.databaseName
    , user = cfg.user
    , password = cfg.password
    , port = cfg.port
    , sslMode = cfg.sslMode
    , sslRootCert = cfg.sslRootCert
    }


sslThreadingSpec :: Spec Unit
sslThreadingSpec =
  describe "toEventStoreConfig (WI-5 #684 sslMode threading regression)" do
    it "threads pgSslMode=Require into the resolved sslMode (FileUpload honours DB_SSL_MODE)" \_ -> do
      let cfg = configFor SslModeRequire Nothing
      cfg.sslMode |> shouldBe SslModeRequire
      paramsFor cfg
        |> ConnectionConfig.resolveParams
        |> Result.map (\r -> r.sslMode)
        |> shouldBe (Ok SslModeRequire)

    it "threads pgSslMode=VerifyFull plus the root-cert path through unchanged" \_ -> do
      let cfg = configFor SslModeVerifyFull (Just "/etc/postgresql/azure-roots.pem")
      cfg.sslMode |> shouldBe SslModeVerifyFull
      paramsFor cfg
        |> ConnectionConfig.resolveParams
        |> Result.map (\r -> r.sslRootCert)
        |> shouldBe (Ok (Just "/etc/postgresql/azure-roots.pem"))

    it "emits an sslmode param for Require (one entry past the unset baseline)" \_ -> do
      paramsFor (configFor SslModeRequire Nothing)
        |> ConnectionConfig.toConnectionParams
        |> Result.map LinkedList.length
        |> shouldBe (Ok 2)

    it "emits NO sslmode param for SslModeUnset (default-off, no regression)" \_ -> do
      paramsFor (configFor SslModeUnset Nothing)
        |> ConnectionConfig.toConnectionParams
        |> Result.map LinkedList.length
        |> shouldBe (Ok 1)


spec :: Spec Unit
spec = do
  sslThreadingSpec
  describe "PostgresFileStateStore" do
    -- Create store once per test. This follows the same pattern as
    -- Service.EventStore.PostgresSpec which also creates new stores per test.
    -- 
    -- Note: newWithCleanup creates a pool internally. The pool is explicitly
    -- released after use to prevent connection leaks in tests.
    let newStore = do
          let withFreshStore operation = do
                (store, pool) <- PostgresFileStore.newWithCleanup testConfig
                Task.finally
                  (HasqlPool.release pool |> Task.fromIO)
                  (operation store)
          -- Reset table for test isolation using a short-lived pool
          (_, setupPool) <- PostgresFileStore.newWithCleanup testConfig
          Task.finally
            (HasqlPool.release setupPool |> Task.fromIO)
            (dropTable setupPool)
          let getState fileRef =
                withFreshStore \store ->
                  store.getState fileRef
          let setState fileRef state =
                withFreshStore \store ->
                  store.setState fileRef state
          let updateState fileRef event =
                withFreshStore \store ->
                  store.updateState fileRef event
          let findByContentHash ownerHashVal contentHashVal =
                withFreshStore \store ->
                  store.findByContentHash ownerHashVal contentHashVal
          Task.yield FileStateStore {getState, setState, updateState, findByContentHash}
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
