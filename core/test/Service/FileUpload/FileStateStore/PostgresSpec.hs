module Service.FileUpload.FileStateStore.PostgresSpec (spec) where

import Core
import AsyncTask qualified
import Environment qualified
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import LinkedList qualified
import Maybe qualified
import Result qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (
  BlobKey (..),
  ContentHash (..),
  FileCreation (..),
  FileDeletedData (..),
  FileDeletionReason (..),
  FileRef (..),
  FileStateStoreBackend (..),
  FileUploadedData (..),
  FileUploadEvent (..),
  OwnerHash (..),
 )
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Service.FileUpload.Web (FileStateStore (..))
import Service.Infra.Postgres.ConnectionConfig (ConnectionParams (..), SslMode (..))
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Task qualified
import Test
import Test.Service.FileUpload.FileStateStore qualified as FileStateStoreSpec
import Text qualified


-- | Test configuration - same as other Postgres tests in the codebase
-- | Full record construction (not a @def { .. }@ update): this module imports
-- both PostgresEventStore and ConnectionParams field sets, so a bare-field
-- record UPDATE would be ambiguous under -XDuplicateRecordFields. Constructing
-- via the named constructor disambiguates. Values mirror the old def-based
-- fixture (poolSize/sslMode/sslRootCert at their PostgresEventStore defaults).
testConfig :: Text -> PostgresEventStore
testConfig databaseName =
  PostgresEventStore
    { host = "localhost"
    , databaseName = databaseName
    , user = "neohaskell"
    , password = "neohaskell"
    , port = 5432
    , poolSize = 6
    , sslMode = SslModeUnset
    , sslRootCert = Nothing
    , erasableApplicationScope = ""
    }


withFreshStore :: PostgresEventStore -> (FileStateStore -> Task Text result) -> Task Text result
withFreshStore config operation = do
  (store, pool) <- PostgresFileStore.newWithCleanup config
  Task.finally
    (HasqlPool.release pool |> Task.fromIO)
    (operation store)


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


privateDatabase :: Task Text Text
privateDatabase = do
  databaseName <- Environment.getVariable "FILE_UPLOAD_TEST_DB"
  if Text.isEmpty databaseName || databaseName == "neohaskell"
    then Task.throw "FILE_UPLOAD_TEST_DB must name a dedicated PostgreSQL test database"
    else Task.yield databaseName


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

    it "threads sslmode=require into the single connection setting for Require (#694: ONE setting, never 2)" \_ -> do
      -- #694: the TLS params must ride INSIDE the one connection setting, not a
      -- second one (a 2nd 'ConnectionSetting.connection' makes hasql drop the
      -- host via last-wins). So the setting count is 1 for Require, same as the
      -- unset baseline -- the sslmode now lives in the shared param list.
      paramsFor (configFor SslModeRequire Nothing)
        |> ConnectionConfig.toConnectionParams
        |> Result.map LinkedList.length
        |> shouldBe (Ok 1)

    it "carries the sslmode=require param through FileUpload's threading (asserted on the inspectable pairs)" \_ -> do
      paramsFor (configFor SslModeRequire Nothing)
        |> ConnectionConfig.toParamPairs
        |> Result.map (LinkedList.any (\(k, v) -> k == "sslmode" && v == "require"))
        |> shouldBe (Ok True)

    it "threads verify-full + sslrootcert + channel_binding through FileUpload's config bridge (hardened path keeps the CA bundle)" \_ -> do
      -- Guards 'toEventStoreConfig' against silently dropping pgSslRootCert: the
      -- full hardened posture (sslmode + sslrootcert + channel_binding) must
      -- survive the FileUpload mapping, not just the sslmode knob.
      paramsFor (configFor SslModeVerifyFull (Just "/etc/ca.pem"))
        |> ConnectionConfig.toParamPairs
        |> Result.map
          ( \pairs ->
              LinkedList.any (\(k, v) -> k == "sslmode" && v == "verify-full") pairs
                && LinkedList.any (\(k, v) -> k == "sslrootcert" && v == "/etc/ca.pem") pairs
                && LinkedList.any (\(k, v) -> k == "channel_binding" && v == "require") pairs
          )
        |> shouldBe (Ok True)

    it "emits NO sslmode param for SslModeUnset (default-off, no regression)" \_ -> do
      paramsFor (configFor SslModeUnset Nothing)
        |> ConnectionConfig.toParamPairs
        |> Result.map (LinkedList.any (\(k, _) -> k == "sslmode"))
        |> shouldBe (Ok False)


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
          databaseName <- privateDatabase
          let config = testConfig databaseName
          -- Reset table for test isolation using a short-lived pool
          (_, setupPool) <- PostgresFileStore.newWithCleanup config
          Task.finally
            (HasqlPool.release setupPool |> Task.fromIO)
            (dropTable setupPool)
          let getState :: FileRef -> Task Text (Maybe FileUploadState)
              getState fileRef =
                withFreshStore config \store ->
                  store.getState fileRef
              setState :: FileRef -> FileUploadState -> Task Text ()
              setState fileRef state =
                withFreshStore config \store ->
                  store.setState fileRef state
              updateState :: FileRef -> FileUploadEvent -> Task Text ()
              updateState fileRef event =
                withFreshStore config \store ->
                  store.updateState fileRef event
              findByContentHash :: OwnerHash -> ContentHash -> Task Text (Maybe FileRef)
              findByContentHash ownerHashVal contentHashVal =
                withFreshStore config \store ->
                  store.findByContentHash ownerHashVal contentHashVal
              reserveCreation :: FileCreation -> Task Text FileCreation
              reserveCreation creation =
                withFreshStore config \store ->
                  store.reserveCreation creation
              publishRetained :: FileCreation -> Task Text ()
              publishRetained creation =
                withFreshStore config \store ->
                  store.publishRetained creation
          Task.yield FileStateStore {getState, setState, updateState, findByContentHash, reserveCreation, publishRetained}
    FileStateStoreSpec.spec newStore
    it "rolls back a rejected update before reusing the same one-connection pool" \_ -> do
      databaseName <- privateDatabase
      let config = (testConfig databaseName) {poolSize = 1}
      (_, setupPool) <- PostgresFileStore.newWithCleanup config
      Task.finally
        (HasqlPool.release setupPool |> Task.fromIO)
        (dropTable setupPool)
      (store, pool) <- PostgresFileStore.newWithCleanup config
      Task.finally
        (HasqlPool.release pool |> Task.fromIO)
        do
          let owner = OwnerHash "transaction-cleanup-owner"
          let duplicateHash = ContentHash "transaction-cleanup-duplicate"
          let firstRef = FileRef "transaction-cleanup-first"
          let rejectedRef = FileRef "transaction-cleanup-rejected"
          let laterRef = FileRef "transaction-cleanup-later"
          let uploaded fileRef contentHash = FileUploaded FileUploadedData
                { fileRef = fileRef
                , ownerHash = owner
                , contentHash = contentHash
                , filename = "transaction-cleanup.txt"
                , contentType = "text/plain"
                , sizeBytes = 36
                , blobKey = BlobKey "transaction-cleanup-blob"
                , expiresAt = 2000000000
                , uploadedAt = 1700000000
                }
          store.updateState firstRef (uploaded firstRef duplicateHash)
          rejectedResult <- store.updateState rejectedRef (uploaded rejectedRef duplicateHash) |> Task.asResult
          case rejectedResult of
            Err err -> Text.contains "23505" err |> shouldBe True
            Ok _ -> fail "Expected duplicate owner/content state to be rejected"
          firstStateResult <- store.getState firstRef |> Task.asResult
          case firstStateResult of
            Ok (Just (Pending _)) -> pass
            Err err -> fail [fmt|The first pending state became unreadable after rejection: #{err}|]
            _ -> fail "The first pending state must survive a rejected duplicate update"
          rejectedStateResult <- store.getState rejectedRef |> Task.asResult
          rejectedStateResult |> shouldBe (Ok Nothing)
          laterWriteResult <- store.updateState laterRef (uploaded laterRef (ContentHash "transaction-cleanup-later")) |> Task.asResult
          laterWriteResult |> shouldBe (Ok ())
          laterStateResult <- store.getState laterRef |> Task.asResult
          case laterStateResult of
            Ok (Just (Pending _)) -> pass
            _ -> fail "A distinct update must succeed after the rejected duplicate"
    it "keeps one reservation across concurrent PostgreSQL writers, reopen, and pending cleanup" \_ -> do
      databaseName <- privateDatabase
      let config = testConfig databaseName
      (_, setupPool) <- PostgresFileStore.newWithCleanup config
      Task.finally
        (HasqlPool.release setupPool |> Task.fromIO)
        (dropTable setupPool)
      let firstCandidate = testCreation "first"
      let competingCandidate = testCreation "second"
      (firstCandidate.fileRef != competingCandidate.fileRef) |> shouldBe True
      (firstCandidate.blobKey != competingCandidate.blobKey) |> shouldBe True
      (store, pool) <- PostgresFileStore.newWithCleanup config
      Task.finally
        (HasqlPool.release pool |> Task.fromIO)
        do
          (first, second) <- AsyncTask.runConcurrently
            (store.reserveCreation firstCandidate, store.reserveCreation competingCandidate)
          first |> shouldBe second
          (reopened, reopenedPool) <- PostgresFileStore.newWithCleanup config
          Task.finally
            (HasqlPool.release reopenedPool |> Task.fromIO)
            do
              repeated <- reopened.reserveCreation firstCandidate
              repeated |> shouldBe first
              reopened.publishRetained repeated
              retainedState <- reopened.getState repeated.fileRef
              case retainedState of
                Just (Retained _) -> pass
                _ -> fail "Expected the published reservation after reopening the PostgreSQL store"
              PostgresFileStore.deleteFileState reopenedPool repeated.fileRef
              stateAfterCleanup <- reopened.getState repeated.fileRef
              case stateAfterCleanup of
                Just (Retained _) -> pass
                _ -> fail "Pending cleanup must preserve retained state and its creation identity"
              reopened.updateState repeated.fileRef (FileDeleted FileDeletedData
                { fileRef = repeated.fileRef
                , reason = UserRequested
                , deletedAt = 1700000100
                })
              PostgresFileStore.deleteFileState reopenedPool repeated.fileRef
              deletedState <- reopened.getState repeated.fileRef
              deletedState |> shouldBe (Just Deleted)
              stillCanonical <- reopened.reserveCreation competingCandidate
              stillCanonical |> shouldBe first

    it "still deletes a confirmed state without a creation identity" \_ -> do
      databaseName <- privateDatabase
      let config = testConfig databaseName
      (_, setupPool) <- PostgresFileStore.newWithCleanup config
      Task.finally
        (HasqlPool.release setupPool |> Task.fromIO)
        (dropTable setupPool)
      (store, pool) <- PostgresFileStore.newWithCleanup config
      Task.finally
        (HasqlPool.release pool |> Task.fromIO)
        do
          let fileRef = FileRef "legacy-confirmed-cleanup"
          let confirmed = Confirmed Lifecycle.ConfirmedFile
                { metadata = Lifecycle.FileMetadata
                    { ref = fileRef
                    , filename = "legacy.pdf"
                    , contentType = "application/pdf"
                    , sizeBytes = 4
                    , blobKey = BlobKey "legacy-confirmed-blob"
                    , uploadedAt = 1700000000
                    , contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                    }
                , ownerHash = OwnerHash "legacy-confirmed-owner"
                , confirmedByRequestId = "legacy-confirmed-request"
                }
          store.setState fileRef confirmed
          PostgresFileStore.deleteFileState pool fileRef
          stateAfterCleanup <- store.getState fileRef
          stateAfterCleanup |> shouldBe Nothing


testCreation :: Text -> FileCreation
testCreation variant =
  FileCreation
    { ownerHash = OwnerHash "postgres-create-file-owner",
      creationKey = "postgres-create-file-key",
      fileRef = FileRef [fmt|file_postgres_create_#{variant}|],
      blobKey = BlobKey [fmt|blob_postgres_create_#{variant}|],
      filename = "report.pdf",
      contentType = "application/pdf",
      sizeBytes = 8,
      contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      createdAt = 1700000000
    }


-- | Drop table for test isolation
dropTable :: Pool -> Task Text ()
dropTable pool = do
  let dropCreationTable = Session.sql "DROP TABLE IF EXISTS file_creation CASCADE"
  creationResult <- HasqlPool.use pool dropCreationTable
    |> Task.fromIO
    |> Task.map Result.fromEither
  case creationResult of
    Err err -> Task.throw [fmt|Failed to drop file creation table: #{show err}|]
    Ok _ -> pass
  let session = Session.sql "DROP TABLE IF EXISTS file_upload_state CASCADE"
  result <- HasqlPool.use pool session
    |> Task.fromIO
    |> Task.map Result.fromEither
  case result of
    Err err -> Task.throw [fmt|Failed to drop table: #{show err}|]
    Ok _ -> Task.yield ()
