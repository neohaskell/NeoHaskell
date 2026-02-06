module Service.FileUpload.Core (
  -- * Opaque References
  FileRef (..),
  BlobKey (..),
  OwnerHash (..),

  -- * File Metadata
  FileMetadata (..),
  ResolvedFile (..),

  -- * Errors
  FileAccessError (..),

  -- * Events
  FileUploadEvent (..),
  FileUploadedData (..),
  FileConfirmedData (..),
  FileDeletedData (..),
  FileDeletionReason (..),

  -- * Configuration
  FileUploadConfig (..),
  FileStateStoreBackend (..),

  -- * Internal Types (exported for use by other nhcore modules)
  InternalFileUploadConfig (..),
) where

import Array (Array)
import Basics
import Data.Aeson qualified as GhcAeson
import Data.Hashable qualified as GhcHashable
import Json qualified
import Maybe (Maybe)
import Schema (ToSchema (..))
import Schema qualified
import Text (Text)


-- ==========================================================================
-- Opaque References (with redacted Show instances for security)
-- ==========================================================================

-- | Opaque reference to an uploaded file (used in command payloads)
-- Show instance is redacted to prevent leaking file identifiers in logs
newtype FileRef = FileRef Text
  deriving (Generic, Eq, Ord)

instance Show FileRef where
  show _ = "FileRef <REDACTED>"

instance GhcHashable.Hashable FileRef where
  hashWithSalt salt (FileRef t) = GhcHashable.hashWithSalt salt t

instance Json.FromJSON FileRef
instance Json.ToJSON FileRef


-- | ToSchema instance for OpenAPI - FileRef is represented as a string
instance ToSchema FileRef where
  toSchema = Schema.SText


-- | Internal storage key for blob store
-- Show instance is redacted to prevent leaking storage paths in logs
newtype BlobKey = BlobKey Text
  deriving (Generic, Eq, Ord)

instance Show BlobKey where
  show _ = "BlobKey <REDACTED>"

instance Json.FromJSON BlobKey
instance Json.ToJSON BlobKey


-- | Pseudonymous owner identity (HMAC of user ID)
-- Show instance is redacted to prevent leaking user information in logs
newtype OwnerHash = OwnerHash Text
  deriving (Generic, Eq, Ord)

instance Show OwnerHash where
  show _ = "OwnerHash <REDACTED>"

instance Json.FromJSON OwnerHash
instance Json.ToJSON OwnerHash


-- ==========================================================================
-- File Metadata
-- ==========================================================================

-- | Metadata about an uploaded file
data FileMetadata = FileMetadata
  { filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileMetadata
instance Json.ToJSON FileMetadata


-- | Resolved file with full metadata (available in RequestContext.files)
data ResolvedFile = ResolvedFile
  { ref :: FileRef
  , metadata :: FileMetadata
  , blobKey :: BlobKey
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON ResolvedFile
instance Json.ToJSON ResolvedFile


-- ==========================================================================
-- File Lifecycle Events
-- ==========================================================================

-- | Data for FileUploaded event
data FileUploadedData = FileUploadedData
  { fileRef :: FileRef
  , ownerHash :: OwnerHash
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , expiresAt :: Int64
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileUploadedData
instance Json.ToJSON FileUploadedData


-- | Data for FileConfirmed event
data FileConfirmedData = FileConfirmedData
  { fileRef :: FileRef
  , confirmedByRequestId :: Text
  , confirmedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileConfirmedData
instance Json.ToJSON FileConfirmedData


-- | Data for FileDeleted event
data FileDeletedData = FileDeletedData
  { fileRef :: FileRef
  , reason :: FileDeletionReason
  , deletedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileDeletedData
instance Json.ToJSON FileDeletedData


-- | File lifecycle events (event-sourced state)
-- Each constructor carries a dedicated record type to avoid partial field selectors
data FileUploadEvent
  = FileUploaded FileUploadedData
  | FileConfirmed FileConfirmedData
  | FileDeleted FileDeletedData
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileUploadEvent
instance Json.ToJSON FileUploadEvent


-- | Reason why a file was deleted
data FileDeletionReason
  = Orphaned -- TTL expired without confirmation
  | UserRequested -- Explicit deletion by owner
  | AdminPurge -- System cleanup
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileDeletionReason
instance Json.ToJSON FileDeletionReason


-- ==========================================================================
-- File Access Errors
-- ==========================================================================

-- | Shared error type for file access operations (resolution, download, etc.)
data FileAccessError
  = FileNotFound FileRef
  -- ^ No file upload exists for this reference
  | StateLookupFailed FileRef Text
  -- ^ Failed to query state store (preserves original error)
  | NotOwner FileRef
  -- ^ File exists but belongs to a different user
  | FileExpired FileRef
  -- ^ File upload has expired (pending TTL exceeded)
  | FileIsDeleted FileRef
  -- ^ File has been deleted (named to avoid collision with FileDeleted event)
  | BlobMissing FileRef
  -- ^ File metadata exists but blob is missing from storage
  | StorageError Text
  -- ^ Error accessing blob store
  deriving (Generic, Eq)


instance Show FileAccessError where
  show err = case err of
    FileNotFound ref -> [fmt|FileNotFound: #{show ref}|]
    StateLookupFailed ref msg -> [fmt|StateLookupFailed: #{show ref} - #{msg}|]
    NotOwner ref -> [fmt|NotOwner: #{show ref}|]
    FileExpired ref -> [fmt|FileExpired: #{show ref}|]
    FileIsDeleted ref -> [fmt|FileIsDeleted: #{show ref}|]
    BlobMissing ref -> [fmt|BlobMissing: #{show ref}|]
    StorageError msg -> [fmt|StorageError: #{msg}|]


instance Json.FromJSON FileAccessError
instance Json.ToJSON FileAccessError


-- ==========================================================================
-- Configuration
-- ==========================================================================

-- | Backend for storing file upload state (lifecycle tracking).
--
-- Choose based on your deployment:
--
-- * 'InMemoryStateStore' - For development/testing. State is lost on restart.
-- * 'PostgresStateStore' - For production. Uses same connection parameters as your event store.
--
-- Example:
--
-- @
-- -- Development
-- stateStoreBackend = InMemoryStateStore
--
-- -- Production (same connection as event store)
-- stateStoreBackend = PostgresStateStore
--     { pgHost = "localhost"
--     , pgPort = 5432
--     , pgDatabase = "neohaskell"
--     , pgUser = "neohaskell"
--     , pgPassword = "neohaskell"
--     }
-- @
-- | All fields are marked lazy (~) to allow this config to reference 'Config.get'
-- values, which are only available after 'Application.run' loads the config.
data FileStateStoreBackend
  = InMemoryStateStore
  -- ^ In-memory storage (lost on restart, for development/testing)
  | PostgresStateStore
      { pgHost :: ~Text
      -- ^ PostgreSQL host
      , pgPort :: ~Int
      -- ^ PostgreSQL port
      , pgDatabase :: ~Text
      -- ^ Database name
      , pgUser :: ~Text
      -- ^ Database user
      , pgPassword :: ~Text
      -- ^ Database password
      }
  -- ^ PostgreSQL storage (persistent, recommended for production)
  deriving (Generic, Eq)


-- | Show instance that redacts the password to prevent leaking secrets in logs
instance Show FileStateStoreBackend where
  show backend = case backend of
    InMemoryStateStore -> "InMemoryStateStore"
    PostgresStateStore {pgHost, pgPort, pgDatabase, pgUser} ->
      [fmt|PostgresStateStore {pgHost = "#{pgHost}", pgPort = #{pgPort}, pgDatabase = "#{pgDatabase}", pgUser = "#{pgUser}", pgPassword = <REDACTED>}|]


instance Json.FromJSON FileStateStoreBackend


-- | ToJSON instance that redacts the password to prevent leaking secrets in JSON
instance Json.ToJSON FileStateStoreBackend where
  toJSON backend = case backend of
    InMemoryStateStore -> GhcAeson.object
      [ ("tag", GhcAeson.toJSON @Text "InMemoryStateStore")
      ]
    PostgresStateStore {pgHost, pgPort, pgDatabase, pgUser} -> GhcAeson.object
      [ ("tag", GhcAeson.toJSON @Text "PostgresStateStore")
      , ("pgHost", GhcAeson.toJSON pgHost)
      , ("pgPort", GhcAeson.toJSON pgPort)
      , ("pgDatabase", GhcAeson.toJSON pgDatabase)
      , ("pgUser", GhcAeson.toJSON pgUser)
      , ("pgPassword", GhcAeson.toJSON @Text "<REDACTED>")
      ]


-- | Declarative configuration for file uploads.
--
-- This is the user-facing configuration type. IO initialization is deferred
-- to 'Application.run', making application setup purely declarative.
--
-- == Basic Example
--
-- @
-- app = Application.new
--   |> Application.withEventStore postgresConfig
--   |> Application.withFileUpload FileUploadConfig
--       { blobStoreDir = "./uploads"
--       , stateStoreBackend = InMemoryStateStore  -- For development
--       , maxFileSizeBytes = 10485760  -- 10 MB
--       , pendingTtlSeconds = 21600    -- 6 hours
--       , cleanupIntervalSeconds = 900 -- 15 minutes
--       , allowedContentTypes = Nothing
--       , storeOriginalFilename = True
--       }
-- @
--
-- == Reusing PostgreSQL Connection Parameters
--
-- For production, you can reuse the same connection parameters as your event store:
--
-- @
-- -- Define once
-- postgresConfig = PostgresEventStore
--   { host = "localhost", port = 5432, databaseName = "myapp"
--   , user = "myuser", password = "mypassword"
--   }
--
-- -- Reuse for file state store
-- fileUploadConfig = FileUploadConfig
--   { blobStoreDir = "./uploads"
--   , stateStoreBackend = PostgresStateStore
--       { pgHost = postgresConfig.host
--       , pgPort = postgresConfig.port
--       , pgDatabase = postgresConfig.databaseName
--       , pgUser = postgresConfig.user
--       , pgPassword = postgresConfig.password
--       }
--   , ...
--   }
--
-- app = Application.new
--   |> Application.withEventStore postgresConfig
--   |> Application.withFileUpload fileUploadConfig
-- @
--
-- == Migration from Imperative Setup
--
-- If you previously used the Task-based setup:
--
-- @
-- -- OLD (imperative)
-- appTask :: Task Text Application
-- appTask = do
--   blobStore <- LocalBlobStore.createBlobStore config
--   stateStore <- PostgresFileStore.new postgresConfig
--   let setup = FileUpload.defaultFileUploadSetup blobStore stateStore
--   Task.yield (baseApp |> Application.withFileUpload setup)
--
-- -- NEW (declarative)
-- app :: Application
-- app = baseApp
--   |> Application.withFileUpload FileUploadConfig
--       { blobStoreDir = "./uploads"
--       , stateStoreBackend = PostgresStateStore {...}
--       , ...
--       }
-- @
-- | All fields are marked lazy (~) to allow this config to reference 'Config.get'
-- values, which are only available after 'Application.run' loads the config.
data FileUploadConfig = FileUploadConfig
  { blobStoreDir :: ~Text
  -- ^ Directory for storing uploaded files (created if missing)
  , stateStoreBackend :: ~FileStateStoreBackend
  -- ^ Where to track file lifecycle state
  , maxFileSizeBytes :: ~Int64
  -- ^ Maximum upload size (default: 10485760 = 10 MB)
  , pendingTtlSeconds :: ~Int64
  -- ^ TTL for unconfirmed uploads before cleanup (default: 21600 = 6 hours)
  , cleanupIntervalSeconds :: ~Int64
  -- ^ How often the cleanup worker runs (default: 900 = 15 minutes)
  , allowedContentTypes :: ~(Maybe (Array Text))
  -- ^ MIME type allowlist (Nothing = all types allowed)
  , storeOriginalFilename :: ~Bool
  -- ^ Whether to store the original filename in events (default: True)
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileUploadConfig
instance Json.ToJSON FileUploadConfig


-- | Internal configuration for file uploads (used after initialization).
--
-- This type holds the runtime configuration after IO initialization has
-- occurred. Exported for use by other nhcore modules (Web.hs, Application.hs).
-- End users should use 'FileUploadConfig' for application configuration.
data InternalFileUploadConfig = InternalFileUploadConfig
  { pendingTtlSeconds :: Int64
  , cleanupIntervalSeconds :: Int64
  , maxFileSizeBytes :: Int64
  , allowedContentTypes :: Maybe (Array Text)
  , storeOriginalFilename :: Bool
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON InternalFileUploadConfig
instance Json.ToJSON InternalFileUploadConfig
