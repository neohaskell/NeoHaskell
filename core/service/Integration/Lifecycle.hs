-- | # Integration.Lifecycle
--
-- Lifecycle management for stateful integrations.
--
-- Use this module when your integration needs to manage expensive resources like:
--
-- * Database connection pools
-- * Embedded interpreters (Lua, Python, etc.)
-- * gRPC channels
-- * WebSocket subscriptions
--
-- == Usage
--
-- @
-- import Integration.Lifecycle qualified as Lifecycle
--
-- postgresNotifier :: Lifecycle.OutboundConfig PostgresPool
-- postgresNotifier = Lifecycle.OutboundConfig
--   { initialize = \\streamId -> do
--       pool <- Postgres.createPool connectionString
--       Task.yield pool
--
--   , processEvent = \\pool event -> do
--       Postgres.notify pool event
--       Task.yield []
--
--   , cleanup = \\pool -> do
--       Postgres.closePool pool
--   }
-- @
--
-- Workers are created per-entity. When a worker is idle for too long
-- (configurable timeout), it is reaped and cleanup is called.
-- The next event for that entity will spawn a fresh worker.
module Integration.Lifecycle (
  -- * Configuration
  OutboundConfig (..),
) where

import Array (Array)
import Basics
import Integration qualified
import Json qualified
import Service.Event (Event)
import Service.Event.StreamId (StreamId)
import Task (Task)
import Text (Text)


-- | Configuration for an outbound integration with resource lifecycle.
--
-- The lifecycle hooks are called as follows:
--
-- 1. @initialize@ - Called once when a worker is spawned for an entity
-- 2. @processEvent@ - Called for each event (uses state from initialize)
-- 3. @cleanup@ - Called when the worker is reaped (idle timeout) or on shutdown
--
-- If a worker is reaped and a new event arrives for that entity,
-- @initialize@ is called again to create a fresh worker.
data OutboundConfig state = OutboundConfig
  { -- | Initialize resources for this entity.
    --
    -- Called once when the worker is spawned. The returned state is passed
    -- to @processEvent@ and @cleanup@.
    --
    -- Should be idempotent - may be called multiple times if worker is reaped
    -- and restarted.
    initialize :: StreamId -> Task Text state

    -- | Process an event using the initialized state.
    --
    -- Called for each event that arrives for this entity.
    -- Returns command payloads to dispatch to other services.
  , processEvent :: state -> Event Json.Value -> Task Text (Array Integration.CommandPayload)

    -- | Cleanup resources when the worker is reaped or on shutdown.
    --
    -- Should release any resources created in @initialize@ (close connections,
    -- stop interpreters, etc.).
    --
    -- Should complete quickly. Long-running cleanup operations may delay
    -- worker shutdown and system termination.
  , cleanup :: state -> Task Text Unit
  }
