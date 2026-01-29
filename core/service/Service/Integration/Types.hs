-- | # Service.Integration.Types
--
-- Internal types for the integration dispatcher system.
--
-- These types are used by the Application runtime to manage integration workers.
-- They are NOT part of the user-facing API - users interact via:
--
-- * 'Integration.Outbound' - for outbound integrations
-- * 'Integration.Inbound' - for inbound integrations
-- * 'Integration.Lifecycle.OutboundConfig' - for stateful integrations
--
-- == Architecture
--
-- @
-- User API (Integration.*)          Internal Runtime (Service.Integration.*)
-- =====================================================================
-- Integration.Outbound      --->    OutboundRunner (type-erased, JSON)
-- Integration.Inbound       --->    (passed directly to worker)
-- Lifecycle.OutboundConfig  --->    OutboundLifecycleRunner -> WorkerState
-- @
module Service.Integration.Types (
  -- * Outbound Runner Types
  OutboundRunner (..),
  OutboundLifecycleRunner (..),
  WorkerState (..),
) where

import Array (Array)
import Basics
import Integration qualified
import Json qualified
import Maybe (Maybe)
import Service.Event (Event)
import Service.Event.StreamId (StreamId)
import Service.EventStore.Core (EventStore)
import Task (Task)
import Text (Text)


-- | A type-erased outbound integration runner.
--
-- This wraps an integration function with JSON decoding so it can process
-- raw events from the event store. Created by 'Application.withOutbound'.
--
-- The 'processEvent' function receives the EventStore so it can reconstruct
-- the entity state via event replay before calling the integration function.
data OutboundRunner = OutboundRunner
  { entityTypeName :: Text
  , processEvent :: Maybe Integration.ActionContext -> EventStore Json.Value -> Event Json.Value -> Task Text (Array Integration.CommandPayload)
  }


-- | A type-erased outbound integration runner with lifecycle management.
--
-- Use this for integrations that need expensive resources like:
-- - Database connection pools
-- - Embedded interpreters
-- - gRPC channels
data OutboundLifecycleRunner = OutboundLifecycleRunner
  { entityTypeName :: Text
  , spawnWorkerState :: StreamId -> Task Text WorkerState
  }


-- | Internal state for a running lifecycle worker.
--
-- Created by the lifecycle runner's 'spawnWorkerState' function.
data WorkerState = WorkerState
  { processEvent :: Event Json.Value -> Task Text (Array Integration.CommandPayload)
  , cleanup :: Task Text Unit
  }
