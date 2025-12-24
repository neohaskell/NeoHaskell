{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Entity.Core (
  Entity (..),
  Event (..),
  EntityOf,
  EventOf,
) where

import Basics
import Uuid (Uuid)


-- | Maps a command type to its corresponding entity type.
--
-- Example:
--
-- @
-- type instance EntityOf CreateCart = CartEntity
-- @
type family EntityOf (command :: Type) :: Type


-- | Maps an entity type to its corresponding event type.
--
-- Example:
--
-- @
-- type instance EventOf CartEntity = CartEvent
-- @
type family EventOf (entityType :: Type)


-- | The Event typeclass defines how events are routed to their entity streams.
--
-- Each event must be able to identify which entity it belongs to via 'getEventEntityIdImpl'.
-- This is used by the event store to determine which stream to write the event to.
class Event event where
  -- | Extract the entity ID from an event.
  --
  -- This allows the system to route events to the correct entity stream.
  getEventEntityIdImpl :: event -> EntityIdType (EntityOf event)


-- | The Entity typeclass represents domain objects that can be reconstructed from events.
--
-- Entities are the "left fold" of their event history - starting from an initial state
-- and applying each event in sequence via 'updateImpl'.
class Entity entity where
  -- | The type of identifier used for this entity.
  --
  -- Defaults to 'Uuid' but can be overridden for entities that use different ID types.
  type EntityIdType entity :: Type
  type EntityIdType entity = Uuid

  -- | The initial state of the entity before any events are applied.
  --
  -- This is used as the starting point when reconstructing an entity from its event stream.
  initialStateImpl :: entity

  -- | Apply an event to the entity state, producing a new state.
  --
  -- This is the reducer function used to reconstruct entity state from events.
  -- It should be a pure function with no side effects.
  updateImpl :: EventOf entity -> entity -> entity
