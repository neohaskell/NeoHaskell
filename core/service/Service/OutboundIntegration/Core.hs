{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.OutboundIntegration.Core (
  -- * Typeclass
  OutboundIntegration (..),
) where

import Basics
import Integration qualified
import Service.Entity.Core (EntityOf)


-- | Typeclass for typed outbound integration handlers.
--
-- Each handler type handles events for an entity.
-- The framework dispatches events to handlers based on the entity type.
--
-- Jess never implements this typeclass manually — the 'outboundIntegration'
-- TH macro generates the instance from her 'handleEvent' function.
--
-- @
-- data ReserveStockOnItemAdded = ReserveStockOnItemAdded
--   deriving (Generic, Typeable, Show)
--
-- type instance EntityOf ReserveStockOnItemAdded = CartEntity
--
-- handleEvent :: CartEntity -> CartEvent -> Integration.Outbound
-- handleEvent cart event = case event of
--   ItemAdded {stockId, quantity} -> Integration.batch [...]
--   _ -> Integration.none
--
-- outboundIntegration ''ReserveStockOnItemAdded
-- @
class OutboundIntegration handler where
  -- | The event type this handler processes.
  --
  -- Set by the TH macro to 'EventOf (EntityOf handler)' — the entity's
  -- full event ADT.
  type HandledEvent handler :: Type

  -- | Process an event for the given entity state.
  --
  -- The entity is reconstructed from the event stream via replay.
  -- The event is the entity's event ADT.
  handleEventImpl :: EntityOf handler -> HandledEvent handler -> Integration.Outbound
