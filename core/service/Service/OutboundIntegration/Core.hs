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
-- Each handler type handles one event payload for an entity.
-- The framework extracts that payload from the entity's stored event sum.
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
-- handleEvent :: CartEntity -> ItemAddedPayload -> Integration.Outbound
-- handleEvent cart payload = Integration.batch [...]
--
-- outboundIntegration ''ReserveStockOnItemAdded
-- @
class OutboundIntegration handler where
  -- | The event payload this handler processes.
  -- The explicit legacy marker sets this to the full event sum.
  type HandledEvent handler :: Type

  -- | Process an event for the given entity state.
  --
  -- The entity is reconstructed from the event stream via replay.
  -- The event is the selected payload.
  handleEventImpl :: EntityOf handler -> HandledEvent handler -> Integration.Outbound
