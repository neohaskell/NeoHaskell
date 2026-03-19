{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EventVariantOf
  ( -- * Typeclass
    EventVariantOf (..)
    -- * Helper
  , event
  ) where

import Maybe (Maybe (..))


-- | Declares that @eventVariant@ can be converted to/from @event@.
--
-- This typeclass enables individual event types to be wrapped into
-- an entity's event ADT ('fromVariant') and extracted back out
-- ('toVariant'). The Decider smart constructors use 'fromVariant'
-- to accept variant types directly. The OutboundIntegration dispatch
-- uses 'toVariant' to extract specific variants for typed handlers.
--
-- The identity instance @EventVariantOf event event@ is provided,
-- so existing code that passes the event ADT directly continues to work.
--
-- Example:
--
-- @
-- -- The event ADT
-- data CartEvent
--   = CartCreated { entityId :: Uuid, ownerId :: Text }
--   | ItemAdded { entityId :: Uuid, stockId :: Uuid, quantity :: Int }
--
-- -- Identity instance (provided by default — every ADT is a variant of itself)
-- instance EventVariantOf CartEvent CartEvent where
--   fromVariant cartEvent = cartEvent
--   toVariant cartEvent = Just cartEvent
--
-- -- Standalone event type with explicit instance
-- data PdfUploaded = PdfUploaded { entityId :: Uuid, pdfRef :: Text }
--
-- instance EventVariantOf DocumentEvent PdfUploaded where
--   fromVariant uploaded = DocumentPdfUploaded uploaded.entityId uploaded.pdfRef
--   toVariant docEvent = case docEvent of
--     DocumentPdfUploaded eid ref -> Just (PdfUploaded { entityId = eid, pdfRef = ref })
--     _ -> Nothing
-- @
class EventVariantOf event eventVariant where
  -- | Convert an event variant into the parent event ADT.
  fromVariant :: eventVariant -> event

  -- | Extract a specific event variant from the parent event ADT.
  --
  -- Returns 'Just' if the event matches this variant, 'Nothing' otherwise.
  -- Used by OutboundIntegration dispatch to route decoded events to
  -- typed handlers.
  toVariant :: event -> Maybe eventVariant


-- | Every event type is a variant of itself.
--
-- This instance ensures that existing code passing the event ADT directly
-- to Decider smart constructors continues to work without modification.
--
-- @
-- -- This still works (CartEvent is a variant of CartEvent):
-- Decider.acceptExisting [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
--
-- -- And toVariant always succeeds for identity:
-- toVariant (ItemAdded { entityId = id, stockId = sid, quantity = 1 }) == Just (ItemAdded ...)
-- @
instance {-# OVERLAPPABLE #-} EventVariantOf event event where
  fromVariant eventValue = eventValue
  {-# INLINE fromVariant #-}
  toVariant eventValue = Just eventValue
  {-# INLINE toVariant #-}


-- | Wrap an event variant into the parent event ADT.
--
-- This is an alias for 'fromVariant' with a friendlier name.
-- Use it when building arrays of mixed event types in a single decision:
--
-- @
-- Decider.acceptExisting
--   [ event (PdfUploaded { entityId = id, pdfRef = ref })
--   , event (TimestampRecorded { entityId = id, timestamp = now })
--   ]
-- @
--
-- For single-type arrays, you don't need this — just pass the events directly:
--
-- @
-- Decider.acceptExisting [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
-- @
event :: forall event eventVariant. (EventVariantOf event eventVariant) => eventVariant -> event
event variant = fromVariant variant
{-# INLINE event #-}
