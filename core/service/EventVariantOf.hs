{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EventVariantOf
  ( -- * Typeclass
    EventVariantOf (..)
    -- * Helper
  , event
  ) where


-- | Declares that @eventVariant@ can be converted into @event@.
--
-- This typeclass enables individual event types to be wrapped into
-- an entity's event ADT. The Decider smart constructors use this
-- constraint to accept variant types directly.
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
--
-- -- Standalone event type with explicit instance
-- data PdfUploaded = PdfUploaded { entityId :: Uuid, pdfRef :: Text }
--
-- instance EventVariantOf DocumentEvent PdfUploaded where
--   fromVariant uploaded = DocumentPdfUploaded uploaded.entityId uploaded.pdfRef
-- @
class EventVariantOf event eventVariant where
  -- | Convert an event variant into the parent event ADT.
  fromVariant :: eventVariant -> event


-- | Every event type is a variant of itself.
--
-- This instance ensures that existing code passing the event ADT directly
-- to Decider smart constructors continues to work without modification.
--
-- @
-- -- This still works (CartEvent is a variant of CartEvent):
-- Decider.acceptExisting [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
-- @
instance EventVariantOf event event where
  fromVariant eventValue = eventValue
  {-# INLINE fromVariant #-}


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
