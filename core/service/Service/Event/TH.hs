-- | Template Haskell marker for event types.
--
-- The 'deriveEvent' marker emits Show, Generic, Json.FromJSON, and Json.ToJSON
-- instances for the named type, skipping any that are already in scope.
--
-- Usage:
--
-- > data CartCreated = CartCreated { cartId :: Uuid }
-- >
-- > deriveEvent ''CartCreated
--
-- CONVENTION — \"marker last\":  Any custom instance you wish to write
-- (e.g. a hand-crafted 'Json.ToJSON') must be declared /before/ the
-- @deriveEvent@ call.  Declaring it after produces a GHC \"Duplicate instance\"
-- error because the marker will have already emitted the derived version.
module Service.Event.TH (
  deriveEvent,
  event,
) where

import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import Service.TH.Boilerplate (emitJsonAndDerivingBoilerplate)


-- | Derive Show, Generic, Json.FromJSON, and Json.ToJSON for an event type.
--
-- Each class is emitted only when it is not already in scope (idempotent).
deriveEvent :: TH.Name -> THLib.DecsQ
deriveEvent = emitJsonAndDerivingBoilerplate
{-# INLINE deriveEvent #-}


-- | Compatibility name for 'deriveEvent'.
event :: TH.Name -> THLib.DecsQ
event = deriveEvent
{-# INLINE event #-}
