-- | Template Haskell marker for event types.
--
-- The 'event' marker emits Show, Generic, Json.FromJSON, and Json.ToJSON
-- instances for the named type, skipping any that are already in scope.
--
-- Usage:
--
-- > data CartCreated = CartCreated { cartId :: Uuid }
-- >
-- > event ''CartCreated
--
-- CONVENTION — \"marker last\":  Any custom instance you wish to write
-- (e.g. a hand-crafted 'Json.ToJSON') must be declared /before/ the
-- @event@ call.  Declaring it after produces a GHC \"Duplicate instance\"
-- error because the marker will have already emitted the derived version.
module Service.Event.TH (
  event,
) where

import Core hiding (event)
import Data.Aeson qualified as Json
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import Service.CommandExecutor.TH.Internal (emitInstanceIfMissing)


-- | Derive Show, Generic, Json.FromJSON, and Json.ToJSON for an event type.
--
-- Each class is emitted only when it is not already in scope (idempotent).
event :: TH.Name -> THLib.DecsQ
event typeName = do
  showDecls <-
    emitInstanceIfMissing ''Show typeName do
      pure
        [ TH.StandaloneDerivD
            (Just TH.StockStrategy)
            []
            (TH.ConT ''Show `TH.AppT` TH.ConT typeName)
        ]
  genericDecls <-
    emitInstanceIfMissing ''Generic typeName do
      pure
        [ TH.StandaloneDerivD
            (Just TH.StockStrategy)
            []
            (TH.ConT ''Generic `TH.AppT` TH.ConT typeName)
        ]
  fromJsonDecls <-
    emitInstanceIfMissing ''Json.FromJSON typeName do
      pure
        [ TH.InstanceD
            Nothing
            []
            (TH.ConT ''Json.FromJSON `TH.AppT` TH.ConT typeName)
            []
        ]
  toJsonDecls <-
    emitInstanceIfMissing ''Json.ToJSON typeName do
      pure
        [ TH.InstanceD
            Nothing
            []
            (TH.ConT ''Json.ToJSON `TH.AppT` TH.ConT typeName)
            []
        ]
  pure (showDecls ++ genericDecls ++ fromJsonDecls ++ toJsonDecls)
{-# INLINE event #-}
