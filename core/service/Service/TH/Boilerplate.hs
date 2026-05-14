-- | Shared TH primitives for concept-marker boilerplate emission.
--
-- Internal to nhcore.  Used by Service.CommandExecutor.TH,
-- Service.Query.TH, and Service.Event.TH to emit the JSON and
-- @deriving stock@ instances every command, event, and query type
-- needs, while remaining a no-op when an instance is already in scope
-- (so consumers can declare custom encodings before the marker call).
module Service.TH.Boilerplate (
  emitInstanceIfMissing,
  emitStockDeriving,
  emitEmptyInstance,
  emitJsonAndDerivingBoilerplate,
) where

import Core
import Data.Aeson qualified as Json
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH


-- | Emit a class instance only when it is not already in scope.
--
-- Checks via 'TH.reifyInstances' whether @className \@typeName@ exists.
-- Returns the @fallbackDecl@ when the instance is absent, or an empty list
-- when it is already present.
emitInstanceIfMissing ::
  TH.Name ->
  TH.Name ->
  THLib.DecsQ ->
  THLib.DecsQ
emitInstanceIfMissing className typeName fallbackDecl = do
  instances <- TH.reifyInstances className [TH.ConT typeName]
  case instances of
    [] -> fallbackDecl
    _ : _ -> pure []
{-# INLINE emitInstanceIfMissing #-}


-- | Emit a @deriving stock instance Class Type@ declaration when the
-- class instance is not already in scope.  Used for 'Show' and 'Generic'.
emitStockDeriving :: TH.Name -> TH.Name -> THLib.DecsQ
emitStockDeriving className typeName =
  emitInstanceIfMissing className typeName do
    pure
      [ TH.StandaloneDerivD
          (Just TH.StockStrategy)
          []
          (TH.ConT className `TH.AppT` TH.ConT typeName)
      ]
{-# INLINE emitStockDeriving #-}


-- | Emit an empty-body @instance Class Type@ declaration when the class
-- instance is not already in scope.  Used for 'Json.FromJSON',
-- 'Json.ToJSON', and 'ToSchema' — each defaults its methods via 'Generic'.
emitEmptyInstance :: TH.Name -> TH.Name -> THLib.DecsQ
emitEmptyInstance className typeName =
  emitInstanceIfMissing className typeName do
    pure
      [ TH.InstanceD
          Nothing
          []
          (TH.ConT className `TH.AppT` TH.ConT typeName)
          []
      ]
{-# INLINE emitEmptyInstance #-}


-- | Emit the four-class boilerplate every concept marker shares:
-- 'Show', 'Generic', 'Json.FromJSON', 'Json.ToJSON'.  Each is emitted
-- only when not already in scope.
--
-- 'Service.Query.TH.deriveQuery' calls 'emitEmptyInstance' separately
-- for 'ToSchema' on top of this four-pack.
emitJsonAndDerivingBoilerplate :: TH.Name -> THLib.DecsQ
emitJsonAndDerivingBoilerplate typeName = do
  showDecls <- emitStockDeriving ''Show typeName
  genericDecls <- emitStockDeriving ''Generic typeName
  fromJsonDecls <- emitEmptyInstance ''Json.FromJSON typeName
  toJsonDecls <- emitEmptyInstance ''Json.ToJSON typeName
  pure (showDecls ++ genericDecls ++ fromJsonDecls ++ toJsonDecls)
{-# INLINE emitJsonAndDerivingBoilerplate #-}
