-- | Internal helpers for TH marker boilerplate emission.
--
-- NOT exported from any public interface.  Used only by
-- Service.CommandExecutor.TH, Service.Query.TH, and Service.Event.TH.
module Service.CommandExecutor.TH.Internal (
  emitInstanceIfMissing,
) where

import Core
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
