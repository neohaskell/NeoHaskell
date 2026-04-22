-- | The production @emit@ hot-path shim.
--
-- At emit time, looks up the pre-bound closure in the 'DispatchRegistry'
-- and invokes it directly. A missing entry is a wire-up bug per ADR-0055
-- §6 — throws 'PermanentFailure' rather than silently falling back to
-- 'runReal'. Silent fallback would (a) hide the misconfiguration, (b)
-- bypass the selection-flag decision made at startup, and (c) pay a
-- dictionary-passing call per emit instead of the pre-bound closure call
-- the 50k req/s budget assumes.
module Service.Integration.ShimEmit (emit) where

import Basics
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy (..))
import Maybe (Maybe (..))
import Service.Integration.Adapter (Integration, Response)
import Service.Integration.DispatchRegistry (DispatchRegistry)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.IntegrationError (IntegrationError (..))
import Task (Task)
import Task qualified


emit ::
  forall request.
  (Integration request, Typeable request, Typeable (Response request)) =>
  DispatchRegistry ->
  request ->
  Task IntegrationError (Response request)
emit registry request =
  case DispatchRegistry.lookup @request registry of
    Just closure -> closure request
    Nothing ->
      Task.throw
        ( PermanentFailure
            [fmt|Integration not registered: #{show (typeRep (Proxy @request))}|]
        )
{-# INLINE emit #-}
