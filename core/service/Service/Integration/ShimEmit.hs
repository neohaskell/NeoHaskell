-- | The production @emit@ hot-path shim.
--
-- At emit time, looks up the pre-bound closure in the 'DispatchRegistry'
-- and invokes it directly. Falls back to 'runReal' when the registry does
-- not hold a binding, so that integrations registered only at compile time
-- (not via the startup wire-up) still dispatch correctly.
module Service.Integration.ShimEmit (emit) where

import Data.Typeable (Typeable)
import Maybe (Maybe (..))
import Service.Integration.Adapter (Integration, Response)
import Service.Integration.Adapter qualified as Adapter
import Service.Integration.DispatchRegistry (DispatchRegistry)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.IntegrationError (IntegrationError)
import Task (Task)


emit ::
  forall request.
  (Integration request, Typeable request, Typeable (Response request)) =>
  DispatchRegistry ->
  request ->
  Task IntegrationError (Response request)
emit registry request =
  case DispatchRegistry.lookup @request registry of
    Just closure -> closure request
    Nothing -> Adapter.runReal request
{-# INLINE emit #-}
