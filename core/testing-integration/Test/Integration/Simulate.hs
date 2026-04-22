{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Test-only inbound driver.
--
-- 'simulate' pushes a synthetic trigger through the fake inbound handle of
-- an 'InboundIntegration' instance — the mirror image of calling the fake
-- outbound dispatcher.
module Test.Integration.Simulate (simulate) where

import Data.Typeable (Typeable)
import Service.Integration.Adapter (InboundIntegration (..))
import Service.Integration.Inbound qualified as Inbound
import Service.Integration.IntegrationError (IntegrationError)
import Task (Task)


simulate ::
  forall inbound.
  (InboundIntegration inbound, Typeable inbound) =>
  Trigger inbound ->
  Task IntegrationError ()
simulate trigger = do
  let handle = runFakeInbound @inbound
  Inbound.inject handle trigger
