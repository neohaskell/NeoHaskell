{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

-- | 'Integration' and 'InboundIntegration' typeclasses — the declarative
-- schema for ADR-0055.
--
-- 'Arbitrary' appears only on the constrained default, not on the class head,
-- so production builds link zero QuickCheck code when every integration
-- overrides 'runFake' / 'runFakeInbound'.
--
-- The two classes deliberately use distinct method names
-- (@runReal@\/@runFake@ for outbound, @runRealInbound@\/@runFakeInbound@ for
-- inbound) to avoid an ambiguous overload on the 'runReal' name. The ADR
-- uses one name per class, but Haskell 98's class-method namespace is flat,
-- so the spec's shared vocabulary is a writing convenience only.
module Service.Integration.Adapter
  ( Integration (..),
    InboundIntegration (..),
  )
where

import Basics
import Service.Integration.Inbound (InboundHandle)
import Service.Integration.Inbound qualified as Inbound
import Service.Integration.IntegrationError (IntegrationError)
import Task (Task)
import Task qualified
import Test.QuickCheck qualified as QuickCheck


-- | Declarative outbound integration. @request -> Response request@ is the
-- contract; the framework routes 'runReal' or 'runFake' depending on CLI
-- selection.
class Integration request where
  type Response request :: Type


  runReal :: request -> Task IntegrationError (Response request)


  runFake :: request -> Task IntegrationError (Response request)
  default runFake ::
    (QuickCheck.Arbitrary (Response request)) =>
    request ->
    Task IntegrationError (Response request)
  runFake _request = do
    generated <-
      QuickCheck.generate QuickCheck.arbitrary
        |> Task.fromIO
    Task.yield generated


-- | Declarative inbound integration. The real implementation listens for
-- triggers from an external source; the fake hands back a controllable
-- handle the test harness can drive.
class InboundIntegration inbound where
  type Trigger inbound :: Type


  runRealInbound ::
    (Trigger inbound -> Task IntegrationError ()) ->
    Task IntegrationError ()


  runFakeInbound :: InboundHandle (Trigger inbound)
  default runFakeInbound ::
    (QuickCheck.Arbitrary (Trigger inbound)) =>
    InboundHandle (Trigger inbound)
  runFakeInbound = Inbound.controllableHandle
