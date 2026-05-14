{-# LANGUAGE TemplateHaskell #-}

-- | Tests for Service.Event.TH (the @event@ marker).
--
-- Test-driving strategy
-- ---------------------
-- Phase 10 will implement @event@ to emit Show / Generic / FromJSON / ToJSON
-- via @emitInstanceIfMissing@.  In phase 9 the stub body emits nothing
-- (@pure []@).
--
-- Tests tagged [impl-driven] are RED against the stub (probe Bool is False,
-- test asserts True).  Tests tagged [regression] are GREEN because they
-- verify already-working behavior or compile-time safety.
--
-- Case count:
--   happy paths (fresh type probes)   : 4
--   happy path (complex fields)       : 1  (probe)
--   idempotency regression (all in)   : 1  (compile-success)
--   idempotency impl-driven (partial) : 2  (probes)
--   idempotency regression (partial)  : 1  (Show pre-existing probe)
--   empty event type                  : 1  (probe)
--   sum type Show                     : 1  (Show on multiple ctors)
--   idempotency across files           : 1  (regression compile-success)
--   Total: 12 cases in this file.
module Service.Event.THSpec where

import Core hiding (event)
import Json qualified
import Language.Haskell.TH.Syntax qualified as TH
import Service.Event.TH (event)
import Test
import Text qualified
import Uuid qualified


-- ============================================================================
-- Fresh event type: no pre-existing Show / Generic / FromJSON / ToJSON
-- ============================================================================

data FreshEvent = FreshEvent {evId :: Uuid, evLabel :: Text}


event ''FreshEvent


$(do
    is <- TH.reifyInstances ''Show [TH.ConT ''FreshEvent]
    case is of
      [] -> [d| hasFreshEventShow :: Bool; hasFreshEventShow = False |]
      _ : _ -> [d| hasFreshEventShow :: Bool; hasFreshEventShow = True |])


$(do
    is <- TH.reifyInstances ''Generic [TH.ConT ''FreshEvent]
    case is of
      [] -> [d| hasFreshEventGeneric :: Bool; hasFreshEventGeneric = False |]
      _ : _ -> [d| hasFreshEventGeneric :: Bool; hasFreshEventGeneric = True |])


$(do
    is <- TH.reifyInstances ''Json.ToJSON [TH.ConT ''FreshEvent]
    case is of
      [] -> [d| hasFreshEventToJSON :: Bool; hasFreshEventToJSON = False |]
      _ : _ -> [d| hasFreshEventToJSON :: Bool; hasFreshEventToJSON = True |])


$(do
    is <- TH.reifyInstances ''Json.FromJSON [TH.ConT ''FreshEvent]
    case is of
      [] -> [d| hasFreshEventFromJSON :: Bool; hasFreshEventFromJSON = False |]
      _ : _ -> [d| hasFreshEventFromJSON :: Bool; hasFreshEventFromJSON = True |])


-- ============================================================================
-- Complex event type: Uuid, Maybe Text, Array fields
-- ============================================================================

data ComplexEvent = ComplexEvent
  { ceId :: Uuid,
    ceMetadata :: Maybe Text,
    ceItems :: Array Uuid
  }


event ''ComplexEvent


$(do
    is <- TH.reifyInstances ''Json.ToJSON [TH.ConT ''ComplexEvent]
    case is of
      [] -> [d| hasComplexEventToJSON :: Bool; hasComplexEventToJSON = False |]
      _ : _ -> [d| hasComplexEventToJSON :: Bool; hasComplexEventToJSON = True |])


-- ============================================================================
-- Already-derived event type: all four instances in scope before event ''X
-- ============================================================================

data AlreadyDerivedEvent = AlreadyDerivedEvent {adeId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON AlreadyDerivedEvent
instance Json.FromJSON AlreadyDerivedEvent


-- [regression] Successful compilation proves idempotency (no duplicate error).
event ''AlreadyDerivedEvent


-- ============================================================================
-- Partially-derived event type: only Generic in scope, missing the others
-- ============================================================================

data PartialEvent = PartialEvent {peId :: Uuid}
  deriving (Generic)


event ''PartialEvent


$(do
    is <- TH.reifyInstances ''Json.ToJSON [TH.ConT ''PartialEvent]
    case is of
      [] -> [d| hasPartialEventToJSON :: Bool; hasPartialEventToJSON = False |]
      _ : _ -> [d| hasPartialEventToJSON :: Bool; hasPartialEventToJSON = True |])


$(do
    is <- TH.reifyInstances ''Show [TH.ConT ''PartialEvent]
    case is of
      [] -> [d| hasPartialEventShow :: Bool; hasPartialEventShow = False |]
      _ : _ -> [d| hasPartialEventShow :: Bool; hasPartialEventShow = True |])


-- Generic was pre-existing and must still be present after the marker.
$(do
    is <- TH.reifyInstances ''Generic [TH.ConT ''PartialEvent]
    case is of
      [] -> [d| hasPartialEventGeneric :: Bool; hasPartialEventGeneric = False |]
      _ : _ -> [d| hasPartialEventGeneric :: Bool; hasPartialEventGeneric = True |])


-- ============================================================================
-- Empty (zero-field) event type
-- ============================================================================

data EmptyEvent = EmptyEvent


event ''EmptyEvent


$(do
    is <- TH.reifyInstances ''Show [TH.ConT ''EmptyEvent]
    case is of
      [] -> [d| hasEmptyEventShow :: Bool; hasEmptyEventShow = False |]
      _ : _ -> [d| hasEmptyEventShow :: Bool; hasEmptyEventShow = True |])


-- ============================================================================
-- Sum-type event: multiple constructors
-- ============================================================================

data SumEvent
  = Created {seId :: Uuid}
  | Updated {seId :: Uuid, seNewLabel :: Text}
  deriving (Eq, Show, Generic)


instance Json.ToJSON SumEvent
instance Json.FromJSON SumEvent


event ''SumEvent


-- ============================================================================
-- Second independent event type (proves no cross-file conflict)
-- ============================================================================

data IndependentEvent = IndependentEvent {indId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON IndependentEvent
instance Json.FromJSON IndependentEvent


-- [regression] A second event ''X in the same file compiles fine.
event ''IndependentEvent


-- ============================================================================
-- Spec
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Service.Event.TH" do
    describe "event :: TH.Name -> THLib.DecsQ" do

      describe "happy paths" do

        it "[impl-driven] emits Show instance on fresh event type with no pre-existing instances" \_ -> do
          hasFreshEventShow |> shouldBe True

        it "[impl-driven] emits Generic instance on fresh event type with no pre-existing instances" \_ -> do
          hasFreshEventGeneric |> shouldBe True

        it "[impl-driven] emits ToJSON instance on fresh event type with no pre-existing instances" \_ -> do
          hasFreshEventToJSON |> shouldBe True

        it "[impl-driven] emits FromJSON instance on fresh event type with no pre-existing instances" \_ -> do
          hasFreshEventFromJSON |> shouldBe True

        it "[impl-driven] works with event records containing Uuid, Array, and Maybe fields" \_ -> do
          hasComplexEventToJSON |> shouldBe True

      describe "idempotency" do

        it "[regression] compiles without duplicate error when all four instances are pre-existing" \_ -> do
          -- Successful compilation of this module (AlreadyDerivedEvent splice)
          -- is the assertion.
          pass

        it "[impl-driven] emits ToJSON when only Generic is pre-existing" \_ -> do
          hasPartialEventToJSON |> shouldBe True

        it "[impl-driven] emits Show when only Generic is pre-existing" \_ -> do
          hasPartialEventShow |> shouldBe True

        it "[regression] Generic is still present after marker call on partially-derived type" \_ -> do
          hasPartialEventGeneric |> shouldBe True

      describe "edge cases" do

        it "[impl-driven] empty event type (zero fields): Show instance is emitted" \_ -> do
          hasEmptyEventShow |> shouldBe True

        it "[regression] sum-type event: Show handles multiple constructors" \_ -> do
          let e1 = Created {seId = Uuid.nil}
          let e2 = Updated {seId = Uuid.nil, seNewLabel = "hello"}
          let s1 = show e1
          let s2 = show e2
          (Text.fromLinkedList s1 |> Text.startsWith "Created") |> shouldBe True
          (Text.fromLinkedList s2 |> Text.startsWith "Updated") |> shouldBe True

        it "[regression] marker is idempotent across multiple event types in same file" \_ -> do
          -- Both IndependentEvent and AlreadyDerivedEvent use event ''X in this
          -- file without conflict.  Compilation success is the assertion.
          pass
