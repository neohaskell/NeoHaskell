module Service.Entity.THSpec where

import Core
import Data.Proxy qualified as Proxy -- HOOK-ALLOW: compiler type-level reflection probe; no Core proxy wrapper.
import GHC.TypeLits qualified as TypeLits -- HOOK-ALLOW: inspect generated type-level names in this compiler-boundary test.
import Json qualified
import Service.Entity.Core qualified as Entity
import Service.Entity.TH.ConflictingFamiliesFixture qualified as Conflicts
import Service.Entity.TH.FreshFixture qualified as Fresh
import Service.Entity.TH.MissingInitialStateFixture qualified as MissingInitial
import Service.Entity.TH.MissingRoutingFixture qualified as MissingRouting
import Service.Entity.TH.MissingUpdateFixture qualified as MissingUpdate
import Service.Entity.TH.NoShowFixture qualified as NoShow
import Service.Entity.TH.PreservedFixture qualified as Preserved
import Test
import Text qualified
import Uuid qualified

spec :: Spec Unit
spec = do
  describe "Core derivation helpers" do
    it "derives an event while retaining Core.event injection" \_ -> do
      Fresh.injectedEvent.delta |> shouldBe 3
      Json.encode Fresh.injectedEvent
        |> shouldBe (Json.encode (Fresh.Changed {eventId = Uuid.nil, delta = 3}))

    it "uses the entity type name by default" \_ -> do
      TypeLits.symbolVal (Proxy.Proxy @(NameOf Fresh.Counter))
        |> Text.fromLinkedList
        |> shouldBe "Counter"

  describe "derived entity behavior" do
    it "uses the supplied initial state for both Entity and Default" \_ -> do
      (initialStateImpl @Fresh.Counter).count |> shouldBe 10
      (def @Fresh.Counter).count |> shouldBe 10
      (def @Fresh.Counter).entityId |> shouldBe Uuid.nil

    it "delegates updates and routes the event despite a local Event type" \_ -> do
      let routedId = Uuid.generateV5 Uuid.nil "entity-derivation-test"
      let change = Fresh.Changed {eventId = routedId, delta = 4}
      let initial = Fresh.Counter {entityId = Uuid.nil, count = 5}
      let updated = updateImpl change initial
      updated.count |> shouldBe 9
      updated.entityId |> shouldBe routedId
      Entity.getEventEntityIdImpl change |> shouldBe routedId

    it "roundtrips generated entity JSON without requiring entity Show" \_ -> do
      let original = Fresh.Counter {entityId = Uuid.generateV5 Uuid.nil "entity-json-test", count = 12}
      case Json.decode @Fresh.Counter (Json.encode original) of
        Err reason -> Test.fail reason
        Ok decoded -> do
          decoded.entityId |> shouldBe original.entityId
          decoded.count |> shouldBe original.count

    it "supports serializable fields that have no Show instance" \_ -> do
      NoShow.hasEntityShow |> shouldBe False
      let original = updateImpl (NoShow.QuietChanged {amount = 8}) (def @NoShow.QuietEntity)
      case Json.decode @NoShow.QuietEntity (Json.encode original) of
        Err reason -> Test.fail reason
        Ok decoded -> decoded.secret.amount |> shouldBe 8

  describe "entity derivation boundaries" do
    it "preserves custom JSON, name, default, entity behavior and identifier type" \_ -> do
      TypeLits.symbolVal (Proxy.Proxy @(NameOf Preserved.CustomEntity))
        |> Text.fromLinkedList
        |> shouldBe "custom-counter"
      (def @Preserved.CustomEntity).count |> shouldBe 77
      (initialStateImpl @Preserved.CustomEntity).count |> shouldBe 11
      let change = Preserved.CustomChanged {customId = "customer-17", delta = 2}
      let initial = Preserved.CustomEntity {count = 3}
      (updateImpl change initial).count |> shouldBe 105
      Entity.getEventEntityIdImpl change |> shouldBe "customer-17"
      Json.encode initial |> shouldBe (Json.encode (3 :: Int))
      case Json.decode @Preserved.CustomEntity (Json.encode (6 :: Int)) of
        Err reason -> Test.fail reason
        Ok decoded -> decoded.count |> shouldBe 6

    it "derives Default from an existing Entity with alias mappings and no companions" \_ -> do
      (def @Preserved.ExistingEntity).count |> shouldBe 33
      let original = Preserved.ExistingEntity {count = 14}
      case Json.decode @Preserved.ExistingEntity (Json.encode original) of
        Err reason -> Test.fail reason
        Ok decoded -> decoded.count |> shouldBe 14

    it "requires initialState when deriving a missing Entity instance" \_ -> do
      MissingInitial.rejectedMissingCompanion |> shouldBe True

    it "requires update when deriving a missing Entity instance" \_ -> do
      MissingUpdate.rejectedMissingCompanion |> shouldBe True

    it "requires getEventEntityId when deriving missing event routing" \_ -> do
      MissingRouting.rejectedMissingCompanion |> shouldBe True

    it "rejects an EventOf equation that names another event type" \_ -> do
      Conflicts.rejectedEventOfConflict |> shouldBe True

    it "rejects an EntityOf equation that names another entity" \_ -> do
      Conflicts.rejectedEntityOfConflict |> shouldBe True
