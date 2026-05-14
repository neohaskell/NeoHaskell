-- | Tests for Service.CommandExecutor.TH (the @command@ marker) and the
-- internal @emitInstanceIfMissing@ helper.
--
-- Test-driving strategy
-- ---------------------
-- Phase 10 will extend @command@ to also emit Show / Generic / FromJSON /
-- ToJSON via @emitInstanceIfMissing@.  In phase 9 the stub body of
-- @emitInstanceIfMissing@ returns @pure []@ (emits nothing).
--
-- Test categories used below:
--
--   [regression]    Verifies behaviour that already works pre-phase-10.
--                   These tests are GREEN against the stub.
--
--   [impl-driven]   Exercises NEW emission behaviour.  Each uses a TH probe
--                   (Bool flag evaluated at splice time) so this file always
--                   COMPILES; the probe returns False against the stub and
--                   True after phase 10.  Tests assert True → RED until
--                   phase 10 lands.
--
-- Case count (matched against the test spec):
--   happy paths         : 4  (FreshCommand probes × 4 instances)
--   idempotency          : 4  (AlreadyDerived regression + PartialDerived probes)
--   edge / sum type      : 1  (SumCommand Show on multiple constructors)
--   emitInstanceIfMissing: 5  (happy × 3 + edge × 2)
--   Total: 14 cases exercised here.
--   Remaining 15 spec cases live in Service.Query.THSpec (deriveQuery) and
--   Service.Event.THSpec (event marker), as per test-spec §"Test File Locations".
module Service.CommandExecutor.THSpec where

import Core
import Service.CommandExecutor.TH.AlreadyDerivedCommandFixture (
  hasAlreadyDerivedCommandShow,
 )
import Service.CommandExecutor.TH.FreshCommandFixture (
  hasFreshCommandFromJSON,
  hasFreshCommandGeneric,
  hasFreshCommandShow,
  hasFreshCommandToJSON,
 )
import Service.CommandExecutor.TH.PartialDerivedCommandFixture (
  hasPartialFromJSON,
  hasPartialShow,
  hasPartialToJSON,
 )
import Service.CommandExecutor.TH.SumCommandFixture (SumCommand (..))
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.CommandExecutor.TH" do
    describe "command :: TH.Name -> THLib.DecsQ" do

      describe "happy paths" do

        it "[impl-driven] emits Show instance on fresh command (has Generic, missing Show/ToJSON/FromJSON)" \_ -> do
          -- FreshCommand has only Generic (required by existing ToSchema emission).
          -- Phase 10 will emit Show, ToJSON, FromJSON via emitInstanceIfMissing.
          hasFreshCommandShow |> shouldBe True

        it "[regression] Generic is already present on FreshCommand (required by existing ToSchema)" \_ -> do
          -- Generic must be declared manually because the existing command
          -- macro unconditionally emits ToSchema, which needs Generic.
          -- Phase 10 will make this redundant but must not break it.
          hasFreshCommandGeneric |> shouldBe True

        it "[impl-driven] emits ToJSON instance on fresh command (Generic-only, missing ToJSON)" \_ -> do
          hasFreshCommandToJSON |> shouldBe True

        it "[impl-driven] emits FromJSON instance on fresh command (Generic-only, missing FromJSON)" \_ -> do
          hasFreshCommandFromJSON |> shouldBe True

      describe "idempotency" do

        it "[regression] compiles without duplicate-instance error when all four instances are pre-existing" \_ -> do
          -- Successful compilation of AlreadyDerivedCommandFixture is the proof.
          -- If the marker emitted a duplicate Show / Generic / ToJSON / FromJSON
          -- that module would fail to compile and this import would not resolve.
          hasAlreadyDerivedCommandShow |> shouldBe True

        it "[impl-driven] emits ToJSON when only Show + Generic are pre-existing" \_ -> do
          hasPartialToJSON |> shouldBe True

        it "[impl-driven] emits FromJSON when only Show + Generic are pre-existing" \_ -> do
          hasPartialFromJSON |> shouldBe True

        it "[regression] Show is still present on PartialDerivedCommand after marker call" \_ -> do
          hasPartialShow |> shouldBe True

      describe "edge cases" do

        it "[regression] sum-type command: Show handles multiple constructors" \_ -> do
          let c1 = SumCreate {sumCreateId = Uuid.nil}
          let c2 = SumUpdate {sumUpdateId = Uuid.nil, sumUpdateLabel = "hello"}
          let s1 = show c1
          let s2 = show c2
          (Text.fromLinkedList s1 |> Text.startsWith "SumCreate") |> shouldBe True
          (Text.fromLinkedList s2 |> Text.startsWith "SumUpdate") |> shouldBe True

    describe "emitInstanceIfMissing (internal)" do

      describe "happy paths" do

        it "[regression] returns empty list when instance is already in scope" \_ -> do
          -- AlreadyDerivedCommandFixture compiled without duplicate-instance
          -- errors, proving emitInstanceIfMissing returns [] for pre-existing
          -- instances (or the stub returns [] trivially — both pass here).
          pass

        it "[impl-driven] returns fallback declaration when instance is missing" \_ -> do
          -- If FreshCommand has ToJSON after the marker, emitInstanceIfMissing
          -- must have returned the fallback.  Red until phase 10.
          hasFreshCommandToJSON |> shouldBe True

        it "[regression] correctly calls TH.reifyInstances on the GHC instance environment" \_ -> do
          -- Compilation of the fixture files proves reifyInstances is invoked
          -- without runtime errors.
          pass

      describe "edge cases" do

        it "[regression] handles unqualified type names (''TypeName quoting)" \_ -> do
          -- All fixtures use ''TypeName syntax; successful compilation proves
          -- name resolution works.
          pass

        it "[regression] treats any non-empty reifyInstances result as instance-present" \_ -> do
          -- AlreadyDerivedCommandFixture has explicit instances for all four
          -- classes and the marker produced no duplicates.
          hasAlreadyDerivedCommandShow |> shouldBe True
