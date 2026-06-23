module Service.EventStore.Postgres.PoolBudgetSpec (spec) where

-- | Pure unit spec for the ADR-0060 Postgres connection-pool budget.
--
-- This spec runs unconditionally (no Postgres). It guards the configurable,
-- defaulted pool-size fields on the Postgres config records and the documented
-- Azure Flexible Server B1ms aggregate budget. See ADR-0060.
--
-- The pool sizes are now defaulted fields on the config records (default 6 on
-- 'PostgresEventStore', shared by the EventStore and FileUpload pools; default
-- 4 on 'PostgresQueryObjectStoreConfig'), supplied via their 'Default'
-- instances. These assertions pin the defaults, confirm overrides are
-- respected, and encode the B1ms budget arithmetic so a future default bump
-- that breaks it fails the suite rather than production.

import Core
import Service.EventStore.Postgres.Internal (PostgresEventStore (..))
import Service.EventStore.Postgres.Internal qualified as EventStore
import Service.QueryObjectStore.Postgres (PostgresQueryObjectStoreConfig (..))
import Service.QueryObjectStore.Postgres qualified as QueryObjectStore
import Task qualified
import Test.Hspec qualified as Hspec
import Text qualified


-- | The EventStore pool's default size (also used by the FileUpload pool,
-- which shares the EventStore config's 'poolSize').
eventStorePoolSize :: Int
eventStorePoolSize = (def :: PostgresEventStore).poolSize


-- | The FileUpload pool size: it shares the EventStore config's 'poolSize'
-- field, so the default is identical to the EventStore default.
fileUploadPoolSize :: Int
fileUploadPoolSize = (def :: PostgresEventStore).poolSize


-- | The QueryObjectStore pool's default size.
queryObjectStorePoolSize :: Int
queryObjectStorePoolSize = (def :: PostgresQueryObjectStoreConfig).poolSize


-- | Azure Flexible Server B1ms usable-connection ceiling (~35).
b1msUsableCeiling :: Int
b1msUsableCeiling = 35


-- | Headroom reserved for an operator @psql@ session + a maintenance task.
adminHeadroom :: Int
adminHeadroom = 2


-- | Fixed unpooled connections: listener pair (2) + init-listen (1).
fixedUnpooledConnections :: Int
fixedUnpooledConnections = 3


-- | The documented worst-case fixed connection demand: the three pools plus
-- the fixed unpooled connections (per-stream subscriptions, term N, excluded).
--
-- FileUpload shares the EventStore config's pool, so its contribution to the
-- worst-case budget is the EventStore default size again (6 + 4 + 6 + 3 = 19).
fixedBudget :: Int
fixedBudget =
  eventStorePoolSize
    + queryObjectStorePoolSize
    + fileUploadPoolSize
    + fixedUnpooledConnections


spec :: Hspec.Spec
spec = Hspec.describe "Service.EventStore.Postgres.PoolBudget" do
  Hspec.describe "PostgresEventStore.poolSize (default via Default instance)" do
    Hspec.it "is the ADR-0060 B1ms default of 6" do
      (def :: PostgresEventStore).poolSize |> Hspec.shouldBe 6
    Hspec.it "is strictly positive" do
      ((def :: PostgresEventStore).poolSize > 0) |> Hspec.shouldBe True
    Hspec.it "does not on its own exceed the B1ms usable ceiling" do
      ((def :: PostgresEventStore).poolSize <= b1msUsableCeiling) |> Hspec.shouldBe True
    Hspec.it "respects an explicit override" do
      ((def :: PostgresEventStore) {EventStore.poolSize = 10}).poolSize |> Hspec.shouldBe 10

  Hspec.describe "PostgresQueryObjectStoreConfig.poolSize (default via Default instance)" do
    Hspec.it "is the ADR-0060 B1ms default of 4" do
      (def :: PostgresQueryObjectStoreConfig).poolSize |> Hspec.shouldBe 4
    Hspec.it "is strictly positive" do
      ((def :: PostgresQueryObjectStoreConfig).poolSize > 0) |> Hspec.shouldBe True
    Hspec.it "is no larger than the EventStore pool default" do
      ((def :: PostgresQueryObjectStoreConfig).poolSize <= eventStorePoolSize) |> Hspec.shouldBe True
    Hspec.it "respects an explicit override" do
      ((def :: PostgresQueryObjectStoreConfig) {QueryObjectStore.poolSize = 8}).poolSize |> Hspec.shouldBe 8

  Hspec.describe "FileUpload pool (shares the EventStore config's poolSize)" do
    Hspec.it "uses the EventStore config's poolSize default of 6" do
      fileUploadPoolSize |> Hspec.shouldBe 6
    Hspec.it "tracks an EventStore poolSize override (shared field)" do
      ((def :: PostgresEventStore) {EventStore.poolSize = 12}).poolSize |> Hspec.shouldBe 12

  Hspec.describe "B1ms aggregate budget invariant" do
    Hspec.it "fixed budget equals the documented 19 connections" do
      fixedBudget |> Hspec.shouldBe 19
    Hspec.it "fixed budget fits under the B1ms usable ceiling with admin headroom" do
      (fixedBudget <= b1msUsableCeiling - adminHeadroom) |> Hspec.shouldBe True
    Hspec.it "leaves at least one slot of per-stream subscription margin" do
      ((b1msUsableCeiling - adminHeadroom) - fixedBudget >= 1) |> Hspec.shouldBe True
    Hspec.it "would fail the ceiling gate if a pool were bumped past the margin" do
      -- Negative control: a hypothetical +20 bump must break the gate,
      -- pinning the gate's direction without mutating the real defaults.
      ((fixedBudget + 20 <= b1msUsableCeiling - adminHeadroom)) |> Hspec.shouldBe False

  Hspec.describe "fail-fast poolSize validation (EventStore acquire)" do
    Hspec.it "rejects poolSize = 0 with a clear error" do
      result <-
        EventStore.defaultOps.acquire ((def :: PostgresEventStore) {EventStore.poolSize = 0})
          |> Task.runResult
      case result of
        Err err -> (err |> Text.contains "poolSize must be > 0") |> Hspec.shouldBe True
        Ok _ -> Hspec.expectationFailure "expected poolSize=0 to be rejected"
    Hspec.it "rejects a negative poolSize with a clear error" do
      result <-
        EventStore.defaultOps.acquire ((def :: PostgresEventStore) {EventStore.poolSize = -1})
          |> Task.runResult
      case result of
        Err err -> (err |> Text.contains "poolSize must be > 0") |> Hspec.shouldBe True
        Ok _ -> Hspec.expectationFailure "expected negative poolSize to be rejected"
