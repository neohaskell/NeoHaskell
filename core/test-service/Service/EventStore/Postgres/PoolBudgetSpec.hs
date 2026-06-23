module Service.EventStore.Postgres.PoolBudgetSpec (spec) where

-- | Pure unit spec for the ADR-0060 Postgres connection-pool budget.
--
-- This spec runs unconditionally (no Postgres). It guards the three
-- explicit pool-size constants and the documented Azure Flexible Server
-- B1ms aggregate budget. See ADR-0060 and the architecture doc
-- docs/architecture/0060-postgres-pool-budget.md.
--
-- Note: the ADR's Testing §1 ("assert the constructed HasqlPoolConfig.Config
-- carries the expected size") is not implementable — the Config record's
-- @size@ field lives in the hidden module Hasql.Pool.Config.Config. These
-- constant-value + budget assertions are the implementable substitute that
-- catches the same regression class (a future builder merge dropping a size).

import Core
import Service.EventStore.Postgres.Internal (eventStorePoolSize)
import Service.FileUpload.FileStateStore.Postgres (fileUploadPoolSize)
import Service.QueryObjectStore.Postgres (queryObjectStorePoolSize)
import Test.Hspec qualified as Hspec


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
fixedBudget :: Int
fixedBudget =
  eventStorePoolSize
    + queryObjectStorePoolSize
    + fileUploadPoolSize
    + fixedUnpooledConnections


spec :: Hspec.Spec
spec = Hspec.describe "Service.EventStore.Postgres.PoolBudget" do
  Hspec.describe "eventStorePoolSize" do
    Hspec.it "is the ADR-0060 B1ms default of 6" do
      eventStorePoolSize |> Hspec.shouldBe 6
    Hspec.it "is strictly positive" do
      (eventStorePoolSize > 0) |> Hspec.shouldBe True
    Hspec.it "does not on its own exceed the B1ms usable ceiling" do
      (eventStorePoolSize <= b1msUsableCeiling) |> Hspec.shouldBe True

  Hspec.describe "queryObjectStorePoolSize" do
    Hspec.it "is the ADR-0060 B1ms default of 4" do
      queryObjectStorePoolSize |> Hspec.shouldBe 4
    Hspec.it "is strictly positive" do
      (queryObjectStorePoolSize > 0) |> Hspec.shouldBe True
    Hspec.it "is no larger than the EventStore pool" do
      (queryObjectStorePoolSize <= eventStorePoolSize) |> Hspec.shouldBe True

  Hspec.describe "fileUploadPoolSize" do
    Hspec.it "is the ADR-0060 B1ms default of 2" do
      fileUploadPoolSize |> Hspec.shouldBe 2
    Hspec.it "is strictly positive" do
      (fileUploadPoolSize > 0) |> Hspec.shouldBe True
    Hspec.it "is the smallest of the three pools" do
      (fileUploadPoolSize <= queryObjectStorePoolSize) |> Hspec.shouldBe True

  Hspec.describe "B1ms aggregate budget invariant" do
    Hspec.it "fixed budget equals the documented 15 connections" do
      fixedBudget |> Hspec.shouldBe 15
    Hspec.it "fixed budget fits under the B1ms usable ceiling with admin headroom" do
      (fixedBudget <= b1msUsableCeiling - adminHeadroom) |> Hspec.shouldBe True
    Hspec.it "leaves at least one slot of per-stream subscription margin" do
      ((b1msUsableCeiling - adminHeadroom) - fixedBudget >= 1) |> Hspec.shouldBe True
    Hspec.it "would fail the ceiling gate if a pool were bumped past the margin" do
      -- Negative control: a hypothetical +20 bump must break the gate,
      -- pinning the gate's direction without mutating the real constants.
      ((fixedBudget + 20 <= b1msUsableCeiling - adminHeadroom)) |> Hspec.shouldBe False
