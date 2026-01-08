module Service.Query.RegistrySpec where

import Array qualified
import Core
import Service.Event.EntityName (EntityName (..))
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "QueryRegistry" do
    describe "empty" do
      it "creates an empty registry" \_ -> do
        let registry = Registry.empty
        let entityName = EntityName "TestEntity"
        let updaters = Registry.getUpdatersForEntity entityName registry
        updaters |> Array.length |> shouldBe 0

    describe "register" do
      it "registers a single updater for an entity" \_ -> do
        let entityName = EntityName "TestEntity"
        let updater = makeTestUpdater "query1"
        let registry =
              Registry.empty
                |> Registry.register entityName updater

        let updaters = Registry.getUpdatersForEntity entityName registry
        updaters |> Array.length |> shouldBe 1

      it "registers multiple updaters for the same entity" \_ -> do
        let entityName = EntityName "TestEntity"
        let updater1 = makeTestUpdater "query1"
        let updater2 = makeTestUpdater "query2"
        let registry =
              Registry.empty
                |> Registry.register entityName updater1
                |> Registry.register entityName updater2

        let updaters = Registry.getUpdatersForEntity entityName registry
        updaters |> Array.length |> shouldBe 2

      it "registers updaters for different entities independently" \_ -> do
        let entityName1 = EntityName "Entity1"
        let entityName2 = EntityName "Entity2"
        let updater1 = makeTestUpdater "query1"
        let updater2 = makeTestUpdater "query2"
        let registry =
              Registry.empty
                |> Registry.register entityName1 updater1
                |> Registry.register entityName2 updater2

        let updaters1 = Registry.getUpdatersForEntity entityName1 registry
        let updaters2 = Registry.getUpdatersForEntity entityName2 registry
        updaters1 |> Array.length |> shouldBe 1
        updaters2 |> Array.length |> shouldBe 1

    describe "getUpdatersForEntity" do
      it "returns empty array for unregistered entity" \_ -> do
        let entityName = EntityName "UnknownEntity"
        let registry = Registry.empty
        let updaters = Registry.getUpdatersForEntity entityName registry
        updaters |> Array.length |> shouldBe 0

      it "returns updaters in registration order" \_ -> do
        let entityName = EntityName "TestEntity"
        let updater1 = makeTestUpdater "first"
        let updater2 = makeTestUpdater "second"
        let updater3 = makeTestUpdater "third"
        let registry =
              Registry.empty
                |> Registry.register entityName updater1
                |> Registry.register entityName updater2
                |> Registry.register entityName updater3

        let updaters = Registry.getUpdatersForEntity entityName registry
        updaters |> Array.length |> shouldBe 3

        case Array.get 0 updaters of
          Just u -> u.queryName |> shouldBe "first"
          Nothing -> fail "Expected updater at index 0"

        case Array.get 1 updaters of
          Just u -> u.queryName |> shouldBe "second"
          Nothing -> fail "Expected updater at index 1"

        case Array.get 2 updaters of
          Just u -> u.queryName |> shouldBe "third"
          Nothing -> fail "Expected updater at index 2"


-- | Helper to create a test QueryUpdater that does nothing.
makeTestUpdater :: Text -> QueryUpdater
makeTestUpdater name =
  QueryUpdater
    { queryName = name,
      updateQuery = \_ -> Task.yield unit
    }
