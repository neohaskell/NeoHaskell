module Service.ApplicationSpec where

import Core
import Service.Application qualified as Application
import Service.Event.EntityName (EntityName (..))
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Task qualified
import Test


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Service.Application" do
    describe "new" do
      it "creates an empty application" \_ -> do
        let app = Application.new
        Application.isEmpty app |> shouldBe True

    describe "withQueryRegistry" do
      it "adds a query registry to the application" \_ -> do
        -- Create a registry with one updater
        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater
        let app =
              Application.new
                |> Application.withQueryRegistry registry
        Application.hasQueryRegistry app |> shouldBe True

      it "replaces existing registry" \_ -> do
        let updater1 =
              QueryUpdater
                { queryName = "Query1",
                  updateQuery = \_ -> Task.yield unit
                }
        let updater2 =
              QueryUpdater
                { queryName = "Query2",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry1 =
              Registry.empty
                |> Registry.register (EntityName "Entity1") updater1
        let registry2 =
              Registry.empty
                |> Registry.register (EntityName "Entity2") updater2

        let app =
              Application.new
                |> Application.withQueryRegistry registry1
                |> Application.withQueryRegistry registry2

        -- Should have registry2, not registry1
        Application.hasQueryRegistry app |> shouldBe True

    describe "isEmpty" do
      it "returns True for new application" \_ -> do
        Application.isEmpty Application.new |> shouldBe True

      it "returns False after adding registry" \_ -> do
        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater
        let app =
              Application.new
                |> Application.withQueryRegistry registry
        Application.isEmpty app |> shouldBe False

    describe "hasQueryRegistry" do
      it "returns False for empty application" \_ -> do
        Application.hasQueryRegistry Application.new |> shouldBe False

      it "returns True when registry has updaters" \_ -> do
        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater
        let app =
              Application.new
                |> Application.withQueryRegistry registry
        Application.hasQueryRegistry app |> shouldBe True
