module Service.Query.DefinitionSpec where

import ConcurrentVar qualified
import Core  -- HOOK-ALLOW: Core is the NeoHaskell prelude, imported unqualified by every module (re-exports Query, NameOf, EntitiesOf)
import Json qualified
import Service.EventStore.InMemory qualified as EventStoreInMemory
import Service.Query.Definition (QueryDefinition (..), createDefinitionWithStore)
import Service.QueryObjectStore.Core (QueryObjectStore)
import Service.QueryObjectStore.InMemory qualified as QOSInMemory
import Task qualified
import Test  -- HOOK-ALLOW: Test is the spec DSL, imported unqualified by every spec module


-- | A minimal zero-entity query fixture.
--
-- It exists only to exercise the generic wiring in 'createDefinitionWithStore'.
-- With @EntitiesOf = '[]@ the entity machinery (fetchers, updaters, event
-- store) is the trivial 'WireEntities' base case, so the wiring test needs no
-- Postgres and no derived entities — just enough of a 'Query' to type-resolve.
data StoreWiringQuery = StoreWiringQuery
  { probe :: Int
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON StoreWiringQuery


instance Json.FromJSON StoreWiringQuery


instance ToSchema StoreWiringQuery


type instance NameOf StoreWiringQuery = "store-wiring-probe"


type instance EntitiesOf StoreWiringQuery = '[]


instance Query StoreWiringQuery where
  canAccessImpl _ = Nothing
  canViewImpl _ _ = Nothing


-- | Wire the fixture query through 'createDefinitionWithStore' with a spy store
-- factory.
--
-- Until #734 is fixed 'createDefinitionWithStore' takes an already-built store
-- (@Task Text (QueryObjectStore query)@) and cannot thread the query name, so we
-- model the buggy reality by pre-feeding the "__trait__" sentinel the Postgres
-- trait actually used. The fix changes the factory to @Text -> Task ...@ and
-- 'createDefinitionWithStore' supplies the real query name; this helper then
-- becomes @createDefinitionWithStore \@StoreWiringQuery spy@ and the assertion
-- goes green. Only this helper flips red->green — the test body does not.
wireWithSpy :: (Text -> Task Text (QueryObjectStore StoreWiringQuery)) -> QueryDefinition
wireWithSpy spy = createDefinitionWithStore @StoreWiringQuery (spy "__trait__")


spec :: Spec Unit
spec = do
  describe "createDefinitionWithStore" do
    it "passes NameOf query to the supplied store factory (#734)" \_ -> do
      -- The automatic wiring must hand the query's OWN name to the store factory
      -- (so each query's rows are keyed by it), not the shared "__trait__"
      -- sentinel. This pins the application wiring itself — the store-level
      -- PostgresSpec isolation tests only prove that manually named stores do
      -- not collide, which does not exercise how the name reaches the store.
      captured <- ConcurrentVar.containing ""
      let spyFactory queryName = do
            captured |> ConcurrentVar.modify (\_ -> queryName)
            QOSInMemory.new |> Task.mapError toText
      let definition = wireWithSpy spyFactory
      eventStore <- EventStoreInMemory.new |> Task.mapError toText
      _ <-
        definition.wireQuery eventStore
          |> Task.asResult
      capturedName <- captured |> ConcurrentVar.peek
      let expectedName = definition.queryName
      if capturedName == expectedName && capturedName != "__trait__" && capturedName != ""
        then pass
        else fail [fmt|store factory received "#{capturedName}"; expected the query's own name "#{expectedName}" (not the "__trait__" sentinel)|]
