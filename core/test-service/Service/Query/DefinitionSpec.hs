module Service.Query.DefinitionSpec where

import ConcurrentVar qualified
import Core
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Service.EventStore.InMemory qualified as EventStoreInMemory
import Service.Query.Definition (QueryDefinition (..), createDefinitionWithStore)
import Service.Query.TH (deriveQuery)
import Service.QueryObjectStore.Core (QueryObjectStore)
import Service.QueryObjectStore.InMemory qualified as QOSInMemory
import Task qualified
import Test


-- | A minimal zero-entity query fixture, derived the canonical way.
--
-- It exists only to exercise the generic wiring in 'createDefinitionWithStore'.
-- Per the neohaskell-concept-derivation skill the type carries NO hand-written
-- boilerplate: 'deriveQuery' emits Show, Generic, the JSON instances, ToSchema,
-- @NameOf = "StoreWiringQuery"@, @EntitiesOf = '[]@ (zero entities → the trivial
-- 'WireEntities' base case, so no Postgres or entity fixtures are needed),
-- 'Query', and 'KnownHash'. Only the required 'canAccess'/'canView' companion
-- functions are authored here.
data StoreWiringQuery = StoreWiringQuery
  { probe :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.publicAccess


canView :: Maybe UserClaims -> StoreWiringQuery -> Maybe AccessError
canView = AccessControl.publicView


deriveQuery ''StoreWiringQuery []


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
      definition.wireQuery eventStore
        |> Task.asResult
        |> discard
      capturedName <- captured |> ConcurrentVar.peek
      let expectedName = definition.queryName
      Task.unless (capturedName == expectedName && capturedName != "__trait__" && capturedName != "") do
        fail [fmt|store factory received "#{capturedName}"; expected the query's own name "#{expectedName}" (not the "__trait__" sentinel)|]
