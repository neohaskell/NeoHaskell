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
-- factory that captures the name it is handed.
--
-- Since #734 / ADR-0070 'createDefinitionWithStore' hands the store factory the
-- query's real name (@NameOf query@), so the spy records @"StoreWiringQuery"@,
-- not the old @"__trait__"@ sentinel. The wiring test (C4) asserts exactly that;
-- only this helper flipped red->green — the test body did not.
wireWithSpy :: (Text -> Task Text (QueryObjectStore StoreWiringQuery)) -> QueryDefinition
wireWithSpy spy = createDefinitionWithStore @StoreWiringQuery spy


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
