{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.Query.Definition (
  QueryDefinition (..),
  createDefinition,
  WireEntities (..),
) where

import Basics
import GHC.TypeLits qualified as GHC
import Json qualified
import Record qualified
import Service.Command.Core (Entity (..), EventOf, NameOf)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Service.Query.Core (EntitiesOf, Query, QueryOf)
import Service.Query.Endpoint qualified as Endpoint
import Service.Query.Registry (QueryRegistry)
import Service.Query.Registry qualified as Registry
import Service.Query.Updater qualified as Updater
import Service.QueryObjectStore.Core (QueryObjectStore)
import Service.QueryObjectStore.InMemory qualified as InMemory
import Service.Transport (QueryEndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | Captures how to wire a query type at runtime.
--
-- A QueryDefinition contains all the information needed to:
-- 1. Create a QueryObjectStore for the query
-- 2. Create EntityFetchers and QueryUpdaters for each contributing entity
-- 3. Register updaters in the QueryRegistry
-- 4. Create an HTTP endpoint handler
--
-- QueryDefinitions are created declaratively via 'createDefinition' and
-- evaluated at runtime by 'Application.runWith'.
data QueryDefinition = QueryDefinition
  { queryName :: Text,
    -- | Given an EventStore, creates the query infrastructure and returns:
    -- - QueryRegistry entries for this query's entities
    -- - The query endpoint name and handler
    wireQuery ::
      EventStore Json.Value ->
      Task Text (QueryRegistry, (Text, QueryEndpointHandler))
  }


-- | Create a QueryDefinition for a query type.
--
-- This automatically wires all entities listed in 'EntitiesOf query'.
-- The query name is derived from 'NameOf query' (kebab-case).
--
-- Example:
--
-- @
-- -- For a query with one entity:
-- cartSummaryDef = createDefinition \@CartSummary
--
-- -- For a query with multiple entities:
-- userOrdersDef = createDefinition \@UserOrders
-- @
createDefinition ::
  forall query queryName entities.
  ( Query query,
    Json.ToJSON query,
    Json.FromJSON query,
    queryName ~ NameOf query,
    entities ~ EntitiesOf query,
    GHC.KnownSymbol queryName,
    WireEntities entities query
  ) =>
  QueryDefinition
createDefinition = do
  let queryNameText =
        GHC.symbolVal (Record.Proxy @queryName)
          |> Text.fromLinkedList

  QueryDefinition
    { queryName = queryNameText,
      wireQuery = \rawEventStore -> do
        -- 1. Create QueryObjectStore for this query type
        queryStore <- InMemory.new @query |> Task.mapError toText

        -- 2. Wire all entities and collect their registries
        registry <- wireEntities @entities @query queryNameText rawEventStore queryStore

        -- 3. Create endpoint handler
        let endpoint = Endpoint.createQueryEndpoint queryStore

        Task.yield (registry, (queryNameText, endpoint))
    }


-- | Typeclass for wiring a list of entity types to a query.
--
-- This recurses over the type-level list 'EntitiesOf query' and for each
-- entity creates an EntityFetcher, QueryUpdater, and registry entry.
class WireEntities (entities :: [Type]) (query :: Type) where
  wireEntities ::
    Text ->
    EventStore Json.Value ->
    QueryObjectStore query ->
    Task Text QueryRegistry


-- | Base case: empty entity list produces empty registry.
instance WireEntities '[] query where
  wireEntities _ _ _ = Task.yield Registry.empty


-- | Recursive case: wire the head entity, then combine with tail.
instance
  ( Entity entity,
    Query query,
    QueryOf entity query,
    Json.FromJSON (EventOf entity),
    Json.ToJSON (EventOf entity),
    GHC.KnownSymbol (NameOf entity),
    WireEntities rest query
  ) =>
  WireEntities (entity ': rest) query
  where
  wireEntities queryNameText rawEventStore queryStore = do
    -- Wire this entity
    let entityNameText =
          GHC.symbolVal (Record.Proxy @(NameOf entity))
            |> Text.fromLinkedList

    -- Create typed EventStore for this entity's event type
    let typedEventStore = rawEventStore |> EventStore.castEventStore @(EventOf entity)

    -- Create EntityFetcher for this entity type
    entityFetcher <-
      EntityFetcher.new
        typedEventStore
        (initialStateImpl @entity)
        (updateImpl @entity)
        |> Task.mapError toText

    -- Create QueryUpdater for entity -> query
    let updater =
          Updater.createUpdater @entity @query
            queryNameText
            entityFetcher
            queryStore

    -- Create registry entry for this entity
    let thisRegistry =
          Registry.empty
            |> Registry.register (EntityName entityNameText) updater

    -- Recursively wire remaining entities
    restRegistry <- wireEntities @rest @query queryNameText rawEventStore queryStore

    -- Combine registries
    Task.yield (thisRegistry |> Registry.mergeInto restRegistry)
