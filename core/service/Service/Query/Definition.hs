{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Query.Definition (
  QueryDefinition (..),
  createDefinition,
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
import Service.Query.Core (Query, QueryOf)
import Service.Query.Endpoint qualified as Endpoint
import Service.Query.Registry (QueryRegistry)
import Service.Query.Registry qualified as Registry
import Service.Query.Updater qualified as Updater
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


-- | Create a QueryDefinition for a query with a single entity source.
--
-- This is the simplest case where a query is derived from one entity type.
-- The query name is derived from 'NameOf query' (kebab-case).
--
-- Example:
--
-- @
-- cartSummaryDef = createDefinition \@CartEntity \@CartSummary
-- @
createDefinition ::
  forall entity query entityName queryName event.
  ( Entity entity,
    Query query,
    QueryOf entity query,
    event ~ EventOf entity,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.ToJSON query,
    Json.FromJSON query,
    entityName ~ NameOf entity,
    queryName ~ NameOf query,
    GHC.KnownSymbol entityName,
    GHC.KnownSymbol queryName
  ) =>
  QueryDefinition
createDefinition = do
  let queryNameText =
        GHC.symbolVal (Record.Proxy @queryName)
          |> Text.fromLinkedList
  let entityNameText =
        GHC.symbolVal (Record.Proxy @entityName)
          |> Text.fromLinkedList

  QueryDefinition
    { queryName = queryNameText,
      wireQuery = \rawEventStore -> do
        -- 1. Create QueryObjectStore for this query type
        queryStore <- InMemory.new @query |> Task.mapError toText

        -- 2. Create typed EventStore for this entity's event type
        let typedEventStore = rawEventStore |> EventStore.castEventStore @event

        -- 3. Create EntityFetcher for this entity type
        entityFetcher <-
          EntityFetcher.new
            typedEventStore
            (initialStateImpl @entity)
            (updateImpl @entity)
            |> Task.mapError toText

        -- 4. Create QueryUpdater for entity -> query
        let updater =
              Updater.createUpdater @entity @query
                queryNameText
                entityFetcher
                queryStore

        -- 5. Create QueryRegistry with this updater
        let registry =
              Registry.empty
                |> Registry.register (EntityName entityNameText) updater

        -- 6. Create endpoint handler
        let endpoint = Endpoint.createQueryEndpoint queryStore

        Task.yield (registry, (queryNameText, endpoint))
    }
