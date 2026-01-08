{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Query.Updater (
  createUpdater,
) where

import Basics
import Json qualified
import Maybe (Maybe (..))
import Service.Entity.Core (Entity (..), EventOf)
import Service.EntityFetcher.Core (EntityFetcher (..), EntityFetchResult (..), FetchedEntity (..))
import Service.Event (Event (..))
import Service.Query.Core (Query, QueryAction (..), QueryOf (..))
import Service.Query.Registry (QueryUpdater (..))
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Task qualified
import Text (Text)
import ToText (toText)


-- | Create a QueryUpdater for a specific entity-query relationship.
--
-- The updater handles the complete flow of updating a query when an entity event occurs:
--
-- 1. Receives a raw JSON event from the event store
-- 2. Fetches the current entity state using the provided EntityFetcher
-- 3. Calls the 'combine' function from 'QueryOf' to determine the QueryAction
-- 4. Applies the action (Update, Delete, or NoOp) to the QueryObjectStore
--
-- Example usage:
--
-- @
-- -- Create an updater for User -> UserOrders relationship
-- userUpdater <- Query.Updater.createUpdater \@User \@UserOrders
--   "UserOrders"
--   userEntityFetcher
--   userOrdersStore
--
-- -- Register it in the QueryRegistry
-- let registry = Registry.register userEntityName userUpdater Registry.empty
-- @
createUpdater ::
  forall entity query.
  ( Entity entity,
    Query query,
    QueryOf entity query,
    Json.FromJSON (EventOf entity)
  ) =>
  Text -> -- Query name for logging
  EntityFetcher entity (EventOf entity) ->
  QueryObjectStore query ->
  QueryUpdater
createUpdater queryName entityFetcher queryStore =
  QueryUpdater
    { queryName = queryName,
      updateQuery = \rawEvent -> do
        let entityName = rawEvent.entityName
        let streamId = rawEvent.streamId

        -- Reconstruct the entity from its event stream
        fetchResult <-
          entityFetcher.fetch entityName streamId
            |> Task.mapError toText

        case fetchResult of
          EntityNotFound ->
            -- Entity doesn't exist, nothing to update
            Task.yield unit
          EntityFound fetchedEntity -> do
            let entity = fetchedEntity.state
            let targetQueryId = queryId @entity @query entity

            -- Atomically update the query using the combine function
            let updateFn maybeExistingQuery =
                  case combine @entity @query entity maybeExistingQuery of
                    Update newQuery -> Just newQuery
                    Delete -> Nothing
                    NoOp -> maybeExistingQuery

            queryStore.atomicUpdate targetQueryId updateFn
              |> Task.mapError toText
    }
