module Service.Query.Endpoint (
  createQueryEndpoint,
) where

import Array qualified
import Auth.Claims (UserClaims)
import Basics
import Json qualified
import Maybe (Maybe (..))
import Service.Query.Auth (QueryEndpointError)
import Service.Query.Auth qualified as Auth
import Service.Query.Core (Query (..))
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Task (Task)
import Task qualified
import Text (Text)
import NeoQL (Expr)
import NeoQL qualified


-- | Create an endpoint handler for a query type.
--
-- Returns all instances of the query as a JSON array.
-- Used for the HTTP endpoint: GET /queries/{query-name}
--
-- This handler performs two-phase authorization:
-- 1. canAccessImpl (pre-fetch): "Can this user access this query type at all?"
-- 2. canViewImpl (post-fetch): "Can this user view this specific instance?"
--
-- Returns typed QueryEndpointError for proper HTTP status mapping.
--
-- Example:
--
-- @
-- handler <- Endpoint.createQueryEndpoint @UserOrders queryStore
-- -- Returns: "[{\"userId\":\"...\",\"orders\":[...]}, ...]"
-- @
createQueryEndpoint ::
  forall query.
  ( Json.ToJSON query,
    Query query
  ) =>
  QueryObjectStore query ->
  Maybe UserClaims ->
  Maybe Expr ->
  Task QueryEndpointError Text
createQueryEndpoint queryStore userClaims maybeExpr = do
  -- Phase 1: Pre-fetch authorization
  case canAccessImpl @query userClaims of
    Just authErr -> Task.throw (Auth.AuthorizationError authErr)
    Nothing -> do
      -- Fetch all query instances
      allQueries <-
        queryStore.getAll
          |> Task.mapError storeErrorToEndpointError

      -- Phase 2: Post-fetch authorization (filter out unauthorized instances)
      let authorizedQueries =
            allQueries
              |> Array.takeIf (\query -> canViewImpl @query userClaims query == Nothing)

      let filteredQueries =
            case maybeExpr of
              Maybe.Nothing -> authorizedQueries
              Maybe.Just expr ->
                authorizedQueries
                  |> Array.takeIf (\query -> NeoQL.execute expr (Json.toJSON query))

      let responseText = filteredQueries |> Json.encodeText

      Task.yield responseText


-- | Convert QueryObjectStore Error to QueryEndpointError.
storeErrorToEndpointError :: Error -> QueryEndpointError
storeErrorToEndpointError err = case err of
  Service.QueryObjectStore.Core.StorageError msg -> Auth.StorageError msg
  SerializationError msg -> Auth.StorageError msg
