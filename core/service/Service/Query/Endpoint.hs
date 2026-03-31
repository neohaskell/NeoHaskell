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
import Service.Query.Pagination (QueryPageRequest (..), QueryPageResponse (..))
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Task (Task)
import Task qualified
import Text (Text)
import NeoQL (Expr)
import NeoQL qualified


-- | Create an endpoint handler for a query type.
--
-- Returns a paginated response with items, total count, hasMore flag,
-- and the effective limit that was applied.
--
-- This handler performs two-phase authorization:
-- 1. canAccessImpl (pre-fetch): "Can this user access this query type at all?"
-- 2. canViewImpl (post-fetch): "Can this user view this specific instance?"
--
-- Pagination is applied AFTER authorization and NeoQL filtering.
-- The @total@ count reflects only authorized, filtered results.
--
-- Example:
--
-- @
-- handler <- Endpoint.createQueryEndpoint @UserOrders queryStore
-- -- Returns: "{\"items\":[...],\"total\":142,\"hasMore\":true,\"effectiveLimit\":100}"
-- @
createQueryEndpoint ::
  forall query.
  ( Json.ToJSON query,
    Query query
  ) =>
  QueryObjectStore query ->
  Maybe UserClaims ->
  Maybe Expr ->
  QueryPageRequest ->
  Task QueryEndpointError Text
createQueryEndpoint queryStore userClaims maybeExpr pageRequest = do
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

      -- SECURITY: total computed AFTER canViewImpl and NeoQL filtering.
      -- Exposing pre-auth count would leak record existence to unauthorized users.
      let total = Array.length filteredQueries
      let effectiveLimit = min pageRequest.limit (maxResultsImpl @query)
      let pagedItems =
            filteredQueries
              |> Array.drop pageRequest.offset
              |> Array.take effectiveLimit
      let hasMore = pageRequest.offset + effectiveLimit < total
      let response = QueryPageResponse
            { items = pagedItems
            , total = total
            , hasMore = hasMore
            , effectiveLimit = effectiveLimit
            }

      let responseText = response |> Json.encodeText

      Task.yield responseText


-- | Convert QueryObjectStore Error to QueryEndpointError.
storeErrorToEndpointError :: Error -> QueryEndpointError
storeErrorToEndpointError err = case err of
  Service.QueryObjectStore.Core.StorageError msg -> Auth.StorageError msg
  SerializationError msg -> Auth.StorageError msg
