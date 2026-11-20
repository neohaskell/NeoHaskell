module Service.Query.Endpoint (
  createQueryEndpoint,
  createQueryEndpointWithProtection,
  createQueryByIdEndpoint,
  createQueryByIdEndpointWithProtection,
) where

import Array qualified
import Auth.Claims (UserClaims)
import Basics
import Erasable.Internal qualified as Erasable
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.AccessControl (QueryEndpointError)
import Service.AccessControl qualified as Auth
import Service.Query.Core (Query (..))
import Service.Query.Pagination (QueryPageRequest (..), QueryPageResponse (..))
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Task (Task)
import Task qualified
import Text (Text)
import NeoQL (Expr)
import NeoQL qualified
import Uuid (Uuid)


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
  createQueryEndpointWithProtection rejectProtectedJson queryStore userClaims maybeExpr pageRequest


rejectProtectedJson :: Json.Value -> Task QueryEndpointError Json.Value
rejectProtectedJson value =
  case Erasable.references value of
    Err _ -> Task.throw (Auth.StorageError "Malformed erasable response")
    Ok refs ->
      if Array.isEmpty refs
        then Task.yield value
        else Task.throw (Auth.StorageError "Protected response resolver is unavailable")


createQueryEndpointWithProtection ::
  forall query.
  (Json.ToJSON query, Query query) =>
  (Json.Value -> Task QueryEndpointError Json.Value) ->
  QueryObjectStore query ->
  Maybe UserClaims ->
  Maybe Expr ->
  QueryPageRequest ->
  Task QueryEndpointError Text
createQueryEndpointWithProtection renderProtected queryStore userClaims maybeExpr pageRequest = do
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

      responseJson <- response |> Json.toJSON |> renderProtected
      let responseText = responseJson |> Json.encodeText

      Task.yield responseText


-- | Create a single-object handler for a query type.
--
-- Query-level access is checked before the UUID lookup. Missing and row-denied
-- objects return the same 'Auth.QueryNotFound' error. Protected JSON is rejected
-- unless the caller uses 'createQueryByIdEndpointWithProtection'.
createQueryByIdEndpoint ::
  forall query.
  ( Json.ToJSON query,
    Query query
  ) =>
  QueryObjectStore query ->
  Maybe UserClaims ->
  Uuid ->
  Task QueryEndpointError Text
createQueryByIdEndpoint queryStore userClaims queryId = do
  createQueryByIdEndpointWithProtection rejectProtectedJson queryStore userClaims queryId


-- | Create a single-object handler that resolves protected values before encoding.
--
-- The typed store lookup happens once after query-level authorization. Row-level
-- authorization runs before JSON conversion or protected-value rendering.
createQueryByIdEndpointWithProtection ::
  forall query.
  (Json.ToJSON query, Query query) =>
  (Json.Value -> Task QueryEndpointError Json.Value) ->
  QueryObjectStore query ->
  Maybe UserClaims ->
  Uuid ->
  Task QueryEndpointError Text
createQueryByIdEndpointWithProtection renderProtected queryStore userClaims queryId = do
  case canAccessImpl @query userClaims of
    Just authErr -> Task.throw (Auth.AuthorizationError authErr)
    Nothing -> do
      maybeQuery <- queryStore.get queryId |> Task.mapError storeErrorToEndpointError
      case maybeQuery of
        Nothing -> Task.throw Auth.QueryNotFound
        Just query ->
          case canViewImpl @query userClaims query of
            Just _ -> Task.throw Auth.QueryNotFound
            Nothing -> do
              responseJson <- Json.toJSON query |> renderProtected
              Task.yield (Json.encodeText responseJson)


-- | Convert QueryObjectStore Error to QueryEndpointError.
storeErrorToEndpointError :: Error -> QueryEndpointError
storeErrorToEndpointError err = case err of
  Service.QueryObjectStore.Core.StorageError msg -> Auth.StorageError msg
  SerializationError msg -> Auth.StorageError msg
