module Service.Query.Endpoint (
  createQueryEndpoint,
) where

import Basics
import Json qualified
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Task (Task)
import Task qualified
import Text (Text)


-- | Create an endpoint handler for a query type.
--
-- Returns all instances of the query as a JSON array.
-- Used for the HTTP endpoint: GET /queries/{query-name}
--
-- Example:
--
-- @
-- handler <- Endpoint.createQueryEndpoint @UserOrders queryStore
-- -- Returns: "[{\"userId\":\"...\",\"orders\":[...]}, ...]"
-- @
createQueryEndpoint ::
  forall query.
  (Json.ToJSON query) =>
  QueryObjectStore query ->
  Task Text Text
createQueryEndpoint queryStore = do
  allQueries <- queryStore.getAll
    |> Task.mapError errorToText

  let responseText = allQueries |> Json.encodeText

  Task.yield responseText


-- | Convert QueryObjectStore Error to Text.
errorToText :: Error -> Text
errorToText err = case err of
  StorageError msg -> msg
  SerializationError msg -> msg
