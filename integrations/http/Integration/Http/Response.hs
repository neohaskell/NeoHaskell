-- | HTTP Response type for outbound integrations.
--
-- The 'Response' type is passed to 'onSuccess' callbacks,
-- allowing Jess to extract data from API responses.
module Integration.Http.Response
  ( Response (..)
  ) where

import Array (Array)
import Basics
import Json qualified
import Text (Text)


-- | Response data extracted from HTTP response.
--
-- Used in 'onSuccess' callbacks to access response data.
--
-- @
-- onSuccess = \\response ->
--   NotifyCustomer
--     { trackingUrl = response.body
--         |> Json.get "tracking_url"
--         |> Maybe.withDefault ""
--     }
-- @
--
-- You can also check the status code for conditional logic:
--
-- @
-- onSuccess = \\response ->
--   if response.statusCode == 304
--     then ResourceNotModified
--     else ResourceUpdated { etag = findHeader "ETag" response.headers }
-- @
data Response = Response
  { -- | HTTP status code (e.g., 200, 201, 404)
    statusCode :: Int
  , -- | Parsed JSON body (or Json.Null if not JSON)
    body :: Json.Value
  , -- | Response headers as key-value pairs
    headers :: Array (Text, Text)
  }
  deriving (Eq, Show, Generic)


instance Json.FromJSON Response
instance Json.ToJSON Response
