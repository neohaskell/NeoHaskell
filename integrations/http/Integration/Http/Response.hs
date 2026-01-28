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
-- == Known Limitations (v1)
--
-- * 'statusCode' is currently a placeholder (always 200)
-- * 'headers' is currently empty
--
-- These will be populated when 'Http.Client' is extended
-- to expose response metadata.
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
