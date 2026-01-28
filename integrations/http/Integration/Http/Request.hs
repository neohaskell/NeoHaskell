-- | HTTP Request types for outbound integrations.
--
-- This module defines the 'Request' record that Jess configures
-- to make HTTP requests to external APIs.
--
-- == Example
--
-- @
-- Integration.outbound Http.Request
--   { method = Http.POST
--   , url = "https://api.example.com/orders"
--   , headers = []
--   , body = Http.json orderData
--   , onSuccess = \\response -> ConfirmOrder { ... }
--   , onError = Nothing
--   , auth = Http.Bearer "${API_TOKEN}"
--   , retry = Http.defaultRetry
--   , timeoutSeconds = 30
--   }
-- @
module Integration.Http.Request
  ( -- * Request Configuration
    Request (..)

    -- * HTTP Methods
  , Method (..)

    -- * Request Body
  , Body (..)
  , json
  , form
  , raw
  , noBody
  ) where

import Array (Array)
import Basics
import Integration.Http.Auth (Auth)
import Integration.Http.Response (Response)
import Integration.Http.Retry (Retry)
import Json qualified
import Maybe (Maybe)
import Text (Text)


-- | HTTP methods supported for outbound requests.
--
-- == Known Limitations (v1)
--
-- PUT, PATCH, and DELETE are defined but may not be fully
-- supported by the underlying 'Http.Client'. They will return
-- an error indicating the method is unsupported.
data Method
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  deriving (Eq, Show, Generic)


instance Json.ToJSON Method
instance Json.FromJSON Method


-- | Request body configuration.
--
-- Use the helper functions 'json', 'form', 'raw', and 'noBody'
-- to construct body values.
data Body
  = -- | JSON body from any ToJSON value
    JsonBody Json.Value
  | -- | Form-urlencoded body
    FormBody (Array (Text, Text))
  | -- | Raw body with custom content type
    RawBody
      { contentType :: Text
      , content :: Text
      }
  | -- | No request body
    NoBody
  deriving (Eq, Show, Generic)


-- | Create a JSON body from any ToJSON value.
--
-- @
-- Http.json { name = "Order", items = [1, 2, 3] }
-- @
json :: forall a. (Json.ToJSON a) => a -> Body
json value = JsonBody (Json.encode value)


-- | Create a form-urlencoded body.
--
-- @
-- Http.form
--   [ ("grant_type", "authorization_code")
--   , ("code", authCode)
--   ]
-- @
form :: Array (Text, Text) -> Body
form params = FormBody params


-- | Create a raw body with custom content type.
--
-- @
-- Http.raw "application/xml" "<order><id>123</id></order>"
-- @
raw :: Text -> Text -> Body
raw ct ct' = RawBody { contentType = ct, content = ct' }


-- | No request body.
--
-- @
-- Http.Request { body = Http.noBody, ... }
-- @
noBody :: Body
noBody = NoBody


-- | The main request configuration record that Jess instantiates.
--
-- The @command@ type parameter is the domain command emitted by
-- 'onSuccess' or 'onError' callbacks.
--
-- == Fields
--
-- * 'method': HTTP method (GET, POST, PUT, PATCH, DELETE)
-- * 'url': Full URL including scheme, host, path. Supports @${VAR}@ expansion.
-- * 'headers': Additional headers as key-value pairs. Supports @${VAR}@ expansion.
-- * 'body': Request body (use 'json', 'form', 'raw', or 'noBody')
-- * 'onSuccess': Callback that receives response and returns a domain command
-- * 'onError': Optional callback for error handling (if Nothing, integration fails)
-- * 'auth': Authentication configuration (see "Integration.Http.Auth")
-- * 'retry': Retry configuration (see "Integration.Http.Retry")
-- * 'timeoutSeconds': Request timeout in seconds
--
-- == Environment Variable Expansion
--
-- URL, headers, and auth values support @${VAR_NAME}@ patterns that
-- are expanded from environment variables at runtime:
--
-- @
-- url = "https://api.example.com/${API_VERSION}/orders"
-- auth = Http.Bearer "${API_TOKEN}"
-- @
data Request command = Request
  { method :: Method
  , url :: Text
  , headers :: Array (Text, Text)
  , body :: Body
  , onSuccess :: Response -> command
  , onError :: Maybe (Text -> command)
  , auth :: Auth
  , retry :: Retry
  , timeoutSeconds :: Int
  }
  deriving (Generic)
