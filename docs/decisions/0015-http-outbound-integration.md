# ADR-0015: HTTP Outbound Integration

## Status

Proposed

## Context

ADR-0008 established the integration pattern for connecting domain events to external systems. It introduced the two-persona model:

- **Nick (Integration Developer)**: Builds reusable integration packages with `ToAction` instances, handling HTTP calls, retries, authentication, and error handling
- **Jess (Integration User)**: Configures integrations by instantiating config records, never seeing `Task`, networking code, or error handling

The core framework currently provides two built-in integrations:

1. `Integration.Command.Emit` - Emits commands to other services (no external calls)
2. `Integration.Timer.Every` - Periodic command submission

However, the most common integration need is **HTTP outbound calls**: notifying external APIs when domain events occur. Examples include:

- Sending order data to a shipping provider when an order ships
- Notifying a payment processor of refunds
- Posting to Slack/Discord when significant events occur
- Syncing data to third-party CRMs or analytics platforms

Without a standard HTTP integration, every team would need to:

1. Write their own HTTP integration with Nick's complexity
2. Handle authentication patterns (Bearer tokens, API keys, Basic auth)
3. Implement retry logic with exponential backoff
4. Map HTTP errors to `IntegrationError`
5. Parse responses and extract data for command emission

This ADR introduces `Integration.Http` as the first integration in a new `integrations` package, providing a production-ready HTTP outbound integration that Jess can use with pure record configuration.

### Design Goals

1. **Zero HTTP code for Jess**: Jess configures method, URL, headers, and body without importing `Http` or writing `Task` code
2. **Flexible authentication**: Support common patterns (Bearer, Basic, API key header) with environment variable expansion
3. **Typed responses**: Extract response data for `onSuccess` callbacks
4. **Configurable retries**: Exponential backoff with sensible defaults
5. **Clear error handling**: Map HTTP failures to `IntegrationError` with optional `onError` callbacks
6. **Future-proof**: Package structure supports additional integrations (Sendgrid, Stripe, Slack)

## Decision

### 1. Package Structure

Create a new `integrations` directory alongside `core` and `testbed`:

```text
NeoHaskell/
  core/                       -- Core framework (existing)
  testbed/                    -- Example application (existing)
  integrations/               -- NEW: Integration packages
    http/                     -- HTTP outbound integration
      Integration/
        Http.hs               -- Main module (Jess's API)
        Http/
          Request.hs          -- Request config type
          Response.hs         -- Response handling types
          Auth.hs             -- Authentication patterns
          Retry.hs            -- Retry configuration
          Internal.hs         -- ToAction implementation (Nick's code)
      nhintegration-http.cabal
```

This structure allows future integrations (`integrations/sendgrid/`, `integrations/slack/`) to follow the same pattern and depend on `nhintegration-http` for their HTTP needs.

### 2. Core Types (Jess's API)

#### Request Config

```haskell
-- Integration/Http.hs
module Integration.Http
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Method (..)
  , Body (..)
  , Auth (..)
  , Retry (..)
  , Response (..)
    -- * Response Helpers
  , ResponseHandler
  , ignoreResponse
  , expectJson
    -- * Body Helpers
  , json
  , form
  , raw
  , noBody
    -- * Retry Helpers
  , defaultRetry
  , noRetry
  , withRetries
  ) where

import Integration qualified
import Json qualified

-- | HTTP methods supported for outbound requests.
data Method
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  deriving (Eq, Show, Generic)

-- | Request body configuration.
data Body
  = JsonBody Json.Value
  | FormBody (Array (Text, Text))
  | RawBody Text Text           -- ^ Content-Type, body content
  | NoBody
  deriving (Eq, Show, Generic)

-- | Authentication configuration with environment variable expansion.
-- Values like "${API_KEY}" are expanded from environment at runtime.
data Auth
  = NoAuth
  | Bearer Text                  -- ^ Authorization: Bearer <token>
  | Basic Text Text              -- ^ Authorization: Basic base64(user:pass)
  | ApiKey Text Text             -- ^ Custom header name and value
  deriving (Eq, Show, Generic)

-- | Retry configuration for transient failures.
data Retry = Retry
  { maxAttempts :: Int           -- ^ Maximum retry attempts (default: 3)
  , initialDelayMs :: Int        -- ^ Initial delay in milliseconds (default: 1000)
  , maxDelayMs :: Int            -- ^ Maximum delay in milliseconds (default: 30000)
  , retryableStatuses :: Array Int -- ^ HTTP status codes to retry (default: [429, 500, 502, 503, 504])
  }
  deriving (Eq, Show, Generic)

-- | Response data extracted from HTTP response.
-- Used in onSuccess callbacks.
data Response = Response
  { statusCode :: Int
  , body :: Json.Value           -- ^ Parsed JSON body (or Json.Null if not JSON)
  , headers :: Array (Text, Text)
  }
  deriving (Eq, Show, Generic)

instance Json.FromJSON Response
instance Json.ToJSON Response

-- | The main request configuration record that Jess instantiates.
--
-- @
-- Integration.outbound Http.Request
--   { method = Http.POST
--   , url = "https://api.shippo.com/shipments"
--   , headers = [("X-Api-Key", "${SHIPPO_API_KEY}")]
--   , body = Http.json shipmentData
--   , onSuccess = \\response -> NotifyCustomer
--       { trackingUrl = response.body.tracking_url
--       }
--   , onError = Just (\\err -> LogShippingError { error = err })
--   , auth = Http.NoAuth
--   , retry = Http.defaultRetry
--   , timeoutSeconds = 30
--   }
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
```

#### Helper Functions

```haskell
-- | Create a JSON body from any ToJSON value.
json :: forall a. (Json.ToJSON a) => a -> Body
json value = JsonBody (Json.encode value)

-- | Create a form-encoded body.
form :: Array (Text, Text) -> Body
form params = FormBody params

-- | Create a raw body with custom content type.
raw :: Text -> Text -> Body
raw contentType content = RawBody contentType content

-- | No request body.
noBody :: Body
noBody = NoBody

-- | Default retry configuration (3 attempts, exponential backoff).
defaultRetry :: Retry
defaultRetry = Retry
  { maxAttempts = 3
  , initialDelayMs = 1000
  , maxDelayMs = 30000
  , retryableStatuses = [429, 500, 502, 503, 504]
  }

-- | No retries.
noRetry :: Retry
noRetry = Retry
  { maxAttempts = 1
  , initialDelayMs = 0
  , maxDelayMs = 0
  , retryableStatuses = []
  }

-- | Custom retry count with default backoff.
withRetries :: Int -> Retry
withRetries n = defaultRetry { maxAttempts = n }
```

### 3. ToAction Implementation (Nick's Code)

```haskell
-- Integration/Http/Internal.hs
module Integration.Http.Internal
  ( executeRequest
  ) where

import AsyncTask qualified
import Environment qualified
import Http.Client qualified as Http
import Integration qualified
import Integration.Http
import Json qualified
import Task qualified
import Text qualified

-- | ToAction instance that executes the HTTP request.
instance
  (Json.ToJSON command, GHC.KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction config = Integration.action do
    executeRequest config

-- | Execute an HTTP request with retries and error handling.
executeRequest ::
  forall command.
  (Json.ToJSON command, GHC.KnownSymbol (NameOf command)) =>
  Request command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeRequest config = do
  -- Expand environment variables in URL and headers
  expandedUrl <- expandEnvVars config.url
  expandedHeaders <- config.headers
    |> Array.mapM (\(k, v) -> do
        expandedV <- expandEnvVars v
        Task.yield (k, expandedV))

  -- Build authentication header
  authHeader <- buildAuthHeader config.auth

  -- Execute with retries
  result <- executeWithRetry config.retry 1 do
    executeHttpRequest
      config.method
      expandedUrl
      (expandedHeaders |> addAuthHeader authHeader)
      config.body
      config.timeoutSeconds

  case result of
    Result.Ok response ->
      Integration.emitCommand (config.onSuccess response)
    Result.Err errorText ->
      case config.onError of
        Just handler -> Integration.emitCommand (handler errorText)
        Nothing -> Task.throw (Integration.NetworkError errorText)

-- | Execute HTTP request with exponential backoff retry.
executeWithRetry ::
  Retry ->
  Int ->                         -- Current attempt number
  Task Integration.IntegrationError (Result Text Response) ->
  Task Integration.IntegrationError (Result Text Response)
executeWithRetry retryConfig attempt action = do
  result <- action
    |> Task.attempt
  case result of
    Result.Ok (Result.Ok response) ->
      -- Success - check if status is retryable
      if Array.contains response.statusCode retryConfig.retryableStatuses
           && attempt < retryConfig.maxAttempts
        then do
          delay <- calculateBackoff retryConfig attempt
          AsyncTask.sleep delay
          executeWithRetry retryConfig (attempt + 1) action
        else Task.yield (Result.Ok response)

    Result.Ok (Result.Err errorText) ->
      -- HTTP error - check if retryable
      if attempt < retryConfig.maxAttempts
        then do
          delay <- calculateBackoff retryConfig attempt
          AsyncTask.sleep delay
          executeWithRetry retryConfig (attempt + 1) action
        else Task.yield (Result.Err errorText)

    Result.Err integrationError ->
      -- Integration error - propagate
      Task.throw integrationError

-- | Calculate backoff delay with exponential increase and jitter.
calculateBackoff :: Retry -> Int -> Task Integration.IntegrationError Int
calculateBackoff config attempt = do
  let baseDelay = config.initialDelayMs * (2 ^ (attempt - 1))
  let cappedDelay = min baseDelay config.maxDelayMs
  -- Add jitter (0-25% of delay)
  jitter <- Random.int 0 (cappedDelay `div` 4)
  Task.yield (cappedDelay + jitter)

-- | Execute a single HTTP request.
executeHttpRequest ::
  Method ->
  Text ->
  Array (Text, Text) ->
  Body ->
  Int ->
  Task Integration.IntegrationError (Result Text Response)
executeHttpRequest method url headers body timeoutSeconds = do
  let baseRequest = Http.request
        |> Http.withUrl url
        |> Http.withTimeout timeoutSeconds

  let withHeaders = headers
        |> Array.reduce baseRequest (\(key, value) req ->
             req |> Http.addHeader key value)

  response <- case (method, body) of
    (GET, _) ->
      Http.get @Json.Value withHeaders
        |> Task.mapError mapHttpError
        |> Task.attempt

    (POST, JsonBody jsonBody) ->
      Http.post @Json.Value withHeaders jsonBody
        |> Task.mapError mapHttpError
        |> Task.attempt

    (POST, FormBody formParams) ->
      Http.postForm @Json.Value withHeaders formParams
        |> Task.mapError mapHttpError
        |> Task.attempt

    -- PUT, PATCH, DELETE would need extensions to Http.Client
    -- For now, they follow similar patterns

    _ ->
      Task.yield (Result.Err "Unsupported method/body combination")

  case response of
    Result.Ok jsonValue ->
      Task.yield (Result.Ok Response
        { statusCode = 200  -- TODO: Extract from actual response
        , body = jsonValue
        , headers = []      -- TODO: Extract from actual response
        })
    Result.Err err ->
      Task.yield (Result.Err (toText err))

-- | Map HTTP errors to IntegrationError.
mapHttpError :: Http.Error -> Integration.IntegrationError
mapHttpError (Http.Error msg) =
  if Text.contains "timeout" msg
    then Integration.NetworkError [fmt|Request timeout: {msg}|]
    else if Text.contains "connection" msg
      then Integration.NetworkError [fmt|Connection error: {msg}|]
      else Integration.NetworkError msg

-- | Expand environment variables in text.
-- Patterns like "${VAR_NAME}" are replaced with environment values.
expandEnvVars :: Text -> Task Integration.IntegrationError Text
expandEnvVars text = do
  -- Simple pattern matching for ${VAR_NAME}
  -- In production, use a proper parser
  let pattern = "\\$\\{([A-Z_][A-Z0-9_]*)\\}"
  Text.replaceAllWithM pattern text \match -> do
    let varName = match |> Text.drop 2 |> Text.dropEnd 1
    Environment.get varName
      |> Task.mapError (\_ -> Integration.AuthenticationError
           [fmt|Missing environment variable: {varName}|])

-- | Build authentication header from Auth config.
buildAuthHeader :: Auth -> Task Integration.IntegrationError (Maybe (Text, Text))
buildAuthHeader auth = case auth of
  NoAuth -> Task.yield Nothing
  Bearer token -> do
    expanded <- expandEnvVars token
    Task.yield (Just ("Authorization", [fmt|Bearer {expanded}|]))
  Basic user pass -> do
    expandedUser <- expandEnvVars user
    expandedPass <- expandEnvVars pass
    let credentials = Base64.encode [fmt|{expandedUser}:{expandedPass}|]
    Task.yield (Just ("Authorization", [fmt|Basic {credentials}|]))
  ApiKey headerName headerValue -> do
    expandedValue <- expandEnvVars headerValue
    Task.yield (Just (headerName, expandedValue))

-- | Add auth header to headers array if present.
addAuthHeader ::
  Maybe (Text, Text) ->
  Array (Text, Text) ->
  Array (Text, Text)
addAuthHeader maybeAuth headers = case maybeAuth of
  Nothing -> headers
  Just header -> headers |> Array.push header
```

### 4. Usage Examples

#### Basic POST Request (Jess's Code)

```haskell
-- Order/Integrations.hs
module Order.Integrations where

import Integration qualified
import Integration.Http qualified as Http
import Order.Core (Order, OrderEvent (..))
import Notification.Commands (NotifyCustomer (..))

orderIntegrations :: Order -> OrderEvent -> Integration.Outbound
orderIntegrations order event = case event of
  OrderShipped info -> Integration.batch
    [ Integration.outbound Http.Request
        { method = Http.POST
        , url = "https://api.shippo.com/v1/shipments"
        , headers = []
        , body = Http.json
            { tracking_number = info.trackingNumber
            , carrier = info.carrier
            , recipient_email = order.customerEmail
            }
        , onSuccess = \response -> NotifyCustomer
            { orderId = order.id
            , trackingUrl = response.body
                |> Json.get "tracking_url_provider"
                |> Maybe.withDefault ""
            }
        , onError = Just (\err -> LogShippingError
            { orderId = order.id
            , error = err
            })
        , auth = Http.ApiKey "X-Api-Key" "${SHIPPO_API_KEY}"
        , retry = Http.defaultRetry
        , timeoutSeconds = 30
        }
    ]

  OrderRefunded info -> Integration.batch
    [ Integration.outbound Http.Request
        { method = Http.POST
        , url = "https://api.stripe.com/v1/refunds"
        , headers = []
        , body = Http.form
            [ ("payment_intent", info.paymentIntentId)
            , ("amount", Text.fromInt info.amountCents)
            ]
        , onSuccess = \_ -> RecordRefund
            { orderId = order.id
            , refundId = ""  -- Extract from response if needed
            }
        , onError = Nothing  -- Let it fail and retry
        , auth = Http.Bearer "${STRIPE_SECRET_KEY}"
        , retry = Http.withRetries 5
        , timeoutSeconds = 60
        }
    ]

  _ -> Integration.none
```

#### Slack Notification

```haskell
slackNotification :: Http.Request SlackMessageSent
slackNotification = Http.Request
  { method = Http.POST
  , url = "https://hooks.slack.com/services/T00/B00/XXX"
  , headers = [("Content-Type", "application/json")]
  , body = Http.json
      { text = "New order received!"
      , channel = "#orders"
      }
  , onSuccess = \_ -> SlackMessageSent
  , onError = Nothing  -- Fire and forget
  , auth = Http.NoAuth  -- Webhook URL contains auth
  , retry = Http.noRetry
  , timeoutSeconds = 10
  }
```

### 5. Error Mapping

HTTP errors map to `IntegrationError` as follows:

| HTTP Condition | IntegrationError | Retry? |
|----------------|------------------|--------|
| Connection refused | `NetworkError` | Yes |
| Timeout | `NetworkError` | Yes |
| 400 Bad Request | `ValidationError` | No |
| 401 Unauthorized | `AuthenticationError` | No |
| 403 Forbidden | `AuthenticationError` | No |
| 429 Too Many Requests | `RateLimited` | Yes (with Retry-After) |
| 500 Internal Server Error | `NetworkError` | Yes |
| 502 Bad Gateway | `NetworkError` | Yes |
| 503 Service Unavailable | `NetworkError` | Yes |
| 504 Gateway Timeout | `NetworkError` | Yes |
| Other 4xx | `ValidationError` | No |
| Other 5xx | `NetworkError` | Yes |

### 6. Cabal Configuration

```cabal
-- integrations/http/nhintegration-http.cabal
cabal-version: 3.0
name:          nhintegration-http
version:       0.1.0.0
synopsis:      HTTP outbound integration for NeoHaskell

library
  exposed-modules:
    Integration.Http
    Integration.Http.Auth
    Integration.Http.Request
    Integration.Http.Response
    Integration.Http.Retry
  other-modules:
    Integration.Http.Internal
  build-depends:
    base,
    nhcore,
    text,
    aeson
  hs-source-dirs: .
  default-language: GHC2021
```

### 7. Module Structure Summary

```text
integrations/http/
  Integration/
    Http.hs                   -- Re-exports (Jess's entry point)
    Http/
      Request.hs              -- Request, Method, Body types
      Response.hs             -- Response type
      Auth.hs                 -- Auth type
      Retry.hs                -- Retry type and helpers
      Internal.hs             -- ToAction instance (Nick's code)
```

**What Jess imports:**
```haskell
import Integration.Http qualified as Http
-- Uses: Http.Request, Http.POST, Http.json, Http.Bearer, Http.defaultRetry
```

**What Jess does NOT see:**
- `Task` type
- `Http.Client` module
- Retry loop implementation
- Error mapping logic
- Environment variable expansion

## Consequences

### Positive

1. **Pure configuration for Jess**: Jess writes plain record instantiation with no monadic syntax, no `Task`, no imports from `Http.Client`.

2. **Environment variable security**: Secrets like API keys use `${VAR_NAME}` patterns, never hardcoded. Variables are expanded at runtime, not compile time.

3. **Production-ready retries**: Exponential backoff with jitter prevents thundering herd. Configurable retry statuses handle rate limiting.

4. **Typed responses**: The `Response` type provides structured access to status code, headers, and parsed JSON body for `onSuccess` callbacks.

5. **Optional error handling**: `onError` callback allows Jess to emit a command on failure (e.g., logging), or let the integration fail and retry via the dispatcher.

6. **Consistent with ADR-0008**: Follows the two-persona model exactly. Nick's complexity is hidden in `Internal.hs`; Jess sees only config records.

7. **Foundation for other integrations**: The `integrations/` package structure supports future additions (Sendgrid, Stripe, Slack SDKs) that build on `nhintegration-http`.

8. **NeoHaskell style compliant**: Uses `|>` pipe operator, `do` blocks with `let`, `Task` not `IO`, qualified imports.

### Negative

1. **HTTP client limitations**: The current `Http.Client` in core only supports GET and POST. PUT, PATCH, DELETE would need core extensions.

2. **Response status not exposed**: Current `Http.Client` doesn't expose status codes or headers. The `Response` type anticipates this but requires core changes.

3. **No streaming**: Large request/response bodies are loaded into memory. Not suitable for file uploads or streaming APIs.

4. **No OAuth2 flows**: Only supports static credentials (Bearer, API key). OAuth2 token refresh would need additional infrastructure.

5. **Hardcoded content types**: JSON and form-encoded are supported. Other content types (XML, multipart) would need additional body constructors.

### Trade-offs

1. **Simplicity over flexibility**: The config record has fixed fields. Advanced HTTP features (custom TLS, proxy, cookies) are not exposed to Jess. This keeps the API simple but limits edge cases.

2. **Retry policy is per-request**: Each request has its own retry config. Global circuit breaker or rate limiting per-host is not included.

3. **Environment variables over config**: Secrets use `${VAR_NAME}` expansion rather than a config/secrets management system. Simple but less flexible than vault integration.

4. **Synchronous response handling**: The `onSuccess` callback receives the full response. Streaming or chunked responses are not supported.

## Future Work

1. **Extend `Http.Client`**: Add PUT, PATCH, DELETE methods. Expose response status codes and headers.

2. **Circuit breaker**: Add per-host circuit breaking to prevent cascading failures when external APIs are down.

3. **Rate limiting**: Implement client-side rate limiting to respect API quotas.

4. **OAuth2 token refresh**: For APIs requiring OAuth2, integrate with ADR-0010's token management.

5. **Metrics**: Add timing/success/failure metrics for observability.

6. **Batch requests**: Support for APIs that accept batch payloads to reduce round trips.

7. **Higher-level integrations**: Build `Integration.Sendgrid`, `Integration.Slack`, etc. using `Integration.Http` as the foundation.

## References

- [ADR-0008: Integration Pattern](0008-integration-pattern.md) - Two-persona integration design
- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md) - Command emission patterns
- [ADR-0009: JWT Authentication Middleware](0009-jwt-authentication-middleware.md) - Related auth patterns
- [core/http/Http/Client.hs](../../core/http/Http/Client.hs) - Current HTTP client implementation
- [core/service/Integration.hs](../../core/service/Integration.hs) - Core integration types
- [core/service/Integration/Command.hs](../../core/service/Integration/Command.hs) - Example ToAction implementation
- [testbed/src/Testbed/Cart/Integrations.hs](../../testbed/src/Testbed/Cart/Integrations.hs) - Example integration usage
