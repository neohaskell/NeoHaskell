{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for ACS transactional email integration.
--
-- Contains the transformation from 'Request' to 'Http.Request', the single
-- 'Redacted.unwrap' site for the Bearer token, the endpoint guard (SEC-001),
-- and the 'AcsResponseHandler' HTTP response dispatcher.
--
-- __This module is not re-exported from 'Integration.Acs'.__
module Integration.Acs.Internal
  ( -- * Transformation (exported for testing)
    toHttpRequest
  , encodeRequest
  , encodeBodyField
  , encodeRecipientAddress
  , senderEmail
  , validateEndpoint
  , operationIdFrom
  , operationIdFromLocation
  , handleAcsResponse

    -- * Internal types (exported for testing)
  , AcsResponseHandler (..)
  ) where

import Array (Array)
import Array qualified
import Basics
import Integration (ToAction (..))
import Integration qualified
import Integration.Acs.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..))
import Integration.Acs.Response (Response (..))
import Integration.Http qualified as Http
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Redacted qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Text (Text)
import Text qualified


-- | Internal record bundling the onSuccess and onError callbacks from the Request.
-- Used to pass callbacks through to 'handleAcsResponse' without exposing them
-- outside Internal.
data AcsResponseHandler command = AcsResponseHandler
  { onSuccess :: Response -> command
  , onError :: Text -> command
  }


-- | Bridge from Request to the Integration.Http layer.
-- Validates the endpoint BEFORE unwrapping the token (ADR §4, SEC-001).
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  ToAction (Request command)
  where
  toAction req =
    case validateEndpoint req.endpoint of
      Result.Err message ->
        -- Non-https endpoint: emit the error command directly.
        -- The Bearer token is never unwrapped in this branch.
        Integration.action (\_ctx -> Integration.emitCommand (req.onError message))
      Result.Ok trimmedEndpoint ->
        -- Use the trimmed endpoint so leading/trailing whitespace never
        -- reaches the request URL (CodeRabbit: honor validateEndpoint's result).
        req { endpoint = trimmedEndpoint } |> toHttpRequest |> Integration.toAction


-- | Accept only an https endpoint; anything else would leak the Bearer token.
-- Returns 'Result.Ok' with the trimmed endpoint on success, 'Result.Err'
-- with a guided message on non-https.
-- Called from: 'toAction' (before token unwrap).
-- ADR §4 (SEC-001): ensures the Bearer token is never transmitted over cleartext.
validateEndpoint :: Text -> Result Text Text
validateEndpoint endpoint =
  do
    let trimmed = Text.trim endpoint
    if Text.toLower trimmed |> Text.startsWith "https://"
      then Result.Ok trimmed
      else Result.Err "ACS endpoint must use https (refusing to send the Bearer token over cleartext)"


-- | Convert a validated Request into an Http.Request, unwrapping the Bearer token.
-- This is the only site where 'Redacted.unwrap' is called (single-unwrap invariant).
-- Called from: 'toAction' (after 'validateEndpoint' passes).
toHttpRequest ::
  forall command.
  Request command ->
  Http.Request command
toHttpRequest req =
  do
    let endpointBase = Text.trim req.endpoint
    let url = [fmt|#{endpointBase}/emails:send?api-version=2023-03-31|]
    let tokenValue = Redacted.unwrap req.accessToken
    let encodedBody = encodeRequest req
    let handler = AcsResponseHandler { onSuccess = req.onSuccess, onError = req.onError }
    Http.Request
      { method = Http.POST
      , url
      , headers = Array.empty
      , body = Http.raw "application/json" encodedBody
      , onSuccess = handleAcsResponse handler
      , onError = Just req.onError
      , auth = Http.Bearer tokenValue
      , retry = Http.noRetry
      , timeoutSeconds = 30
      }


-- | Encode a Request into ACS-compatible JSON wire format.
-- Target shape:
--
-- @
-- { "senderAddress": "noreply\@myapp.com"
-- , "content": { "subject": "Welcome", "html": "\<h1\>Hi\<\/h1\>" }
-- , "recipients": { "to": [ { "address": "user\@example.com" } ] }
-- }
-- @
--
-- Hand-built to omit optional fields entirely (never serialize null).
-- Called from: 'toHttpRequest'.
encodeRequest ::
  forall command.
  Request command ->
  Text
encodeRequest req =
  do
    let contentFields =
          encodeBodyField req.body
            |> Array.push ("subject", Json.encode req.subject)
            |> Array.toLinkedList
    let toEntries =
          req.to
            |> Array.map encodeRecipientAddress
    let allFields =
          [ ("senderAddress", Json.encode (senderEmail req.sender))
          , ("content", Json.object contentFields)
          , ("recipients", Json.object [("to", Json.encode toEntries)])
          ]
    Json.encodeText (Json.object allFields)


-- | Pick the ACS body key for the chosen Body variant.
-- Returns an array with a single (key, value) pair: @("html", ...)@ or
-- @("plainText", ...)@.
encodeBodyField :: Body -> Array (Text, Json.Value)
encodeBodyField body =
  case body of
    HtmlBody html ->
      Array.fromLinkedList [("html", Json.encode html)]
    TextBody text ->
      Array.fromLinkedList [("plainText", Json.encode text)]


-- | Encode a Recipient as a JSON object @{ "address": "..." }@.
encodeRecipientAddress :: Recipient -> Json.Value
encodeRecipientAddress recipientVal =
  case recipientVal of
    Recipient (Address { email = emailVal }) ->
      Json.object [("address", Json.encode emailVal)]


-- | Extract the email address from a Sender.
senderEmail :: Sender -> Text
senderEmail senderVal =
  case senderVal of
    Sender (Address { email = emailVal }) ->
      emailVal


-- | Dispatch the ACS HTTP response on status code.
-- 202 Accepted always succeeds (never retried, ADR §6, SEC-002).
-- Auth/error cases return sanitized messages (no token or body leak).
-- Called from: 'toHttpRequest' as the onSuccess handler for 'Http.Request'.
handleAcsResponse ::
  forall command.
  AcsResponseHandler command ->
  Http.Response ->
  command
handleAcsResponse handler response =
  case response.statusCode of
    202 ->
      -- Accepted.  Always succeed; never re-send on a 202 (ADR §6, SEC-002).
      handler.onSuccess (Response { operationId = operationIdFrom response })
    401 ->
      handler.onError "Authentication failed (HTTP 401): unauthorized"
    403 ->
      handler.onError "Authorization failed (HTTP 403): unauthorized"
    429 ->
      handler.onError "Rate limit exceeded (HTTP 429): throttled"
    status | status >= 400 && status < 500 ->
      handler.onError [fmt|ACS client error (HTTP #{status})|]
    status | status >= 500 ->
      handler.onError [fmt|ACS server error (HTTP #{status})|]
    status ->
      handler.onError [fmt|Unexpected HTTP status #{status}|]


-- | Best-effort ACS operation id: read from body @id@ key, fall back to
-- @Operation-Location@ header trailing segment, fall back to empty string.
-- Called from 'handleAcsResponse' on every 202.
operationIdFrom :: Http.Response -> Text
operationIdFrom response =
  case (Json.decode response.body :: Result Text Response) of
    Result.Ok (Response { operationId = opId }) ->
      opId
    Result.Err _ ->
      response.headers
        |> Array.find (\(headerName, _) -> Text.toLower headerName == "operation-location")
        |> Maybe.map (\(_, value) -> operationIdFromLocation value)
        |> Maybe.withDefault ""


-- | Parse the operation id from an Operation-Location header value.
-- The header value is typically something like
-- @https://my-acs.communication.azure.com/operations/abc-123@;
-- extract the last path segment.
operationIdFromLocation :: Text -> Text
operationIdFromLocation location =
  location
    |> Text.split "/"
    |> Array.last
    |> Maybe.withDefault ""
