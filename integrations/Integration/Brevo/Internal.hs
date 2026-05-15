-- | Internal implementation for Brevo transactional email integration.
--
-- This module contains the transformation from 'Brevo.Request' to
-- 'Http.Request', the single Redacted.unwrap site for the API key,
-- and the 'BrevoResponseHandler' HTTP response dispatcher.
--
-- __This module is not re-exported from 'Integration.Brevo'.__
module Integration.Brevo.Internal
  ( -- * Transformation (exported for testing)
    toHttpRequest
  , encodeRequest
  , BrevoResponseHandler (..)
  ) where

import Array (Array)
import Array qualified
import Basics
import Integration.Brevo.Request (Body (..), Request (..))
import Integration.Brevo.Response (Response)
import Integration.Http qualified as Http
import Json qualified
import Map qualified
import Maybe (Maybe (..))
import Redacted qualified
import Result qualified
import Text (Text)


-- | Handle Brevo's HTTP response: success (201 with messageId) or error
-- (401/402/429/4xx/5xx with sanitized error text).
data BrevoResponseHandler command = BrevoResponseHandler
  { onSuccess :: Response -> command
  , onError :: Text -> command
  }


-- | Transform a Brevo Request into an Http.Request, encoding the body as
-- JSON and constructing the Authorization header.
--
-- This is the single site where the apiKey Redacted wrapper is unwrapped.
-- No other unwrapping is allowed.
toHttpRequest ::
  forall command.
  Request command ->
  Http.Request command
toHttpRequest req =
  do
    let url = "https://api.brevo.com/v3/smtp/email"
    let apiKeyValue = Redacted.unwrap req.apiKey
    let authHeader = Http.ApiKey { headerName = "api-key", headerValue = apiKeyValue }
    let encodedBody = encodeRequest req
    let handler = BrevoResponseHandler { onSuccess = req.onSuccess, onError = req.onError }
    Http.Request
      { method = Http.POST
      , url
      , headers = Array.empty
      , body = Http.raw "application/json" encodedBody
      , onSuccess = handleBrevoResponse handler
      , onError = Just req.onError
      , auth = authHeader
      , retry = Http.noRetry
      , timeoutSeconds = 30
      }


-- | Handle the Brevo HTTP response, dispatching on status code.
handleBrevoResponse ::
  forall command.
  BrevoResponseHandler command ->
  Http.Response ->
  command
handleBrevoResponse handler response =
  case response.statusCode of
    201 ->
      case Json.decode response.body of
        Result.Ok brevoResponse ->
          handler.onSuccess brevoResponse
        Result.Err _ ->
          handler.onError "Failed to parse Brevo response"
    401 ->
      handler.onError "Authentication failed (HTTP 401): invalid API key"
    402 ->
      handler.onError "Payment required (HTTP 402): account credit exhausted"
    429 ->
      handler.onError "Rate limit exceeded (HTTP 429): too many requests"
    status | status >= 400 && status < 500 ->
      handler.onError [fmt|Brevo client error (HTTP #{status})|]
    status | status >= 500 ->
      handler.onError [fmt|Brevo server error (HTTP #{status})|]
    status ->
      handler.onError [fmt|Unexpected HTTP status #{status}|]


-- | Encode a Brevo Request to the JSON shape expected by the Brevo API.
--
-- Optional fields (@cc@, @bcc@, @replyTo@, @tags@) are omitted entirely
-- when empty / Nothing, matching Brevo's wire contract.
encodeRequest ::
  forall command.
  Request command ->
  Text
encodeRequest req =
  do
    let bodyFields = case req.body of
          HtmlBody html ->
            Array.fromLinkedList [("htmlContent", Json.encode html)]
          TextBody text ->
            Array.fromLinkedList [("textContent", Json.encode text)]
          Template { templateId, params } ->
            do
              let paramsJson =
                    params
                      |> Map.entries
                      |> Array.map (\(k, v) -> (k, Json.encode v))
                      |> Array.toLinkedList
                      |> Json.object
              Array.fromLinkedList
                [ ("templateId", Json.encode templateId)
                , ("params", paramsJson)
                ]
    let requiredFields =
          Array.fromLinkedList
            [ ("sender", Json.encode req.sender)
            , ("to", Json.encode req.to)
            , ("subject", Json.encode req.subject)
            ]
    let optionalFields =
          [ optionalArrayField "cc" req.cc
          , optionalArrayField "bcc" req.bcc
          , req.replyTo |> fmap (\r -> ("replyTo", Json.encode r))
          , optionalArrayField "tags" req.tags
          ]
            |> Array.fromLinkedList
            |> Array.getJusts
    let allFields =
          requiredFields
            |> Array.append optionalFields
            |> Array.append bodyFields
            |> Array.toLinkedList
    Json.encodeText (Json.object allFields)


-- | Pair @(name, JSON array)@ when @arr@ is non-empty, otherwise 'Nothing'.
-- Used to omit empty optional array fields from the Brevo request body.
optionalArrayField ::
  forall a.
  Json.ToJSON a =>
  Text ->
  Array a ->
  Maybe (Text, Json.Value)
optionalArrayField fieldName arr =
  case Array.length arr of
    0 -> Nothing
    _ -> Just (fieldName, Json.encode arr)
