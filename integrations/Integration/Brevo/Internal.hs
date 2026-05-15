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

import Array qualified
import Basics
import Integration.Brevo.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..))
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
encodeRequest ::
  forall command.
  Request command ->
  Text
encodeRequest req =
  do
    let Sender senderAddress = req.sender
    let senderJson = addressToJson senderAddress
    let toJsonArray = req.to |> Array.map recipientToJson
    let ccJsonArray = req.cc |> Array.map recipientToJson
    let bccJsonArray = req.bcc |> Array.map recipientToJson
    let replyToJson = req.replyTo |> fmap recipientToJson
    let tagsJsonArray = req.tags |> Array.map Json.encode
    let bodyFields = case req.body of
          HtmlBody html ->
            Array.fromLinkedList [("htmlContent", Json.encode html)]
          TextBody text ->
            Array.fromLinkedList [("textContent", Json.encode text)]
          Template { templateId, params } ->
            let paramsJson =
                  params
                    |> Map.entries
                    |> Array.map (\(k, v) -> (k, Json.encode v))
                    |> Array.toLinkedList
                    |> Json.object
            in
              Array.fromLinkedList
                [ ("templateId", Json.encode templateId)
                , ("params", paramsJson)
                ]
    let baseFields =
          Array.fromLinkedList
            [ ("sender", senderJson)
            , ("to", Json.encode toJsonArray)
            , ("subject", Json.encode req.subject)
            , ("cc", Json.encode ccJsonArray)
            , ("bcc", Json.encode bccJsonArray)
            , ("replyTo", Json.encode replyToJson)
            , ("tags", Json.encode tagsJsonArray)
            ]
    let allFields = baseFields |> Array.append bodyFields |> Array.toLinkedList
    Json.encodeText (Json.object allFields)


-- | Encode an Address to a JSON value.
addressToJson :: Address -> Json.Value
addressToJson addr =
  case addr.name of
    Nothing ->
      Json.object
        [ ("email", Json.encode addr.email)
        ]
    Just displayName ->
      Json.object
        [ ("name", Json.encode displayName)
        , ("email", Json.encode addr.email)
        ]


-- | Encode a Recipient to a JSON value by extracting its inner Address.
recipientToJson :: Recipient -> Json.Value
recipientToJson (Recipient addr) =
  addressToJson addr
