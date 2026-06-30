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
import Basics
import Integration (ToAction (..))
import Integration.Acs.Request (Body (..), Request (..), Recipient (..), Sender (..))
import Integration.Acs.Response (Response (..))
import Integration.Http qualified as Http
import Json qualified
import Result (Result)
import Service.Command.Core (NameOf)
import Text (Text)


-- | Internal record bundling the onSuccess and onError callbacks from the Request.
data AcsResponseHandler command = AcsResponseHandler
  { onSuccess :: Response -> command
  , onError :: Text -> command
  }


-- | ToAction instance: validate endpoint (SEC-001) before unwrapping token.
--
-- Stub: throws to make every test that calls 'toAction' red.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  ToAction (Request command)
  where
  toAction _req = panic "Acs.Internal.toAction stub: not implemented"


-- | Validate that the endpoint uses the https scheme.
--
-- Stub: throws to make every test that calls 'validateEndpoint' red.
validateEndpoint :: Text -> Result Text Text
validateEndpoint _endpoint = panic "Acs.Internal.validateEndpoint stub: not implemented"


-- | Convert a validated Request into an Http.Request.
-- The single unwrap site for the Entra Bearer token.
--
-- Stub: throws to make every test that calls 'toHttpRequest' red.
toHttpRequest ::
  forall command.
  Request command ->
  Http.Request command
toHttpRequest _req = panic "Acs.Internal.toHttpRequest stub: not implemented"


-- | Encode a Request into ACS-compatible JSON wire format.
--
-- Stub: throws to make every test that calls 'encodeRequest' red.
encodeRequest ::
  forall command.
  Request command ->
  Text
encodeRequest _req = panic "Acs.Internal.encodeRequest stub: not implemented"


-- | Pick the ACS body key for the chosen Body variant.
--
-- Stub: throws to make every test that calls 'encodeBodyField' red.
encodeBodyField :: Body -> Array (Text, Json.Value)
encodeBodyField _body = panic "Acs.Internal.encodeBodyField stub: not implemented"


-- | Encode a Recipient as a JSON object { \"address\": \"...\" }.
--
-- Stub: throws to make every test that calls 'encodeRecipientAddress' red.
encodeRecipientAddress :: Recipient -> Json.Value
encodeRecipientAddress _recipient = panic "Acs.Internal.encodeRecipientAddress stub: not implemented"


-- | Extract the email address from a Sender.
--
-- Stub: throws to make every test that calls 'senderEmail' red.
senderEmail :: Sender -> Text
senderEmail _sender = panic "Acs.Internal.senderEmail stub: not implemented"


-- | Dispatch the ACS HTTP response on status code.
-- 202 Accepted always succeeds (SEC-002).
--
-- Stub: throws to make every test that calls 'handleAcsResponse' red.
handleAcsResponse ::
  forall command.
  AcsResponseHandler command ->
  Http.Response ->
  command
handleAcsResponse _handler _response = panic "Acs.Internal.handleAcsResponse stub: not implemented"


-- | Best-effort ACS operation id from the response body or Operation-Location header.
--
-- Stub: throws to make every test that calls 'operationIdFrom' red.
operationIdFrom :: Http.Response -> Text
operationIdFrom _response = panic "Acs.Internal.operationIdFrom stub: not implemented"


-- | Parse the operation id from an Operation-Location header value.
--
-- Stub: throws to make every test that calls 'operationIdFromLocation' red.
operationIdFromLocation :: Text -> Text
operationIdFromLocation _location = panic "Acs.Internal.operationIdFromLocation stub: not implemented"
