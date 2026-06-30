{-# LANGUAGE ImplicitParams #-}

-- | Request types for ACS transactional email integration.
--
-- Provides the 'Request' record and 'Body' sum type that Jess uses
-- to configure email sends via Azure Communication Services.
module Integration.Acs.Request
  ( -- * Core types
    Request (..)
  , Body (..)
  , Address (..)
  , Sender (..)
  , Recipient (..)

    -- * Smart constructors
  , sender
  , recipient
  , send
  ) where

import Array (Array)
import Array qualified
import Basics
import Integration.Acs.Response (Response)
import Json qualified
import Maybe (Maybe (..))
import Redacted (Redacted)
import Text (Text)


-- | A wire-format email address: optional display name + email string.
data Address = Address
  { name :: Maybe Text
  , email :: Text
  } deriving (Eq, Generic)


-- | 'Show' redacts the address so recipient/sender PII never lands in logs
-- or error text via @show@. (Equality and JSON still use the real fields.)
instance Show Address where
  show _ = "Address <redacted>"


-- | Hand-written ToJSON: omit the @name@ field entirely when no display
-- name is supplied, rather than serialising @null@.  This matches ACS wire
-- contract and mirrors the Brevo pattern.
instance Json.ToJSON Address where
  toJSON addr =
    case addr.name of
      Nothing ->
        Json.object [("email", Json.encode addr.email)]
      Just displayName ->
        Json.object
          [ ("name", Json.encode displayName)
          , ("email", Json.encode addr.email)
          ]


-- | Generic FromJSON: Aeson's generic decoder treats a missing @name@ field
-- as 'Nothing', which round-trips correctly with the custom 'ToJSON'.
instance Json.FromJSON Address


-- | The sender address.  A newtype over 'Address' to prevent accidental
-- swap with 'Recipient'.  'Show' redacts the email (no PII leak).
newtype Sender = Sender Address deriving (Eq)


instance Show Sender where
  show _ = "Sender <redacted>"


-- | A recipient address.  A newtype over 'Address' to prevent accidental
-- swap with 'Sender'.  'Show' redacts the email (no PII leak).
newtype Recipient = Recipient Address deriving (Eq)


instance Show Recipient where
  show _ = "Recipient <redacted>"


-- | Mutually-exclusive email body.  ACS rejects a request that sets both
-- @html@ and @plainText@; the sum enforces the invariant at compile time.
data Body
  = HtmlBody Text
  | TextBody Text
  deriving (Eq, Show, Generic)


-- | Generic ToJSON for round-trip testing.  The ACS wire body is hand-built
-- by 'Internal.encodeBodyField'; this instance is only used in tests.
instance Json.ToJSON Body


-- | Generic FromJSON for round-trip testing.
instance Json.FromJSON Body


-- | A complete ACS email send, produced by 'send' and consumed by
-- 'Integration.outbound'.  'accessToken' is 'Redacted Text' so the Entra
-- Bearer token is never logged or serialized by accident.
data Request command = Request
  { endpoint :: Text
  , sender :: Sender
  , to :: Array Recipient
  , subject :: Text
  , body :: Body
  , accessToken :: Redacted Text
  , onSuccess :: Response -> command
  , onError :: Text -> command
  }


-- | Build a sender from just an email address (no display name).
--
-- @
-- Acs.sender "noreply\@myapp.com"
-- @
sender :: Text -> Sender
sender email =
  Sender (Address { name = Nothing, email })


-- | Build a recipient from just an email address (no display name).
--
-- @
-- Acs.recipient "user\@example.com"
-- @
recipient :: Text -> Recipient
recipient email =
  Recipient (Address { name = Nothing, email })


-- | Send one transactional email through an ACS Email resource.
-- Reads the Entra Bearer token from @?config.acsAccessToken@.
-- The endpoint must use @https://@ — a non-https endpoint is reported
-- through the error callback without ever unwrapping the token.
--
-- @
-- Acs.send "https://my-acs.communication.azure.com"
--   (Acs.sender "noreply\@myapp.com") (Acs.recipient info.email)
--   "Your order is confirmed" (Acs.HtmlBody "\<h1\>Thanks\<\/h1\>")
--   (\\r -> EmailSent { operationId = r.operationId }) (\\e -> EmailFailed { reason = e })
--   |> Integration.outbound
-- @
send ::
  forall command config.
  ( ?config :: config
  , HasField "acsAccessToken" config (Redacted Text)
  ) =>
  Text ->
  Sender ->
  Recipient ->
  Text ->
  Body ->
  (Response -> command) ->
  (Text -> command) ->
  Request command
send endpointVal senderVal recipientVal subjectVal bodyVal onSuccess onError =
  Request
    { endpoint = endpointVal
    , sender = senderVal
    , to = Array.wrap recipientVal
    , subject = subjectVal
    , body = bodyVal
    , accessToken = ?config.acsAccessToken
    , onSuccess
    , onError
    }
