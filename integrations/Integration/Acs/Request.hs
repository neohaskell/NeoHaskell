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
  } deriving (Eq, Show, Generic)


-- Stub: generic ToJSON (omits nothing, no custom key mapping).
-- Phase 10 will provide the ACS wire shape (omit name when Nothing).
instance Json.ToJSON Address


-- Stub: always fails so round-trip tests are red.
instance Json.FromJSON Address where
  parseJSON _ = Json.fail "Address.fromJSON stub: not implemented"


-- | The sender address.  A newtype over 'Address' to prevent accidental
-- swap with 'Recipient'.
newtype Sender = Sender Address deriving (Eq, Show)


-- | A recipient address.  A newtype over 'Address' to prevent accidental
-- swap with 'Sender'.
newtype Recipient = Recipient Address deriving (Eq, Show)


-- | Mutually-exclusive email body.  ACS rejects a request that sets both
-- @html@ and @plainText@; the sum enforces the invariant at compile time.
data Body
  = HtmlBody Text
  | TextBody Text
  deriving (Eq, Show, Generic)


-- Stub: generic ToJSON (uses constructor names, not ACS wire keys).
instance Json.ToJSON Body


-- Stub: always fails so round-trip tests are red.
instance Json.FromJSON Body where
  parseJSON _ = Json.fail "Body.fromJSON stub: not implemented"


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
-- Stub: throws to make every test that calls 'sender' red.
sender :: Text -> Sender
sender _email = panic "Acs.sender stub: not implemented"


-- | Build a recipient from just an email address (no display name).
--
-- Stub: throws to make every test that calls 'recipient' red.
recipient :: Text -> Recipient
recipient _email = panic "Acs.recipient stub: not implemented"


-- | Send one transactional email through an ACS Email resource.
-- Reads the Entra Bearer token from @?config.acsAccessToken@.
--
-- Stub: throws to make every test that calls 'send' red.
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
send _endpoint _sender _recipient _subject _body _onSuccess _onError =
  panic "Acs.send stub: not implemented"
