{-# LANGUAGE ImplicitParams #-}

-- | Request types for Brevo transactional email integration.
--
-- Provides the 'Request' record and 'Body' sum type that Jess uses
-- to configure email sends.
module Integration.Brevo.Request
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
import Integration.Brevo.Response (Response)
import Json qualified
import Map (Map)
import Maybe (Maybe (..))
import Redacted (Redacted)
import Text (Text)


-- | A wire-format email address: optional display name + email string.
data Address = Address
  { name :: Maybe Text
  , email :: Text
  } deriving (Eq, Show, Generic)


instance Json.ToJSON Address
instance Json.FromJSON Address


-- | The sender address of a transactional email. A newtype over 'Address'
-- to prevent accidental swap with 'Recipient'.
newtype Sender = Sender Address deriving (Eq, Show)


-- | A recipient address (one of: to, cc, bcc, replyTo). A newtype over
-- 'Address' to prevent accidental swap with 'Sender'.
newtype Recipient = Recipient Address deriving (Eq, Show)


-- | Mutually-exclusive email body type. Brevo refuses requests that combine
-- htmlContent, textContent, and templateId; this sum type enforces the
-- invariant at compile time.
data Body
  = HtmlBody Text
  | TextBody Text
  | Template { templateId :: Int, params :: Map Text Text }
  deriving (Eq, Show, Generic)


instance Json.ToJSON Body
instance Json.FromJSON Body


-- | Complete Brevo SMTP email request. The 'apiKey' field is 'Redacted Text'
-- to prevent accidental logging or serialization of the secret.
data Request command = Request
  { sender :: Sender
  , to :: Array Recipient
  , subject :: Text
  , body :: Body
  , cc :: Array Recipient
  , bcc :: Array Recipient
  , replyTo :: Maybe Recipient
  , tags :: Array Text
  , apiKey :: Redacted Text
  , onSuccess :: Response -> command
  , onError :: Text -> command
  } deriving (Generic)


-- | Build a sender from just an email address (no display name).
sender :: Text -> Sender
sender email = Sender (Address { name = Nothing, email })


-- | Build a recipient from just an email address (no display name).
recipient :: Text -> Recipient
recipient email = Recipient (Address { name = Nothing, email })


-- | Send a transactional email with a single sender and a single recipient.
--
-- Reads the API key from the user's project config via the implicit
-- ?config parameter.
send ::
  forall command config.
  ( ?config :: config
  , HasField "brevoApiKey" config (Redacted Text)
  ) =>
  Sender ->
  Recipient ->
  Text ->
  Body ->
  (Response -> command) ->
  (Text -> command) ->
  Request command
send senderVal recipientVal subjectVal bodyVal onSuccess onError =
  Request
    { sender = senderVal
    , to = Array.wrap recipientVal
    , subject = subjectVal
    , body = bodyVal
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = ?config.brevoApiKey
    , onSuccess
    , onError
    }
