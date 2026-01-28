-- | Example HTTP outbound integration for testbed.
--
-- This module demonstrates how Jess (Integration User) would use the
-- HTTP integration to send order data to external APIs.
--
-- == Usage Pattern
--
-- Jess configures HTTP requests using pure records:
--
-- @
-- Integration.outbound Http.Request
--   { method = Http.POST
--   , url = "https://api.example.com/orders"
--   , body = Http.json orderData
--   , onSuccess = \\response -> ConfirmOrder { ... }
--   , ...
--   }
-- @
--
-- No @Task@, no @Http.Client@, no retry logic visible to Jess.
--
-- == Note
--
-- This module demonstrates the API design. The actual commands
-- (NotifyCustomer, LogShippingError, etc.) would be defined in
-- their respective domain modules using the @command ''CommandName@ TH.
module Testbed.Examples.HttpIntegration
  ( -- * Example Integration Builders
    orderShippingIntegration
  , paymentRefundIntegration
  , slackNotificationIntegration
    -- * Example Command Types (for demonstration)
  , ShippingResult (..)
  , RefundResult (..)
  , SlackResult (..)
  ) where

import Core
import Integration qualified
import Integration.Http qualified as Http
import Json qualified
import Result qualified
import Text qualified


-- ============================================================================
-- Example Commands
-- These are simplified commands for demonstration purposes.
-- In a real application, these would be defined in domain modules with
-- proper EntityOf instances and command handlers.
-- ============================================================================

-- | Result of shipping integration.
--
-- Since onSuccess and onError must return the same command type,
-- we use a sum type to represent all possible outcomes.
data ShippingResult
  = CustomerNotified
      { orderId :: Text
      , trackingUrl :: Text
      }
  | ShippingFailed
      { orderId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON ShippingResult
instance Json.FromJSON ShippingResult


-- | Result of refund integration.
data RefundResult
  = RefundRecorded
      { orderId :: Text
      , refundId :: Text
      }
  | RefundFailed
      { orderId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON RefundResult
instance Json.FromJSON RefundResult


-- | Result of Slack notification.
--
-- For fire-and-forget webhooks, we often don't care about failures,
-- so this is kept simple.
data SlackResult
  = SlackMessageSent
  deriving (Generic, Show, Typeable)

instance Json.ToJSON SlackResult
instance Json.FromJSON SlackResult


-- Type-level name instances required by Integration.ToAction
type instance NameOf ShippingResult = "ShippingResult"
type instance NameOf RefundResult = "RefundResult"
type instance NameOf SlackResult = "SlackResult"




-- ============================================================================
-- Example Integrations (Jess's code)
-- ============================================================================

-- | Example: Notify shipping provider when order ships.
--
-- This shows the full configuration with all fields:
--
-- * POST request to external API
-- * JSON body with order data
-- * API key authentication from environment variable
-- * Default retry policy (3 attempts, exponential backoff)
-- * Success callback that emits a domain command
-- * Error callback for logging failures
orderShippingIntegration ::
  -- | Order ID
  Text ->
  -- | Tracking number
  Text ->
  -- | Carrier name
  Text ->
  -- | Customer email
  Text ->
  Integration.Action
orderShippingIntegration orderId trackingNumber carrier customerEmail =
  Integration.outbound Http.Request
    { method = Http.POST
    , url = "https://api.shippo.com/v1/shipments"
    , headers = []
    , body = Http.json ShipmentPayload
        { tracking_number = trackingNumber
        , carrier = carrier
        , recipient_email = customerEmail
        }
    , onSuccess = \response ->
        CustomerNotified
          { orderId = orderId
          , trackingUrl = response.body
              |> Json.decode @TrackingResponse
              |> Result.map (.tracking_url_provider)
              |> Result.withDefault ""
          }
    , onError = Just (\err ->
        ShippingFailed
          { orderId = orderId
          , errorMessage = err
          })
    , auth = Http.ApiKey { headerName = "X-Api-Key", headerValue = "${SHIPPO_API_KEY}" }
    , retry = Http.defaultRetry
    , timeoutSeconds = 30
    }


-- | Payload for shipping API
data ShipmentPayload = ShipmentPayload
  { tracking_number :: Text
  , carrier :: Text
  , recipient_email :: Text
  }
  deriving (Generic, Show)

instance Json.ToJSON ShipmentPayload


-- | Response from shipping API
data TrackingResponse = TrackingResponse
  { tracking_url_provider :: Text
  }
  deriving (Generic, Show)

instance Json.FromJSON TrackingResponse


-- | Example: Process refund through payment provider.
--
-- This shows:
--
-- * Form-encoded body (typical for Stripe)
-- * Bearer token authentication
-- * Custom retry count (5 attempts)
-- * No error callback (let integration fail and retry via dispatcher)
paymentRefundIntegration ::
  -- | Order ID
  Text ->
  -- | Payment intent ID
  Text ->
  -- | Amount in cents
  Int ->
  Integration.Action
paymentRefundIntegration orderId paymentIntentId amountCents =
  Integration.outbound Http.Request
    { method = Http.POST
    , url = "https://api.stripe.com/v1/refunds"
    , headers = []
    , body = Http.form
        [ ("payment_intent", paymentIntentId)
        , ("amount", Text.fromInt amountCents)
        ]
    , onSuccess = \_response ->
        RefundRecorded
          { orderId = orderId
          , refundId = ""  -- Would extract from response.body in real impl
          }
    , onError = Nothing  -- Let it fail and retry
    , auth = Http.Bearer "${STRIPE_SECRET_KEY}"
    , retry = Http.withRetries 5
    , timeoutSeconds = 60
    }


-- | Example: Fire-and-forget Slack notification.
--
-- This shows:
--
-- * Simple webhook (no auth - URL contains token)
-- * No retries (fire and forget)
-- * Short timeout
-- * Minimal configuration
slackNotificationIntegration ::
  -- | Message text
  Text ->
  -- | Channel name
  Text ->
  Integration.Action
slackNotificationIntegration messageText channel =
  Integration.outbound Http.Request
    { method = Http.POST
    , url = "https://hooks.slack.com/services/${SLACK_WEBHOOK_PATH}"
    , headers = [("Content-Type", "application/json")]
    , body = Http.json SlackPayload
        { text = messageText
        , channel = channel
        }
    , onSuccess = \_response -> SlackMessageSent
    , onError = Nothing  -- Fire and forget
    , auth = Http.NoAuth
    , retry = Http.noRetry
    , timeoutSeconds = 10
    }


-- | Payload for Slack webhook
data SlackPayload = SlackPayload
  { text :: Text
  , channel :: Text
  }
  deriving (Generic, Show)

instance Json.ToJSON SlackPayload
