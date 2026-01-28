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
    -- ** PUT/PATCH/DELETE Examples
  , updateUserProfileIntegration
  , partialUpdateOrderIntegration
  , cancelSubscriptionIntegration
    -- ** Status Code & Headers Examples
  , conditionalUpdateIntegration
    -- ** Raw Body Example
  , xmlOrderIntegration
    -- * Example Command Types (for demonstration)
  , ShippingResult (..)
  , RefundResult (..)
  , SlackResult (..)
  , ProfileUpdateResult (..)
  , OrderPatchResult (..)
  , SubscriptionResult (..)
  , ConditionalResult (..)
  , XmlOrderResult (..)
  ) where

import Array qualified
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


-- ============================================================================
-- PUT/PATCH/DELETE Examples (Loop 7 - Fixing Workarounds)
-- ============================================================================

-- | Result of profile update (PUT).
data ProfileUpdateResult
  = ProfileUpdated
      { userId :: Text
      }
  | ProfileUpdateFailed
      { userId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON ProfileUpdateResult
instance Json.FromJSON ProfileUpdateResult

type instance NameOf ProfileUpdateResult = "ProfileUpdateResult"


-- | Result of order patch (PATCH).
data OrderPatchResult
  = OrderPatched
      { orderId :: Text
      , updatedFields :: Array Text
      }
  | OrderPatchFailed
      { orderId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON OrderPatchResult
instance Json.FromJSON OrderPatchResult

type instance NameOf OrderPatchResult = "OrderPatchResult"


-- | Result of subscription cancellation (DELETE).
data SubscriptionResult
  = SubscriptionCancelled
      { subscriptionId :: Text
      }
  | SubscriptionCancelFailed
      { subscriptionId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON SubscriptionResult
instance Json.FromJSON SubscriptionResult

type instance NameOf SubscriptionResult = "SubscriptionResult"


-- | Result of conditional update (uses status code and headers).
data ConditionalResult
  = ResourceUpdated
      { resourceId :: Text
      , newEtag :: Text
      }
  | ResourceNotModified
      { resourceId :: Text
      }
  | ConditionalUpdateFailed
      { resourceId :: Text
      , statusCode :: Int
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON ConditionalResult
instance Json.FromJSON ConditionalResult

type instance NameOf ConditionalResult = "ConditionalResult"


-- | Result of XML order submission.
data XmlOrderResult
  = XmlOrderAccepted
      { orderId :: Text
      }
  | XmlOrderRejected
      { errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON XmlOrderResult
instance Json.FromJSON XmlOrderResult

type instance NameOf XmlOrderResult = "XmlOrderResult"


-- | Example: Full update of user profile (PUT).
--
-- PUT replaces the entire resource. This shows:
--
-- * PUT method for full resource replacement
-- * Bearer token authentication
-- * Error handling for conflicts
updateUserProfileIntegration ::
  -- | User ID
  Text ->
  -- | New display name
  Text ->
  -- | New email
  Text ->
  Integration.Action
updateUserProfileIntegration userId displayName email =
  Integration.outbound Http.Request
    { method = Http.PUT
    , url = [fmt|https://api.example.com/users/#{userId}|]
    , headers = []
    , body = Http.json UserProfile
        { display_name = displayName
        , email = email
        }
    , onSuccess = \_response ->
        ProfileUpdated { userId = userId }
    , onError = Just (\err ->
        ProfileUpdateFailed
          { userId = userId
          , errorMessage = err
          })
    , auth = Http.Bearer "${API_TOKEN}"
    , retry = Http.defaultRetry
    , timeoutSeconds = 30
    }


-- | Payload for user profile
data UserProfile = UserProfile
  { display_name :: Text
  , email :: Text
  }
  deriving (Generic, Show)

instance Json.ToJSON UserProfile


-- | Example: Partial update of order (PATCH).
--
-- PATCH updates only specified fields. This shows:
--
-- * PATCH method for partial updates
-- * API key authentication
-- * Extracting updated field list from response
partialUpdateOrderIntegration ::
  -- | Order ID
  Text ->
  -- | New status
  Text ->
  Integration.Action
partialUpdateOrderIntegration orderId newStatus =
  Integration.outbound Http.Request
    { method = Http.PATCH
    , url = [fmt|https://api.example.com/orders/#{orderId}|]
    , headers = []
    , body = Http.json OrderPatch
        { status = newStatus
        }
    , onSuccess = \response ->
        OrderPatched
          { orderId = orderId
          , updatedFields = response.body
              |> Json.decode @PatchResponse
              |> Result.map (.updated_fields)
              |> Result.withDefault []
          }
    , onError = Just (\err ->
        OrderPatchFailed
          { orderId = orderId
          , errorMessage = err
          })
    , auth = Http.ApiKey { headerName = "X-Api-Key", headerValue = "${ORDER_API_KEY}" }
    , retry = Http.defaultRetry
    , timeoutSeconds = 30
    }


-- | Payload for order patch
data OrderPatch = OrderPatch
  { status :: Text
  }
  deriving (Generic, Show)

instance Json.ToJSON OrderPatch


-- | Response from patch API
data PatchResponse = PatchResponse
  { updated_fields :: Array Text
  }
  deriving (Generic, Show)

instance Json.FromJSON PatchResponse


-- | Example: Cancel subscription (DELETE).
--
-- DELETE removes a resource. This shows:
--
-- * DELETE method
-- * No request body
-- * Basic authentication
cancelSubscriptionIntegration ::
  -- | Subscription ID
  Text ->
  Integration.Action
cancelSubscriptionIntegration subscriptionId =
  Integration.outbound Http.Request
    { method = Http.DELETE
    , url = [fmt|https://api.stripe.com/v1/subscriptions/#{subscriptionId}|]
    , headers = []
    , body = Http.noBody
    , onSuccess = \_response ->
        SubscriptionCancelled { subscriptionId = subscriptionId }
    , onError = Just (\err ->
        SubscriptionCancelFailed
          { subscriptionId = subscriptionId
          , errorMessage = err
          })
    , auth = Http.Basic { username = "${STRIPE_KEY}", password = "" }
    , retry = Http.withRetries 3
    , timeoutSeconds = 30
    }


-- | Example: Conditional update with ETag (uses status code and headers).
--
-- This demonstrates:
--
-- * Checking real status code (304 Not Modified vs 200 OK)
-- * Extracting ETag from response headers
-- * Using If-Match header for optimistic locking
conditionalUpdateIntegration ::
  -- | Resource ID
  Text ->
  -- | Current ETag
  Text ->
  -- | New data
  Text ->
  Integration.Action
conditionalUpdateIntegration resourceId currentEtag newData =
  Integration.outbound Http.Request
    { method = Http.PUT
    , url = [fmt|https://api.example.com/resources/#{resourceId}|]
    , headers = [("If-Match", currentEtag)]
    , body = Http.json ResourceData { data_field = newData }
    , onSuccess = \response ->
        -- Check status code to determine outcome
        if response.statusCode == 304
          then ResourceNotModified { resourceId = resourceId }
          else ResourceUpdated
            { resourceId = resourceId
            , newEtag = response.headers
                |> findHeader "ETag"
                |> Result.withDefault ""
            }
    , onError = Just (\err ->
        ConditionalUpdateFailed
          { resourceId = resourceId
          , statusCode = 0  -- Unknown on error
          , errorMessage = err
          })
    , auth = Http.Bearer "${API_TOKEN}"
    , retry = Http.noRetry  -- Don't retry conditional updates
    , timeoutSeconds = 30
    }


-- | Payload for resource update
data ResourceData = ResourceData
  { data_field :: Text
  }
  deriving (Generic, Show)

instance Json.ToJSON ResourceData


-- | Find a header by name (case-insensitive).
findHeader :: Text -> Array (Text, Text) -> Result Text Text
findHeader name headers =
  headers
    |> Array.find (\(k, _) -> Text.toLower k == Text.toLower name)
    |> Result.fromMaybe "Header not found"
    |> Result.map (\(_, v) -> v)


-- | Example: Submit order as XML (raw body).
--
-- Some legacy APIs require XML. This shows:
--
-- * Raw body with custom content type
-- * XML payload
xmlOrderIntegration ::
  -- | Order ID
  Text ->
  -- | Product code
  Text ->
  -- | Quantity
  Int ->
  Integration.Action
xmlOrderIntegration orderId productCode quantity =
  Integration.outbound Http.Request
    { method = Http.POST
    , url = "https://legacy-api.example.com/orders"
    , headers = []
    , body = Http.raw "application/xml" (buildXmlOrder orderId productCode quantity)
    , onSuccess = \_response ->
        XmlOrderAccepted { orderId = orderId }
    , onError = Just (\err ->
        XmlOrderRejected { errorMessage = err })
    , auth = Http.Basic { username = "${LEGACY_USER}", password = "${LEGACY_PASS}" }
    , retry = Http.defaultRetry
    , timeoutSeconds = 60
    }


-- | Build XML order payload.
buildXmlOrder :: Text -> Text -> Int -> Text
buildXmlOrder orderId productCode quantity =
  [fmt|<?xml version="1.0" encoding="UTF-8"?>
<order>
  <id>#{orderId}</id>
  <product>#{productCode}</product>
  <quantity>#{Text.fromInt quantity}</quantity>
</order>|]
