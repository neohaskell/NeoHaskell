-- | # HTTP Outbound Integration
--
-- This module provides HTTP outbound integration for NeoHaskell,
-- allowing Jess to make HTTP requests to external APIs using
-- pure record configuration.
--
-- == Two-Persona Model
--
-- Following ADR-0008 and ADR-0015, this integration separates concerns:
--
-- * **Jess (Integration User)**: Configures requests with pure records.
--   No @Task@, no @Http.Client@, no retry logic visible.
--
-- * **Nick (Integration Developer)**: Implements 'ToAction' with retries,
--   authentication, and error handling in "Integration.Http.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Http qualified as Http
--
-- orderIntegrations :: Order -> OrderEvent -> Integration.Outbound
-- orderIntegrations order event = case event of
--   OrderShipped info -> Integration.batch
--     [ Integration.outbound Http.Request
--         { method = Http.POST
--         , url = "https://api.shippo.com/v1/shipments"
--         , headers = []
--         , body = Http.json
--             { tracking_number = info.trackingNumber
--             , carrier = info.carrier
--             }
--         , onSuccess = \\response -> NotifyCustomer
--             { orderId = order.id
--             , trackingUrl = response.body
--                 |> Json.get "tracking_url"
--                 |> Maybe.withDefault ""
--             }
--         , onError = Just (\\err -> LogShippingError
--             { orderId = order.id
--             , error = err
--             })
--         , auth = Http.ApiKey "X-Api-Key" "${SHIPPO_API_KEY}"
--         , retry = Http.defaultRetry
--         , timeoutSeconds = 30
--         }
--     ]
--   _ -> Integration.none
-- @
--
-- == Environment Variables
--
-- Secrets should never be hardcoded. Use @${VAR_NAME}@ patterns:
--
-- @
-- auth = Http.Bearer "${API_TOKEN}"
-- url = "https://api.example.com/${API_VERSION}/orders"
-- @
--
-- Missing variables result in 'Integration.AuthenticationError'.
--
-- == Retry Behavior
--
-- By default, requests are retried on transient errors (5xx, 429)
-- with exponential backoff. Use 'defaultRetry', 'noRetry', or
-- 'withRetries' to configure.
--
-- == Known Limitations (v1)
--
-- * PUT, PATCH, DELETE methods return "unsupported" error
-- * Response status codes and headers are placeholders
-- * No streaming support
-- * No OAuth2 token refresh
--
-- These limitations are documented in ADR-0015 and will be
-- addressed in future versions.
module Integration.Http
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Method (..)
  , Body (..)
  , Auth (..)
  , Retry (..)
  , Response (..)

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

import Integration.Http.Auth (Auth (..))
import Integration.Http.Internal ()  -- ToAction instance
import Integration.Http.Request (Body (..), Method (..), Request (..), form, json, noBody, raw)
import Integration.Http.Response (Response (..))
import Integration.Http.Retry (Retry (..), defaultRetry, noRetry, withRetries)
