-- | Brevo transactional email integration for NeoHaskell.
--
-- Send transactional emails (sign-up confirmations, password resets,
-- receipts, notifications) via Brevo's REST API.
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Brevo qualified as Brevo
--
-- Brevo.send
--   (Brevo.sender "noreply@myapp.com")
--   (Brevo.recipient "user@example.com")
--   "Welcome to MyApp"
--   (Brevo.HtmlBody "<h1>Welcome</h1>")
--   (\\response -> EmailSent { messageId = response.messageId })
--   (\\err -> EmailFailed { reason = err })
--   |> Integration.outbound
-- @
--
-- == Configuration
--
-- Add to your @Config.hs@:
--
-- @
-- Config.field @(Redacted Text) "brevoApiKey"
--   |> Config.doc "Brevo transactional email API key"
--   |> Config.required
--   |> Config.envVar "BREVO_API_KEY"
--   |> Config.secret
-- @
module Integration.Brevo
  ( -- * Request configuration (Jess's API)
    Request (..)
  , Body (..)

    -- * Address shape and the two role-distinct newtypes over it
  , Address (..)
  , Sender (..)
  , Recipient (..)

    -- * Smart constructors
  , send
  , sender
  , recipient

    -- * Response type
  , Response (..)
  ) where

import Integration.Brevo.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..), recipient, send, sender)
import Integration.Brevo.Response (Response (..))
