-- | Azure Communication Services transactional email integration for NeoHaskell.
--
-- Send transactional emails (sign-up confirmations, password resets, receipts,
-- notifications) via an ACS Email resource.
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Acs qualified as Acs
--
-- Acs.send
--   "https://my-acs.communication.azure.com"
--   (Acs.sender "noreply@myapp.com")
--   (Acs.recipient "user@example.com")
--   "Your order is confirmed"
--   (Acs.HtmlBody "<h1>Thank you</h1>")
--   (\\r -> EmailSent { operationId = r.operationId })
--   (\\e -> EmailFailed { reason = e })
--   |> Integration.outbound
-- @
--
-- == Configuration
--
-- Add to your @Config.hs@:
--
-- @
-- Config.field @(Redacted Text) "acsAccessToken"
--   |> Config.doc "ACS Entra ID Bearer token (scope https://communication.azure.com/.default)"
--   |> Config.required
--   |> Config.envVar "ACS_ACCESS_TOKEN"
--   |> Config.secret
-- @
module Integration.Acs
  ( -- * Address types
    Address (..)
  , Sender (..)
  , Recipient (..)

    -- * Construction — build the address values
  , sender
  , recipient

    -- * Email body — mutually-exclusive content variant
  , Body (..)

    -- * Operation — send one transactional email
  , send
  , Request (..)

    -- * Response — the accepted-send payload
  , Response (..)
  ) where

import Integration.Acs.Internal ()  -- ToAction (Request command) instance
import Integration.Acs.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..), recipient, send, sender)
import Integration.Acs.Response (Response (..))
