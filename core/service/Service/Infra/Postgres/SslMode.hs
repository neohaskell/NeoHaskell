module Service.Infra.Postgres.SslMode (
  SslMode (..),
  sslModeToText,
  textToSslMode,
) where

-- | Core-free home for the WI-5 (#684 / ADR-0064) libpq TLS-mode enum and its
-- token mappings. It imports only granular primitives (never 'Core'), so the
-- low-level 'Service.FileUpload.Core' can carry an 'SslMode' field without
-- forming a Core <-> FileUpload import cycle. The richer
-- 'Service.Infra.Postgres.ConnectionConfig' re-exports all three names, so
-- existing call sites are unaffected.

import Basics
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Text (Text)
import Text qualified


-- | Optional libpq TLS negotiation mode (WI-5 / ADR-0064). 'SslModeUnset'
-- (the 'Default' the pool configs pass) emits no sslmode param, so libpq
-- keeps its compiled-in 'prefer' default and localhost/dev is unchanged.
-- Server-auth only: there is deliberately no client-cert (sslcert/sslkey)
-- field, because Azure Flexible Server has no mTLS and supplying them breaks
-- the connection (ADR-0064 §3).
data SslMode
  = SslModeUnset      -- ^ no sslmode param; libpq default 'prefer' (the default)
  | SslModeDisable    -- ^ sslmode=disable
  | SslModeAllow      -- ^ sslmode=allow
  | SslModePrefer     -- ^ sslmode=prefer (explicit)
  | SslModeRequire    -- ^ sslmode=require — forbids silent downgrade
  | SslModeVerifyCa   -- ^ sslmode=verify-ca
  | SslModeVerifyFull -- ^ sslmode=verify-full — forbids downgrade + MITM
  deriving (Eq, Ord, Show)


-- | Map an 'SslMode' to its libpq sslmode token (WI-5 / ADR-0064).
-- Total over the closed enum. 'SslModeUnset' has NO token — returns
-- 'Nothing', because the unset mode emits no sslmode param at all
-- (ADR-0064 §2). Every other constructor maps to its exact libpq token.
--
-- >>> sslModeToText SslModeRequire
-- Just "require"
-- >>> sslModeToText SslModeVerifyFull
-- Just "verify-full"
-- >>> sslModeToText SslModeUnset
-- Nothing
sslModeToText :: SslMode -> Maybe Text
sslModeToText mode =
  case mode of
    SslModeUnset -> Nothing
    SslModeDisable -> Just "disable"
    SslModeAllow -> Just "allow"
    SslModePrefer -> Just "prefer"
    SslModeRequire -> Just "require"
    SslModeVerifyCa -> Just "verify-ca"
    SslModeVerifyFull -> Just "verify-full"


-- | Parse an operator-supplied env token into an 'SslMode' (WI-5 / ADR-0064).
-- The empty string and "unset" both map to 'SslModeUnset' (default-off).
-- Every libpq token maps to its constructor; anything else is one clean
-- 'Result' error naming the bad value.
--
-- >>> textToSslMode "require"
-- Ok SslModeRequire
-- >>> textToSslMode "unset"
-- Ok SslModeUnset
-- >>> textToSslMode ""
-- Ok SslModeUnset
-- >>> textToSslMode "requre"
-- Err "unknown DB_SSL_MODE \"requre\"; expected one of: unset, disable, allow, prefer, require, verify-ca, verify-full"
textToSslMode :: Text -> Result Text SslMode
textToSslMode input = do
  let trimmedLower = input |> Text.trim |> Text.toLower
  case trimmedLower of
    "" -> Ok SslModeUnset
    "unset" -> Ok SslModeUnset
    "disable" -> Ok SslModeDisable
    "allow" -> Ok SslModeAllow
    "prefer" -> Ok SslModePrefer
    "require" -> Ok SslModeRequire
    "verify-ca" -> Ok SslModeVerifyCa
    "verify-full" -> Ok SslModeVerifyFull
    other -> Err [fmt|unknown DB_SSL_MODE "#{other}"; expected one of: unset, disable, allow, prefer, require, verify-ca, verify-full|]


-- | JSON token form so 'SslMode' config fields round-trip through the
-- declarative config layer (e.g. 'Service.FileUpload.Core.FileStateStoreBackend').
-- 'SslModeUnset' serialises as the literal "unset".
instance Json.ToJSON SslMode where
  toJSON mode =
    sslModeToText mode
      |> Maybe.withDefault "unset"
      |> Json.toJSON


instance Json.FromJSON SslMode where
  parseJSON =
    Json.withText "SslMode" \token ->
      case textToSslMode token of
        Ok mode -> Json.yield mode
        Err err -> Json.fail err
