-- | Auth configuration types.
-- AuthConfig is internal (auto-populated from discovery).
-- AuthOverrides is for advanced users who need custom settings.
module Auth.Config (
  -- * Internal configuration (from discovery)
  AuthConfig (..),
  -- * User-facing overrides
  AuthOverrides (..),
  defaultOverrides,
  -- * Default algorithm allowlist
  defaultAllowedAlgorithms,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Int qualified as GhcInt
import Json qualified
import Maybe (Maybe (..))
import Text (Text)


-- | Internal configuration populated from OpenID Connect Discovery.
-- This is NOT exposed to Jess - it's created automatically from the auth server URL.
data AuthConfig = AuthConfig
  { issuer :: Text,
    -- ^ From discovery: iss claim validation
    jwksUri :: Text,
    -- ^ From discovery: where to fetch public keys
    audience :: Maybe Text,
    -- ^ Optional: aud claim validation
    permissionsClaim :: Text,
    -- ^ Claim name for permissions (default: "permissions")
    tenantIdClaim :: Maybe Text,
    -- ^ Claim name for tenant ID (optional)
    clockSkewSeconds :: GhcInt.Int64,
    -- ^ Tolerance for exp/nbf validation (default: 60)
    -- Operational settings for 50k req/s
    refreshIntervalSeconds :: GhcInt.Int64,
    -- ^ Key refresh interval (default: 900 = 15 min)
    missingKidCooldownSeconds :: GhcInt.Int64,
    -- ^ Cooldown per kid miss (default: 60)
    maxStaleSeconds :: GhcInt.Int64,
    -- ^ Max staleness before 503 (default: 86400 = 24h)
    -- RFC 8725 hardening
    allowedAlgorithms :: Array Text,
    -- ^ Explicit allowlist (default: ES256, RS256, etc.)
    supportedCritHeaders :: Array Text
    -- ^ Understood crit headers (default: empty)
  }
  deriving (Generic, Show)


instance Json.FromJSON AuthConfig


instance Json.ToJSON AuthConfig


-- | Optional overrides for advanced users.
-- Most fields use sensible defaults; only override if you have special requirements.
data AuthOverrides = AuthOverrides
  { audience :: Maybe Text,
    -- ^ Override aud claim validation (default: None)
    permissionsClaim :: Maybe Text,
    -- ^ Override permissions claim name (default: "permissions")
    tenantIdClaim :: Maybe Text,
    -- ^ Claim name for tenant ID (default: None)
    clockSkewSeconds :: Maybe GhcInt.Int64,
    -- ^ Override clock skew tolerance (default: 60)
    allowedAlgorithms :: Maybe (Array Text)
    -- ^ Override allowed algorithms
  }
  deriving (Generic, Show)


instance Json.FromJSON AuthOverrides


instance Json.ToJSON AuthOverrides


-- | Default overrides (all Nothing - use discovered/default values).
defaultOverrides :: AuthOverrides
defaultOverrides =
  AuthOverrides
    { audience = Nothing,
      permissionsClaim = Nothing,
      tenantIdClaim = Nothing,
      clockSkewSeconds = Nothing,
      allowedAlgorithms = Nothing
    }


-- | Default allowed algorithms per RFC 8725.
-- Excludes 'none' and symmetric algorithms for public key validation.
defaultAllowedAlgorithms :: Array Text
defaultAllowedAlgorithms =
  Array.fromLinkedList
    [ "ES256",
      "ES384",
      "ES512",
      "EdDSA",
      "RS256",
      "RS384",
      "RS512"
    ]
