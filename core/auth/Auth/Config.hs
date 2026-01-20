-- | Auth configuration types.
-- AuthConfig is internal (auto-populated from discovery).
-- AuthOverrides is for advanced users who need custom settings.
module Auth.Config (
  -- * Internal configuration (from discovery)
  AuthConfig (..),
  -- * Smart constructor
  mkAuthConfig,
  -- * User-facing overrides
  AuthOverrides (..),
  defaultOverrides,
  -- * Default algorithm allowlist
  defaultAllowedAlgorithms,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Aeson qualified as GhcAeson
import Data.Aeson.Types (Parser)
import Data.Int qualified as GhcInt
import Json (Value)
import Json qualified
import Maybe (Maybe (..))
import Set (Set)
import Set qualified
import Text (Text)


-- | Internal configuration populated from OpenID Connect Discovery.
-- This is NOT exposed to Jess - it's created automatically from the auth server URL.
-- Use 'mkAuthConfig' to construct - it precomputes allowedAlgorithmsSet for O(1) lookups.
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
    allowedAlgorithmsSet :: Set Text,
    -- ^ Precomputed Set for O(1) algorithm lookup (derived from allowedAlgorithms)
    supportedCritHeaders :: Array Text
    -- ^ Understood crit headers (default: empty)
  }
  deriving (Generic, Show)


-- | Custom FromJSON that parses raw fields and reconstructs via mkAuthConfig.
-- This ensures allowedAlgorithmsSet is always recomputed, never trusted from external input.
instance Json.FromJSON AuthConfig where
  parseJSON :: Value -> Parser AuthConfig
  parseJSON = GhcAeson.withObject "AuthConfig" (\obj -> do
    issuer <- obj GhcAeson..: "issuer"
    jwksUri <- obj GhcAeson..: "jwksUri"
    audience <- obj GhcAeson..:? "audience"
    permissionsClaim <- obj GhcAeson..: "permissionsClaim"
    tenantIdClaim <- obj GhcAeson..:? "tenantIdClaim"
    clockSkewSeconds <- obj GhcAeson..: "clockSkewSeconds"
    refreshIntervalSeconds <- obj GhcAeson..: "refreshIntervalSeconds"
    missingKidCooldownSeconds <- obj GhcAeson..: "missingKidCooldownSeconds"
    maxStaleSeconds <- obj GhcAeson..: "maxStaleSeconds"
    allowedAlgorithms <- obj GhcAeson..: "allowedAlgorithms"
    supportedCritHeaders <- obj GhcAeson..: "supportedCritHeaders"
    -- Reconstruct via smart constructor to recompute allowedAlgorithmsSet
    pure (mkAuthConfig
      issuer
      jwksUri
      audience
      permissionsClaim
      tenantIdClaim
      clockSkewSeconds
      refreshIntervalSeconds
      missingKidCooldownSeconds
      maxStaleSeconds
      allowedAlgorithms
      supportedCritHeaders))


-- | Custom ToJSON that serializes only the raw fields, omitting allowedAlgorithmsSet.
-- The precomputed Set is an internal optimization and should never be serialized.
instance Json.ToJSON AuthConfig where
  toJSON :: AuthConfig -> Value
  toJSON config =
    GhcAeson.object
      [ "issuer" GhcAeson..= config.issuer,
        "jwksUri" GhcAeson..= config.jwksUri,
        "audience" GhcAeson..= config.audience,
        "permissionsClaim" GhcAeson..= config.permissionsClaim,
        "tenantIdClaim" GhcAeson..= config.tenantIdClaim,
        "clockSkewSeconds" GhcAeson..= config.clockSkewSeconds,
        "refreshIntervalSeconds" GhcAeson..= config.refreshIntervalSeconds,
        "missingKidCooldownSeconds" GhcAeson..= config.missingKidCooldownSeconds,
        "maxStaleSeconds" GhcAeson..= config.maxStaleSeconds,
        "allowedAlgorithms" GhcAeson..= config.allowedAlgorithms,
        "supportedCritHeaders" GhcAeson..= config.supportedCritHeaders
      ]


-- | Smart constructor that precomputes allowedAlgorithmsSet from allowedAlgorithms.
-- Always use this instead of constructing AuthConfig directly.
mkAuthConfig ::
  Text ->
  -- ^ issuer
  Text ->
  -- ^ jwksUri
  Maybe Text ->
  -- ^ audience
  Text ->
  -- ^ permissionsClaim
  Maybe Text ->
  -- ^ tenantIdClaim
  GhcInt.Int64 ->
  -- ^ clockSkewSeconds
  GhcInt.Int64 ->
  -- ^ refreshIntervalSeconds
  GhcInt.Int64 ->
  -- ^ missingKidCooldownSeconds
  GhcInt.Int64 ->
  -- ^ maxStaleSeconds
  Array Text ->
  -- ^ allowedAlgorithms
  Array Text ->
  -- ^ supportedCritHeaders
  AuthConfig
mkAuthConfig
  issuer
  jwksUri
  audience
  permissionsClaim
  tenantIdClaim
  clockSkewSeconds
  refreshIntervalSeconds
  missingKidCooldownSeconds
  maxStaleSeconds
  allowedAlgorithms
  supportedCritHeaders =
    AuthConfig
      { issuer = issuer,
        jwksUri = jwksUri,
        audience = audience,
        permissionsClaim = permissionsClaim,
        tenantIdClaim = tenantIdClaim,
        clockSkewSeconds = clockSkewSeconds,
        refreshIntervalSeconds = refreshIntervalSeconds,
        missingKidCooldownSeconds = missingKidCooldownSeconds,
        maxStaleSeconds = maxStaleSeconds,
        allowedAlgorithms = allowedAlgorithms,
        allowedAlgorithmsSet = Set.fromArray allowedAlgorithms,
        supportedCritHeaders = supportedCritHeaders
      }


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
    allowedAlgorithms :: Maybe (Array Text),
    -- ^ Override allowed algorithms
    allowedIdpDomains :: Maybe (Array Text)
    -- ^ Restrict IdP to specific domains (e.g., ["auth.example.com", "login.company.eu"])
    -- When set, discovery will reject URLs from non-allowlisted domains.
    -- Default: None (allow any HTTPS URL that passes SSRF checks)
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
      allowedAlgorithms = Nothing,
      allowedIdpDomains = Nothing
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
