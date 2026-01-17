-- | Authentication error types.
-- AuthError is request-facing; DiscoveryError is control-plane.
module Auth.Error (
  -- * Request-facing errors
  AuthError (..),
  -- * Control-plane errors
  DiscoveryError (..),
) where

import Array (Array)
import Basics
import Json qualified
import Text (Text)


-- | Errors that can occur during authentication.
-- These are mapped to HTTP responses by the middleware.
-- Note: Error responses use generic messages to prevent information leakage.
data AuthError
  = -- | No Authorization header
    TokenMissing
  | -- | Not a valid JWT format
    TokenMalformed Text
  | -- | exp claim in the past
    TokenExpired
  | -- | nbf claim in the future
    TokenNotYetValid
  | -- | Signature doesn't verify
    SignatureInvalid
  | -- | alg not in allowlist (RFC 8725)
    AlgorithmNotAllowed Text
  | -- | Unknown crit header (RFC 8725)
    UnsupportedCritHeader (Array Text)
  | -- | Key type doesn't match alg (RFC 8725)
    KeyAlgorithmMismatch
  | -- | Expected vs actual issuer
    IssuerMismatch Text Text
  | -- | Expected vs actual audience
    AudienceMismatch Text (Array Text)
  | -- | JWKS/keys unavailable (503, not 401)
    AuthInfraUnavailable Text
  | -- | Key ID not found in JWKS
    KeyNotFound Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON AuthError


instance Json.ToJSON AuthError


-- | Errors during OpenID Connect Discovery or JWKS fetch.
-- These happen during startup or background refresh, not per-request.
data DiscoveryError
  = -- | Failed to fetch discovery document
    DiscoveryFetchFailed Text
  | -- | Failed to parse discovery document
    DiscoveryParseFailed Text
  | -- | Failed to fetch JWKS
    JwksFetchFailed Text
  | -- | Failed to parse JWKS
    JwksParseFailed Text
  | -- | Discovery endpoint returned invalid data
    InvalidDiscoveryDocument Text
  | -- | URL validation failed (HTTPS required, SSRF blocked)
    UrlValidationFailed Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON DiscoveryError


instance Json.ToJSON DiscoveryError
