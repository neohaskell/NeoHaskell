-- NOTE: -Wno-deprecations suppresses warnings from the jose library which
-- deprecates some lens-based APIs we use. These are stable and well-tested;
-- we'll migrate when jose provides non-deprecated alternatives.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | JWT token validation.
-- This module handles parsing and validating JWT tokens against
-- the auth configuration. It implements RFC 8725 JOSE hardening.
module Auth.Jwt (
  -- * Token validation
  validateToken,
  validateTokenWithJwkSet,
  validateTokenWithParsedHeader,
  -- * Token parsing
  extractKidFromToken,
  -- * Optimized header parsing (parse once, use for all checks)
  ParsedHeader (..),
  parseHeader,
  -- * Internal (for testing)
  parseTokenHeader,
  extractAlgorithmFromToken,
) where

import Array (Array)
import Array qualified
import Auth.Claims (UserClaims (..))
import Auth.Config (AuthConfig (..))
import Auth.Error (AuthError (..))
import Basics
import Control.Lens qualified as Lens
import Crypto.JOSE qualified as Jose
import Crypto.JWT qualified as JWT
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64.URL qualified as GhcBase64
import Data.ByteString.Lazy qualified as GhcLBS
import Data.Foldable qualified as GhcFoldable
import Data.Aeson.Key qualified as GhcAesonKey
import Data.Aeson.KeyMap qualified as GhcKeyMap
import Data.Map.Strict qualified as GhcMap
import Data.Text qualified as GhcText
import Data.Text.Encoding qualified as GhcTextEncoding
import Network.URI qualified as GhcURI
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Prelude qualified
import Result (Result (..))
import Set (Set)
import Set qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
import Log qualified


-- | Parsed JWT header for hot-path optimization.
-- Parse once, use for all validation checks.
data ParsedHeader = ParsedHeader
  { alg :: Text,
    -- ^ Algorithm (required)
    kid :: Maybe Text,
    -- ^ Key ID (optional)
    typ :: Maybe Text,
    -- ^ Type header (optional, RFC 8725 recommends checking)
    crit :: Array Text
    -- ^ Critical headers (empty if not present)
  }
  deriving (Show, Eq)


-- | Parse JWT header once for all validation checks.
-- This is the optimized hot path - avoids re-parsing header 3-4 times.
-- Uses strict base64url decoding per RFC 8725 security recommendations.
parseHeader :: Text -> Result AuthError ParsedHeader
parseHeader token = do
  let parts = GhcText.splitOn "." token
  case parts of
    [headerB64, _, _] -> do
      -- Use strict decoding (not lenient) per RFC 8725 - reject malformed base64
      let headerB64Bytes = headerB64 |> GhcTextEncoding.encodeUtf8
      case GhcBase64.decode headerB64Bytes of
        Prelude.Left _decodeErr -> Err (TokenMalformed "Invalid base64url encoding in header")
        Prelude.Right headerBytes ->
          -- OPTIMIZED: Parse directly to Aeson.Object (KeyMap) - avoids intermediate Map
          -- KeyMap uses HashMap internally for O(1) average-case lookup
          case Aeson.decodeStrict headerBytes of
            Nothing -> Err (TokenMalformed "Invalid header encoding")
            Just (Aeson.Object obj) -> do
              -- Extract alg (required) - O(1) KeyMap lookup
              case GhcKeyMap.lookup (GhcAesonKey.fromString "alg") obj of
                Just (Aeson.String algText) -> do
                  -- Extract kid (optional)
                  let kidVal = case GhcKeyMap.lookup (GhcAesonKey.fromString "kid") obj of
                        Just (Aeson.String k) -> Just k
                        _ -> Nothing
                  -- Extract typ (optional)
                  let typVal = case GhcKeyMap.lookup (GhcAesonKey.fromString "typ") obj of
                        Just (Aeson.String t) -> Just t
                        _ -> Nothing
                  -- Extract crit (optional) - RFC 8725: fail closed on malformed
                  case GhcKeyMap.lookup (GhcAesonKey.fromString "crit") obj of
                    Just (Aeson.Array arr) -> do
                      let critValues = arr |> GhcFoldable.toList |> Array.fromLinkedList
                      -- Validate ALL entries are non-empty strings (fail closed per RFC 8725)
                      case extractCritHeaders critValues of
                        Err errMsg -> Err (TokenMalformed errMsg)
                        Ok critArr ->
                          Ok
                            ParsedHeader
                              { alg = algText,
                                kid = kidVal,
                                typ = typVal,
                                crit = critArr
                              }
                    Just _ -> Err (TokenMalformed "crit header must be an array")
                    Nothing ->
                      Ok
                        ParsedHeader
                          { alg = algText,
                            kid = kidVal,
                            typ = typVal,
                            crit = Array.empty
                          }
                _ -> Err (TokenMalformed "Missing or invalid alg in header")
            Just _ -> Err (TokenMalformed "JWT header must be a JSON object")
    _ -> Err (TokenMalformed "Invalid JWT structure")


-- | Validate a JWT token against the auth configuration.
-- Returns UserClaims on success, AuthError on failure.
--
-- Validation order (per RFC 8725 and performance optimization):
-- 1. Parse JWT header ONCE (optimized - single parse for alg/kid/typ/crit)
-- 2. Reject alg not in allowlist (fail fast, no crypto)
-- 3. Reject unknown crit headers
-- 4. Validate typ if present (RFC 8725)
-- 5. Decode and verify with jose library (CPU-heavy)
-- 6. Issuer validation (exact match)
-- 7. Audience validation
-- 8. Time claims (exp/nbf) with clock skew
-- 9. Extract UserClaims
validateToken ::
  forall err.
  AuthConfig ->
  Array Jose.JWK ->
  Text ->
  Task err (Result AuthError UserClaims)
validateToken config keys token = do
  -- Step 1: Check for empty token
  case Text.isEmpty token of
    True -> do
      Log.debug "Token validation failed: empty token" |> Task.ignoreError
      Task.yield (Err (TokenMalformed "Empty token"))
    False -> do
      -- Step 2: Parse header ONCE (optimized hot path)
      case parseHeader token of
        Err err -> do
          Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
          Task.yield (Err err)
        Ok header -> do
          -- Step 3: Check algorithm allowlist (RFC 8725 fail-fast)
          case isAlgorithmAllowed config.allowedAlgorithmsSet header.alg of
            False -> do
              let algErr = AlgorithmNotAllowed header.alg
              Log.debug [fmt|Token validation failed: #{toText algErr}|] |> Task.ignoreError
              Task.yield (Err algErr)
            True -> do
              -- Step 4: Check crit headers
              case checkCritHeaders header.crit config.supportedCritHeaders of
                Err err -> do
                  Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
                  Task.yield (Err err)
                Ok () -> do
                  -- Step 5: Validate typ if present (RFC 8725)
                  case validateTypHeader header.typ of
                    Err err -> do
                      Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
                      Task.yield (Err err)
                    Ok () -> do
                      -- Step 6: Decode and verify with jose library
                      let tokenBytes = token |> GhcTextEncoding.encodeUtf8 |> GhcLBS.fromStrict
                      decodeResult <- Task.fromIO (Jose.runJOSE @Jose.Error (Jose.decodeCompact tokenBytes))
                      case decodeResult of
                        Prelude.Left _err -> do
                          Log.debug "Token validation failed: invalid JWT format" |> Task.ignoreError
                          Task.yield (Err (TokenMalformed "Invalid JWT format"))
                        Prelude.Right jwt ->
                          verifyAndExtract config keys jwt


-- | Validate a JWT token using a pre-built JWKSet.
-- This is the optimized hot path - avoids JWKSet construction per request.
validateTokenWithJwkSet ::
  forall err.
  AuthConfig ->
  Jose.JWKSet ->
  Text ->
  Task err (Result AuthError UserClaims)
validateTokenWithJwkSet config jwkSet token = do
  -- Step 1: Check for empty token
  case Text.isEmpty token of
    True -> do
      Log.debug "Token validation failed: empty token" |> Task.ignoreError
      Task.yield (Err (TokenMalformed "Empty token"))
    False -> do
      -- Step 2: Parse header ONCE (optimized hot path)
      case parseHeader token of
        Err err -> do
          Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
          Task.yield (Err err)
        Ok header -> do
          -- Step 3: Check algorithm allowlist (RFC 8725 fail-fast)
          case isAlgorithmAllowed config.allowedAlgorithmsSet header.alg of
            False -> do
              let algErr = AlgorithmNotAllowed header.alg
              Log.debug [fmt|Token validation failed: #{toText algErr}|] |> Task.ignoreError
              Task.yield (Err algErr)
            True -> do
              -- Step 4: Check crit headers
              case checkCritHeaders header.crit config.supportedCritHeaders of
                Err err -> do
                  Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
                  Task.yield (Err err)
                Ok () -> do
                  -- Step 5: Validate typ if present (RFC 8725)
                  case validateTypHeader header.typ of
                    Err err -> do
                      Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
                      Task.yield (Err err)
                    Ok () -> do
                      -- Step 6: Decode and verify with jose library
                      let tokenBytes = token |> GhcTextEncoding.encodeUtf8 |> GhcLBS.fromStrict
                      decodeResult <- Task.fromIO (Jose.runJOSE @Jose.Error (Jose.decodeCompact tokenBytes))
                      case decodeResult of
                        Prelude.Left _err -> do
                          Log.debug "Token validation failed: invalid JWT format" |> Task.ignoreError
                          Task.yield (Err (TokenMalformed "Invalid JWT format"))
                        Prelude.Right jwt ->
                          verifyAndExtractWithJwkSet config jwkSet jwt


-- | Validate a JWT token with a pre-parsed header.
-- This is the most optimized hot path for the middleware:
-- 1. Parse header once in middleware (for kid extraction)
-- 2. Pass ParsedHeader here to avoid re-parsing
validateTokenWithParsedHeader ::
  forall err.
  AuthConfig ->
  Jose.JWKSet ->
  ParsedHeader ->
  Text ->
  Task err (Result AuthError UserClaims)
validateTokenWithParsedHeader config jwkSet header token = do
  -- Step 1: Check algorithm allowlist (RFC 8725 fail-fast)
  case isAlgorithmAllowed config.allowedAlgorithmsSet header.alg of
    False -> do
      let algErr = AlgorithmNotAllowed header.alg
      Log.debug [fmt|Token validation failed: #{toText algErr}|] |> Task.ignoreError
      Task.yield (Err algErr)
    True -> do
      -- Step 2: Check crit headers
      case checkCritHeaders header.crit config.supportedCritHeaders of
        Err err -> do
          Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
          Task.yield (Err err)
        Ok () -> do
          -- Step 3: Validate typ if present (RFC 8725)
          case validateTypHeader header.typ of
            Err err -> do
              Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
              Task.yield (Err err)
            Ok () -> do
              -- Step 4: Decode and verify with jose library
              let tokenBytes = token |> GhcTextEncoding.encodeUtf8 |> GhcLBS.fromStrict
              decodeResult <- Task.fromIO (Jose.runJOSE @Jose.Error (Jose.decodeCompact tokenBytes))
              case decodeResult of
                Prelude.Left _err -> do
                  Log.debug "Token validation failed: invalid JWT format" |> Task.ignoreError
                  Task.yield (Err (TokenMalformed "Invalid JWT format"))
                Prelude.Right jwt ->
                  verifyAndExtractWithJwkSet config jwkSet jwt


-- | Check crit headers against supported list (using pre-parsed header).
-- OPTIMIZED: Convert supported list to Set for O(1) membership check per crit header.
-- The conversion is O(n) but crit headers are typically 0-3 items, so negligible.
checkCritHeaders :: Array Text -> Array Text -> Result AuthError ()
checkCritHeaders critHeaders supported = do
  -- Convert to Set once, then O(1) lookups for each crit header
  let supportedSet = Set.fromArray supported
  let unsupported =
        critHeaders
          |> Array.dropIf (\c -> supportedSet |> Set.contains c)
  case Array.isEmpty unsupported of
    True -> Ok ()
    False -> Err (UnsupportedCritHeader unsupported)


-- | Validate typ header if present (RFC 8725 hardening).
-- If typ is present, it must be "JWT" or "at+jwt" (access token).
-- Comparison is case-insensitive per RFC 7515.
validateTypHeader :: Maybe Text -> Result AuthError ()
validateTypHeader maybeTyp =
  case maybeTyp of
    Nothing -> Ok () -- typ is optional
    Just typ -> do
      -- Accept "JWT" or "at+jwt" (RFC 9068 access token)
      -- Case-insensitive comparison per RFC 7515 Section 4.1.9
      let typLower = typ |> Text.toLower
      case typLower == "jwt" || typLower == "at+jwt" of
        True -> Ok ()
        False -> Err (TokenMalformed [fmt|Invalid typ header: #{typ}|])


-- | Extract kid (key ID) from JWT token header.
-- Returns Nothing if kid is not present in the header.
-- Note: For hot path, prefer parseHeader once and access .kid
extractKidFromToken :: Text -> Maybe Text
extractKidFromToken token =
  case parseHeader token of
    Err _ -> Nothing
    Ok header -> header.kid


-- | Extract algorithm from JWT token without full parsing.
-- Note: For hot path, prefer parseHeader once and access .alg
extractAlgorithmFromToken :: Text -> Result AuthError Text
extractAlgorithmFromToken token =
  case parseHeader token of
    Err err -> Err err
    Ok header -> Ok header.alg


-- | Extract text from JSON value
extractTextValue :: Aeson.Value -> Text
extractTextValue val =
  case val of
    Aeson.String txt -> txt
    _ -> ""


-- | Extract crit headers with strict validation (RFC 8725 fail-closed).
-- All entries must be non-empty strings; any malformed entry fails the entire parse.
extractCritHeaders :: Array Aeson.Value -> Result Text (Array Text)
extractCritHeaders values = do
  let results = values |> Array.map validateCritEntry
  -- If any entry is invalid, fail closed
  let hasInvalid = results |> Array.any isNothing
  case hasInvalid of
    True -> Err "crit header contains non-string or empty values"
    False -> Ok (results |> Array.map (Maybe.withDefault ""))
 where
  validateCritEntry :: Aeson.Value -> Maybe Text
  validateCritEntry val =
    case val of
      Aeson.String txt ->
        case Text.isEmpty txt of
          True -> Nothing -- Empty string is invalid
          False -> Just txt
      _ -> Nothing -- Non-string is invalid

  isNothing :: forall value. Maybe value -> Bool
  isNothing maybeVal =
    case maybeVal of
      Nothing -> True
      Just _ -> False


-- | Check if algorithm is in the allowlist.
-- Uses O(1) Set lookup from precomputed allowedAlgorithmsSet for efficiency.
isAlgorithmAllowed :: Set Text -> Text -> Bool
isAlgorithmAllowed allowedSet alg = allowedSet |> Set.contains alg
{-# INLINE isAlgorithmAllowed #-}


-- | Verify the JWT signature and extract claims
verifyAndExtract ::
  forall err.
  AuthConfig ->
  Array Jose.JWK ->
  JWT.SignedJWT ->
  Task err (Result AuthError UserClaims)
verifyAndExtract config keys jwt = do
  result <- Task.fromIO do
    -- Create JWK set from keys
    let jwkSet = Jose.JWKSet (Array.toLinkedList keys)

    -- Build validation settings
    -- Use proper StringOrURI comparison (avoids show/unpack mismatches)
    let audCheck = case config.audience of
          Nothing -> Prelude.const True
          Just expectedAud -> \aud -> stringOrUriMatchesText aud expectedAud

    let validationSettings =
          JWT.defaultJWTValidationSettings audCheck
            Lens.& JWT.issuerPredicate
              Lens..~ (\iss -> stringOrUriMatchesText iss config.issuer)
            Lens.& JWT.allowedSkew
              Lens..~ fromIntegral config.clockSkewSeconds

    -- Verify and validate
    JWT.runJOSE @JWT.JWTError do
      JWT.verifyJWT validationSettings jwkSet jwt

  case result of
    Prelude.Left err -> do
      let authErr = mapJoseError config err
      Log.debug [fmt|Token validation failed: #{toText authErr}|] |> Task.ignoreError
      Task.yield (Err authErr)
    Prelude.Right claims -> do
      case extractUserClaims config claims of
        Err err -> do
          Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
          Task.yield (Err err)
        Ok userClaims -> do
          Log.debug "Token validated successfully" |> Task.ignoreError
          Task.yield (Ok userClaims)


-- | Verify the JWT signature and extract claims using a pre-built JWKSet.
-- This is the optimized version that avoids JWKSet construction.
verifyAndExtractWithJwkSet ::
  forall err.
  AuthConfig ->
  Jose.JWKSet ->
  JWT.SignedJWT ->
  Task err (Result AuthError UserClaims)
verifyAndExtractWithJwkSet config jwkSet jwt = do
  result <- Task.fromIO do
    -- Build validation settings
    -- Use proper StringOrURI comparison (avoids show/unpack mismatches)
    let audCheck = case config.audience of
          Nothing -> Prelude.const True
          Just expectedAud -> \aud -> stringOrUriMatchesText aud expectedAud

    let validationSettings =
          JWT.defaultJWTValidationSettings audCheck
            Lens.& JWT.issuerPredicate
              Lens..~ (\iss -> stringOrUriMatchesText iss config.issuer)
            Lens.& JWT.allowedSkew
              Lens..~ fromIntegral config.clockSkewSeconds

    -- Verify and validate
    JWT.runJOSE @JWT.JWTError do
      JWT.verifyJWT validationSettings jwkSet jwt

  case result of
    Prelude.Left err -> do
      let authErr = mapJoseError config err
      Log.debug [fmt|Token validation failed: #{toText authErr}|] |> Task.ignoreError
      Task.yield (Err authErr)
    Prelude.Right claims -> do
      case extractUserClaims config claims of
        Err err -> do
          Log.debug [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
          Task.yield (Err err)
        Ok userClaims -> do
          Log.debug "Token validated successfully" |> Task.ignoreError
          Task.yield (Ok userClaims)


-- | Map jose library errors to our AuthError type.
-- Includes expected values from config for better debugging.
-- Note: jose library doesn't expose actual token values on mismatch.
mapJoseError :: AuthConfig -> JWT.JWTError -> AuthError
mapJoseError config err =
  case err of
    JWT.JWSError jwsErr ->
      case jwsErr of
        Jose.AlgorithmNotImplemented -> SignatureInvalid
        Jose.AlgorithmMismatch _ -> KeyAlgorithmMismatch
        Jose.KeyMismatch _ -> SignatureInvalid
        Jose.KeySizeTooSmall -> SignatureInvalid
        _ -> SignatureInvalid
    JWT.JWTExpired -> TokenExpired
    JWT.JWTNotYetValid -> TokenNotYetValid
    JWT.JWTNotInIssuer ->
      -- Include expected issuer for debugging (actual not available from jose)
      IssuerMismatch "(token issuer hidden)" config.issuer
    JWT.JWTNotInAudience ->
      -- Include expected audience for debugging
      let expectedAud = config.audience |> Maybe.withDefault "(not configured)"
       in AudienceMismatch expectedAud Array.empty
    JWT.JWTClaimsSetDecodeError _ -> TokenMalformed "Invalid claims"
    JWT.JWTIssuedAtFuture -> TokenMalformed "Token issued in the future"


-- | Extract UserClaims from validated JWT claims
extractUserClaims :: AuthConfig -> JWT.ClaimsSet -> Result AuthError UserClaims
extractUserClaims config claims = do
  -- Extract subject (required)
  let subClaim = claims Lens.^. JWT.claimSub
  case subClaim of
    Nothing -> Err (TokenMalformed "Missing sub claim")
    Just sub -> do
      let subText = stringOrUriToText sub
      let unregClaims = claims Lens.^. JWT.unregisteredClaims

      -- Extract optional claims
      let email = lookupTextClaim "email" unregClaims
      let name = lookupTextClaim "name" unregClaims
      let permissions = lookupArrayClaim config.permissionsClaim unregClaims
      let tenantId = config.tenantIdClaim |> Maybe.andThen (\claim -> lookupTextClaim claim unregClaims)

      -- Build raw claims map
      let rawClaims = convertUnregisteredClaims unregClaims

      Ok
        UserClaims
          { sub = subText,
            email = email,
            name = name,
            permissions = permissions,
            tenantId = tenantId,
            rawClaims = rawClaims
          }


-- | Convert StringOrURI to Text
-- For URIs, uses uriToString to get proper canonical form (not Show's debug output)
stringOrUriToText :: JWT.StringOrURI -> Text
stringOrUriToText sou =
  -- Use the string prism to extract Text if it's a plain string
  case sou Lens.^? JWT.string of
    Just txt -> txt
    Nothing ->
      -- It's a URI - use uriToString for proper serialization
      case sou Lens.^? JWT.uri of
        Just souUri -> GhcURI.uriToString Prelude.id souUri "" |> Text.fromLinkedList
        Nothing -> Prelude.show sou |> Text.fromLinkedList -- Fallback (shouldn't happen)


-- | Check if a StringOrURI matches expected text.
-- Uses proper comparison: string form uses direct equality,
-- URI form converts expected text to URI for proper semantic comparison.
-- This avoids issues with show/unpack mismatches on URIs.
stringOrUriMatchesText :: JWT.StringOrURI -> Text -> Bool
stringOrUriMatchesText sou expected = do
  -- First, try direct string match (most common case)
  case sou Lens.^? JWT.string of
    Just souText -> souText == expected
    Nothing ->
      -- It's a URI - compare properly via stringOrUriToText
      -- This is safer than show because it uses the same conversion both ways
      stringOrUriToText sou == expected


-- | Look up a text claim from unregistered claims
lookupTextClaim :: Text -> GhcMap.Map GhcText.Text Aeson.Value -> Maybe Text
lookupTextClaim key claims = do
  case GhcMap.lookup key claims of
    Just (Aeson.String txt) -> Just txt
    _ -> Nothing


-- | Look up an array claim from unregistered claims
lookupArrayClaim :: Text -> GhcMap.Map GhcText.Text Aeson.Value -> Array Text
lookupArrayClaim key claims = do
  case GhcMap.lookup key claims of
    Just (Aeson.Array arr) ->
      arr
        |> GhcFoldable.toList
        |> Array.fromLinkedList
        |> Array.map extractTextValue
        |> Array.dropIf Text.isEmpty
    _ -> Array.empty


-- | Convert unregistered claims to our Map type
convertUnregisteredClaims :: GhcMap.Map GhcText.Text Aeson.Value -> Map Text Aeson.Value
convertUnregisteredClaims claims =
  claims
    |> GhcMap.toList
    |> Array.fromLinkedList
    |> Map.fromArray


-- | Parse just the token header (for testing/debugging)
parseTokenHeader :: Text -> Result AuthError Text
parseTokenHeader = extractAlgorithmFromToken
