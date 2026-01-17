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
import Data.Map.Strict qualified as GhcMap
import Data.Text qualified as GhcText
import Data.Text.Encoding qualified as GhcTextEncoding
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Prelude qualified
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


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
parseHeader :: Text -> Result AuthError ParsedHeader
parseHeader token = do
  let parts = GhcText.splitOn "." token
  case parts of
    [headerB64, _, _] -> do
      let headerBytes = headerB64 |> GhcTextEncoding.encodeUtf8 |> GhcBase64.decodeLenient
      case Aeson.decodeStrict headerBytes of
        Nothing -> Err (TokenMalformed "Invalid header encoding")
        Just (obj :: GhcMap.Map GhcText.Text Aeson.Value) -> do
          -- Extract alg (required)
          case GhcMap.lookup ("alg" :: GhcText.Text) obj of
            Just (Aeson.String algText) -> do
              -- Extract kid (optional)
              let kidVal = case GhcMap.lookup ("kid" :: GhcText.Text) obj of
                    Just (Aeson.String k) -> Just k
                    _ -> Nothing
              -- Extract typ (optional)
              let typVal = case GhcMap.lookup ("typ" :: GhcText.Text) obj of
                    Just (Aeson.String t) -> Just t
                    _ -> Nothing
              -- Extract crit (optional)
              let critArr = case GhcMap.lookup ("crit" :: GhcText.Text) obj of
                    Just (Aeson.Array arr) ->
                      arr
                        |> GhcFoldable.toList
                        |> Array.fromLinkedList
                        |> Array.map extractTextValue
                        |> Array.dropIf Text.isEmpty
                    _ -> Array.empty
              Ok
                ParsedHeader
                  { alg = algText,
                    kid = kidVal,
                    typ = typVal,
                    crit = critArr
                  }
            _ -> Err (TokenMalformed "Missing or invalid alg in header")
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
    True -> Task.yield (Err (TokenMalformed "Empty token"))
    False -> do
      -- Step 2: Parse header ONCE (optimized hot path)
      case parseHeader token of
        Err err -> Task.yield (Err err)
        Ok header -> do
          -- Step 3: Check algorithm allowlist (RFC 8725 fail-fast)
          case isAlgorithmAllowed config.allowedAlgorithms header.alg of
            False -> Task.yield (Err (AlgorithmNotAllowed header.alg))
            True -> do
              -- Step 4: Check crit headers
              case checkCritHeaders header.crit config.supportedCritHeaders of
                Err err -> Task.yield (Err err)
                Ok () -> do
                  -- Step 5: Validate typ if present (RFC 8725)
                  case validateTypHeader header.typ of
                    Err err -> Task.yield (Err err)
                    Ok () -> do
                      -- Step 6: Decode and verify with jose library
                      let tokenBytes = token |> GhcTextEncoding.encodeUtf8 |> GhcLBS.fromStrict
                      decodeResult <- Task.fromIO (Jose.runJOSE @Jose.Error (Jose.decodeCompact tokenBytes))
                      case decodeResult of
                        Prelude.Left _err ->
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
    True -> Task.yield (Err (TokenMalformed "Empty token"))
    False -> do
      -- Step 2: Parse header ONCE (optimized hot path)
      case parseHeader token of
        Err err -> Task.yield (Err err)
        Ok header -> do
          -- Step 3: Check algorithm allowlist (RFC 8725 fail-fast)
          case isAlgorithmAllowed config.allowedAlgorithms header.alg of
            False -> Task.yield (Err (AlgorithmNotAllowed header.alg))
            True -> do
              -- Step 4: Check crit headers
              case checkCritHeaders header.crit config.supportedCritHeaders of
                Err err -> Task.yield (Err err)
                Ok () -> do
                  -- Step 5: Validate typ if present (RFC 8725)
                  case validateTypHeader header.typ of
                    Err err -> Task.yield (Err err)
                    Ok () -> do
                      -- Step 6: Decode and verify with jose library
                      let tokenBytes = token |> GhcTextEncoding.encodeUtf8 |> GhcLBS.fromStrict
                      decodeResult <- Task.fromIO (Jose.runJOSE @Jose.Error (Jose.decodeCompact tokenBytes))
                      case decodeResult of
                        Prelude.Left _err ->
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
  case isAlgorithmAllowed config.allowedAlgorithms header.alg of
    False -> Task.yield (Err (AlgorithmNotAllowed header.alg))
    True -> do
      -- Step 2: Check crit headers
      case checkCritHeaders header.crit config.supportedCritHeaders of
        Err err -> Task.yield (Err err)
        Ok () -> do
          -- Step 3: Validate typ if present (RFC 8725)
          case validateTypHeader header.typ of
            Err err -> Task.yield (Err err)
            Ok () -> do
              -- Step 4: Decode and verify with jose library
              let tokenBytes = token |> GhcTextEncoding.encodeUtf8 |> GhcLBS.fromStrict
              decodeResult <- Task.fromIO (Jose.runJOSE @Jose.Error (Jose.decodeCompact tokenBytes))
              case decodeResult of
                Prelude.Left _err ->
                  Task.yield (Err (TokenMalformed "Invalid JWT format"))
                Prelude.Right jwt ->
                  verifyAndExtractWithJwkSet config jwkSet jwt


-- | Check crit headers against supported list (using pre-parsed header).
checkCritHeaders :: Array Text -> Array Text -> Result AuthError ()
checkCritHeaders critHeaders supported = do
  let unsupported =
        critHeaders
          |> Array.dropIf (\c -> supported |> Array.any (\s -> s == c))
  case Array.isEmpty unsupported of
    True -> Ok ()
    False -> Err (UnsupportedCritHeader unsupported)


-- | Validate typ header if present (RFC 8725 hardening).
-- If typ is present, it must be "JWT" or "at+jwt" (access token).
validateTypHeader :: Maybe Text -> Result AuthError ()
validateTypHeader maybeTyp =
  case maybeTyp of
    Nothing -> Ok () -- typ is optional
    Just typ ->
      -- Accept "JWT" or "at+jwt" (RFC 9068 access token)
      case typ == "JWT" || typ == "at+jwt" of
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


-- | Check if algorithm is in the allowlist
isAlgorithmAllowed :: Array Text -> Text -> Bool
isAlgorithmAllowed allowlist alg =
  allowlist |> Array.any (\allowed -> allowed == alg)


-- | Verify the JWT signature and extract claims
verifyAndExtract ::
  forall err.
  AuthConfig ->
  Array Jose.JWK ->
  JWT.SignedJWT ->
  Task err (Result AuthError UserClaims)
verifyAndExtract config keys jwt = Task.fromIO do
  -- Create JWK set from keys
  let jwkSet = Jose.JWKSet (Array.toLinkedList keys)

  -- Build validation settings
  -- FIXED: Check both string and URI form for audience (RFC 7519)
  let audCheck = case config.audience of
        Nothing -> Prelude.const True
        Just expectedAud -> \aud -> do
          -- StringOrURI can be either a plain string or a URI
          let stringMatch = case aud Lens.^? JWT.string of
                Just audText -> audText == expectedAud
                Nothing -> False
          let uriMatch = case aud Lens.^? JWT.uri of
                Just audUri -> show audUri == GhcText.unpack expectedAud
                Nothing -> False
          stringMatch || uriMatch

  let validationSettings =
        JWT.defaultJWTValidationSettings audCheck
          Lens.& JWT.issuerPredicate
            Lens..~ (\iss -> do
              -- StringOrURI can be either a plain string or a URI
              -- We need to check both cases
              let stringMatch = case iss Lens.^? JWT.string of
                    Just issText -> issText == config.issuer
                    Nothing -> False
              let uriMatch = case iss Lens.^? JWT.uri of
                    Just issUri -> show issUri == GhcText.unpack config.issuer
                    Nothing -> False
              stringMatch || uriMatch)
          Lens.& JWT.allowedSkew
            Lens..~ fromIntegral config.clockSkewSeconds

  -- Verify and validate
  result <- JWT.runJOSE @JWT.JWTError do
    JWT.verifyJWT validationSettings jwkSet jwt

  case result of
    Prelude.Left err -> pure (Err (mapJoseError err))
    Prelude.Right claims -> pure (extractUserClaims config claims)


-- | Verify the JWT signature and extract claims using a pre-built JWKSet.
-- This is the optimized version that avoids JWKSet construction.
verifyAndExtractWithJwkSet ::
  forall err.
  AuthConfig ->
  Jose.JWKSet ->
  JWT.SignedJWT ->
  Task err (Result AuthError UserClaims)
verifyAndExtractWithJwkSet config jwkSet jwt = Task.fromIO do
  -- Build validation settings
  -- FIXED: Check both string and URI form for audience (RFC 7519)
  let audCheck = case config.audience of
        Nothing -> Prelude.const True
        Just expectedAud -> \aud -> do
          -- StringOrURI can be either a plain string or a URI
          let stringMatch = case aud Lens.^? JWT.string of
                Just audText -> audText == expectedAud
                Nothing -> False
          let uriMatch = case aud Lens.^? JWT.uri of
                Just audUri -> show audUri == GhcText.unpack expectedAud
                Nothing -> False
          stringMatch || uriMatch

  let validationSettings =
        JWT.defaultJWTValidationSettings audCheck
          Lens.& JWT.issuerPredicate
            Lens..~ (\iss -> do
              let stringMatch = case iss Lens.^? JWT.string of
                    Just issText -> issText == config.issuer
                    Nothing -> False
              let uriMatch = case iss Lens.^? JWT.uri of
                    Just issUri -> show issUri == GhcText.unpack config.issuer
                    Nothing -> False
              stringMatch || uriMatch)
          Lens.& JWT.allowedSkew
            Lens..~ fromIntegral config.clockSkewSeconds

  -- Verify and validate
  result <- JWT.runJOSE @JWT.JWTError do
    JWT.verifyJWT validationSettings jwkSet jwt

  case result of
    Prelude.Left err -> pure (Err (mapJoseError err))
    Prelude.Right claims -> pure (extractUserClaims config claims)


-- | Map jose library errors to our AuthError type
mapJoseError :: JWT.JWTError -> AuthError
mapJoseError err =
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
    JWT.JWTNotInIssuer -> IssuerMismatch "Token issuer" "Expected issuer"
    JWT.JWTNotInAudience -> AudienceMismatch "Expected audience" Array.empty
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
stringOrUriToText :: JWT.StringOrURI -> Text
stringOrUriToText sou =
  -- Use the string prism to extract Text, fallback to show
  case sou Lens.^? JWT.string of
    Just txt -> txt
    Nothing -> Prelude.show sou |> Text.fromLinkedList


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
