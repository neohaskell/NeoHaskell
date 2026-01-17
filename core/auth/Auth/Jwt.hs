{-# OPTIONS_GHC -Wno-deprecations #-}

-- | JWT token validation.
-- This module handles parsing and validating JWT tokens against
-- the auth configuration. It implements RFC 8725 JOSE hardening.
module Auth.Jwt (
  -- * Token validation
  validateToken,
  validateTokenWithJwkSet,
  -- * Token parsing
  extractKidFromToken,
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


-- | Validate a JWT token against the auth configuration.
-- Returns UserClaims on success, AuthError on failure.
--
-- Validation order (per RFC 8725 and performance optimization):
-- 1. Parse JWT format
-- 2. Reject alg not in allowlist (fail fast, no crypto)
-- 3. Reject unknown crit headers
-- 4. Extract kid (if missing, may still work with single key)
-- 5. Key lookup and signature verification (CPU-heavy)
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
      -- Step 2: Pre-parse to check algorithm (RFC 8725 fail-fast)
      let algResult = extractAlgorithmFromToken token
      case algResult of
        Err err -> Task.yield (Err err)
        Ok alg -> do
          case isAlgorithmAllowed config.allowedAlgorithms alg of
            False -> Task.yield (Err (AlgorithmNotAllowed alg))
            True -> do
              -- Step 3: Check crit headers from raw token
              let critResult = checkCritFromToken token config.supportedCritHeaders
              case critResult of
                Err err -> Task.yield (Err err)
                Ok () -> do
                  -- Step 4: Decode and verify with jose library
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
      -- Step 2: Pre-parse to check algorithm (RFC 8725 fail-fast)
      let algResult = extractAlgorithmFromToken token
      case algResult of
        Err err -> Task.yield (Err err)
        Ok alg -> do
          case isAlgorithmAllowed config.allowedAlgorithms alg of
            False -> Task.yield (Err (AlgorithmNotAllowed alg))
            True -> do
              -- Step 3: Check crit headers from raw token
              let critResult = checkCritFromToken token config.supportedCritHeaders
              case critResult of
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


-- | Extract kid (key ID) from JWT token header.
-- Returns Nothing if kid is not present in the header.
extractKidFromToken :: Text -> Maybe Text
extractKidFromToken token = do
  let parts = GhcText.splitOn "." token
  case parts of
    [headerB64, _, _] -> do
      let headerBytes = headerB64 |> GhcTextEncoding.encodeUtf8 |> GhcBase64.decodeLenient
      case Aeson.decodeStrict headerBytes of
        Nothing -> Nothing
        Just (obj :: GhcMap.Map GhcText.Text Aeson.Value) ->
          case GhcMap.lookup ("kid" :: GhcText.Text) obj of
            Just (Aeson.String kid) -> Just kid
            _ -> Nothing
    _ -> Nothing


-- | Extract algorithm from JWT token without full parsing
-- This allows fail-fast rejection of disallowed algorithms
extractAlgorithmFromToken :: Text -> Result AuthError Text
extractAlgorithmFromToken token = do
  -- JWT format: header.payload.signature
  let parts = GhcText.splitOn "." token
  case parts of
    [headerB64, _, _] -> do
      -- Decode header (base64url)
      let headerBytes = headerB64 |> GhcTextEncoding.encodeUtf8 |> GhcBase64.decodeLenient
      case Aeson.decodeStrict headerBytes of
        Nothing -> Err (TokenMalformed "Invalid header encoding")
        Just obj -> do
          case GhcMap.lookup ("alg" :: GhcText.Text) obj of
            Just (Aeson.String alg) -> Ok alg
            _ -> Err (TokenMalformed "Missing or invalid alg in header")
    _ -> Err (TokenMalformed "Invalid JWT structure")


-- | Check crit headers from raw token
checkCritFromToken :: Text -> Array Text -> Result AuthError ()
checkCritFromToken token supported = do
  let parts = GhcText.splitOn "." token
  case parts of
    [headerB64, _, _] -> do
      let headerBytes = headerB64 |> GhcTextEncoding.encodeUtf8 |> GhcBase64.decodeLenient
      case Aeson.decodeStrict headerBytes of
        Nothing -> Ok () -- Already handled in alg check
        Just (obj :: GhcMap.Map GhcText.Text Aeson.Value) -> do
          case GhcMap.lookup ("crit" :: GhcText.Text) obj of
            Nothing -> Ok ()
            Just (Aeson.Array critArr) -> do
              let critList = critArr |> GhcFoldable.toList |> Array.fromLinkedList
              let unsupported =
                    critList
                      |> Array.map extractTextValue
                      |> Array.dropIf Text.isEmpty
                      |> Array.dropIf (\c -> supported |> Array.any (\s -> s == c))
              case Array.isEmpty unsupported of
                True -> Ok ()
                False -> Err (UnsupportedCritHeader unsupported)
            _ -> Err (TokenMalformed "Invalid crit header format")
    _ -> Ok () -- Already handled


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
  let audCheck = case config.audience of
        Nothing -> Prelude.const True
        Just expectedAud -> \aud -> do
          -- Compare using the string prism
          case aud Lens.^? JWT.string of
            Just audText -> audText == expectedAud
            Nothing -> False

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
  let audCheck = case config.audience of
        Nothing -> Prelude.const True
        Just expectedAud -> \aud -> do
          case aud Lens.^? JWT.string of
            Just audText -> audText == expectedAud
            Nothing -> False

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
