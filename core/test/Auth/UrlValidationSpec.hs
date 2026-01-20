module Auth.UrlValidationSpec where

import Auth.UrlValidation (ValidationError (..))
import Auth.UrlValidation qualified as UrlValidation
import Core
import Test


spec :: Spec Unit
spec = do
  describe "Auth.UrlValidation" do
    -- Existing literal IP validation (non-IO)
    describe "validateSecureUrl (literal IPs)" do
      it "accepts valid HTTPS URL with public domain" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://example.com/path"
        result |> shouldBe (Ok "https://example.com/path")

      it "rejects HTTP URLs" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://example.com/path"
        result |> shouldSatisfy isNotHttps

      it "rejects localhost" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://localhost/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal private IPv4 (10.x.x.x)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://10.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal private IPv4 (172.16.x.x)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://172.16.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal private IPv4 (192.168.x.x)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://192.168.1.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal loopback IPv4 (127.0.0.1)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://127.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal IPv6 loopback ([::1])" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://[::1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks link-local IPv6 ([fe80::])" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://[fe80::1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks unique-local IPv6 ([fc00::])" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://[fc00::1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "rejects malformed URLs" \_ -> do
        let result = UrlValidation.validateSecureUrl "not-a-url"
        result |> shouldSatisfy isMalformedUrl

      it "rejects URLs without hostname" \_ -> do
        let result = UrlValidation.validateSecureUrl "https:///path"
        result |> shouldSatisfy isMissingHostname

    -- DNS resolution tests (IO-based)
    describe "validateSecureUrlWithDns (DNS resolution)" do
      it "accepts URLs resolving to public IPs" \_ -> do
        -- google.com should resolve to public IPs
        result <- UrlValidation.validateSecureUrlWithDns "https://google.com/.well-known/openid-configuration"
        result |> shouldSatisfy isOkResult

      it "blocks localhost via literal check (before DNS)" \_ -> do
        -- localhost is caught by literal "localhost" check, not DNS resolution
        result <- UrlValidation.validateSecureUrlWithDns "https://localhost/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks DNS names resolving to private IPs when configured" \_ -> do
        -- NOTE: This test may be environment-dependent.
        -- In most environments, localhost.localdomain or similar resolves to 127.0.0.1
        result <- UrlValidation.validateSecureUrlWithDns "https://localhost.localdomain/path"
        result |> shouldSatisfy isDnsResolutionBlockedOrFailed

      it "handles DNS resolution failures gracefully" \_ -> do
        -- Non-existent domain should fail DNS resolution
        result <- UrlValidation.validateSecureUrlWithDns "https://this-domain-does-not-exist-12345.invalid/path"
        result |> shouldSatisfy isDnsResolutionFailed

      it "still enforces HTTPS requirement before DNS resolution" \_ -> do
        -- HTTP should be rejected before any DNS lookup
        result <- UrlValidation.validateSecureUrlWithDns "http://example.com/path"
        result |> shouldSatisfy isNotHttps

      it "blocks literal loopback IPs before DNS resolution" \_ -> do
        -- Literal IPs are caught before DNS resolution
        result <- UrlValidation.validateSecureUrlWithDns "https://127.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      -- IPv4-mapped IPv6 addresses (potential bypass)
      it "blocks IPv4-mapped IPv6 addresses to private IPs" \_ -> do
        -- ::ffff:127.0.0.1 is IPv4-mapped loopback
        let result = UrlValidation.validateSecureUrl "https://[::ffff:127.0.0.1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks IPv4-mapped IPv6 addresses to private ranges" \_ -> do
        -- ::ffff:10.0.0.1 is IPv4-mapped private IP
        let result = UrlValidation.validateSecureUrl "https://[::ffff:10.0.0.1]/path"
        result |> shouldSatisfy isPrivateIpBlocked


-- | Helper predicates for test assertions
isNotHttps :: Result ValidationError Text -> Bool
isNotHttps result =
  case result of
    Err (NotHttps _) -> True
    _ -> False


isPrivateIpBlocked :: Result ValidationError Text -> Bool
isPrivateIpBlocked result =
  case result of
    Err (PrivateIpBlocked _) -> True
    _ -> False


isMalformedUrl :: Result ValidationError Text -> Bool
isMalformedUrl result =
  case result of
    Err (MalformedUrl _) -> True
    _ -> False


isMissingHostname :: Result ValidationError Text -> Bool
isMissingHostname result =
  case result of
    Err (MissingHostname _) -> True
    _ -> False


isDnsResolutionBlocked :: Result ValidationError Text -> Bool
isDnsResolutionBlocked result =
  case result of
    Err (DnsResolutionBlocked _ _) -> True
    _ -> False


isDnsResolutionFailed :: Result ValidationError Text -> Bool
isDnsResolutionFailed result =
  case result of
    Err (DnsResolutionFailed _ _) -> True
    _ -> False


-- | Either blocked or failed - for environment-dependent tests
isDnsResolutionBlockedOrFailed :: Result ValidationError Text -> Bool
isDnsResolutionBlockedOrFailed result =
  isDnsResolutionBlocked result || isDnsResolutionFailed result


isOkResult :: Result ValidationError Text -> Bool
isOkResult result =
  case result of
    Ok _ -> True
    _ -> False
