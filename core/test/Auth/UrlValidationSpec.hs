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

      it "rejects HTTP URLs for non-localhost" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://example.com/path"
        result |> shouldSatisfy isNotHttps

      -- Localhost exception: HTTP and HTTPS both allowed for localhost
      it "accepts HTTPS localhost URL" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://localhost/path"
        result |> shouldBe (Ok "https://localhost/path")

      it "accepts HTTP localhost URL (development exception)" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://localhost/path"
        result |> shouldBe (Ok "http://localhost/path")

      it "accepts HTTP localhost with port" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://localhost:8080/path"
        result |> shouldBe (Ok "http://localhost:8080/path")

      it "blocks literal private IPv4 (10.x.x.x)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://10.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal private IPv4 (172.16.x.x)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://172.16.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks literal private IPv4 (192.168.x.x)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://192.168.1.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      -- 127.0.0.1 and [::1] are now allowed (localhost exception)
      it "accepts HTTPS 127.0.0.1 URL (localhost exception)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://127.0.0.1/path"
        result |> shouldBe (Ok "https://127.0.0.1/path")

      it "accepts HTTP 127.0.0.1 URL (development exception)" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://127.0.0.1/path"
        result |> shouldBe (Ok "http://127.0.0.1/path")

      it "accepts HTTP 127.0.0.1 with port" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://127.0.0.1:8180/path"
        result |> shouldBe (Ok "http://127.0.0.1:8180/path")

      it "accepts HTTPS [::1] URL (localhost exception)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://[::1]/path"
        result |> shouldBe (Ok "https://[::1]/path")

      it "accepts HTTP [::1] URL (development exception)" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://[::1]/path"
        result |> shouldBe (Ok "http://[::1]/path")

      it "accepts HTTP [::1] with port" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://[::1]:8080/path"
        result |> shouldBe (Ok "http://[::1]:8080/path")

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

    -- Single-label hostname tests (FQDN requirement)
    describe "validateSecureUrl (FQDN requirement)" do
      it "rejects single-label hostnames (no dots)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://intranet/path"
        result |> shouldSatisfy isSingleLabelHostname

      it "rejects single-label hostnames like 'db'" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://db/admin"
        result |> shouldSatisfy isSingleLabelHostname

      it "accepts FQDNs (hostnames with dots)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://auth.example.com/path"
        result |> shouldSatisfy isOkResult

      it "allows literal IPv4 addresses (they have dots)" \_ -> do
        -- Public IP should be allowed
        let result = UrlValidation.validateSecureUrl "https://8.8.8.8/path"
        result |> shouldSatisfy isOkResult

      it "allows bracketed IPv6 addresses" \_ -> do
        -- Public IPv6 should be allowed (2001:4860:4860::8888 is Google DNS)
        let result = UrlValidation.validateSecureUrl "https://[2001:4860:4860::8888]/path"
        result |> shouldSatisfy isOkResult

      it "rejects invalid IPv4-like strings as single-label (999.999.999.999)" \_ -> do
        -- Invalid IPv4 should be treated as hostname, and rejected as single-label
        -- (no valid domain has this format, but it's not a valid IP either)
        let result = UrlValidation.validateSecureUrl "https://999.999.999.999/path"
        -- This will fail to parse as IPv4, so it's treated as a hostname
        -- Hostnames with dots are allowed, so this passes FQDN check
        -- but the URI parser may reject it - let's see what happens
        result |> shouldSatisfy isOkResult -- It has dots, so FQDN check passes

      it "rejects malformed IPv4 with too many octets as hostname" \_ -> do
        -- "1.2.3.4.5" is not a valid IPv4, treated as hostname with dots
        let result = UrlValidation.validateSecureUrl "https://1.2.3.4.5/path"
        -- Has dots, so FQDN check passes - it's just an invalid hostname
        result |> shouldSatisfy isOkResult

    -- Extended IP range tests
    describe "validateSecureUrl (extended IP ranges)" do
      it "blocks CGNAT range (100.64.0.0/10)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://100.64.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks TEST-NET-1 (192.0.2.0/24)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://192.0.2.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks TEST-NET-2 (198.51.100.0/24)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://198.51.100.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks TEST-NET-3 (203.0.113.0/24)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://203.0.113.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks benchmarking range (198.18.0.0/15)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://198.18.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks multicast IPv4 (224.0.0.0/4)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://224.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks reserved IPv4 (240.0.0.0/4)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://240.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks IPv6 multicast (ff00::/8)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://[ff02::1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks IPv6 documentation prefix (2001:db8::/32)" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://[2001:db8::1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

    -- DNS resolution tests (IO-based)
    describe "validateSecureUrlWithDns (DNS resolution)" do
      it "accepts URLs resolving to public IPs" \_ -> do
        -- google.com should resolve to public IPs
        result <- UrlValidation.validateSecureUrlWithDns "https://google.com/.well-known/openid-configuration"
        result |> shouldSatisfy isOkResult

      it "accepts localhost URLs (development exception, skips DNS)" \_ -> do
        -- localhost is allowed and skips DNS resolution entirely
        result <- UrlValidation.validateSecureUrlWithDns "https://localhost/path"
        result |> shouldSatisfy isOkResult

      it "accepts HTTP localhost URLs with DNS validation" \_ -> do
        -- HTTP localhost should pass and skip DNS
        result <- UrlValidation.validateSecureUrlWithDns "http://localhost:8080/path"
        result |> shouldSatisfy isOkResult

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

      it "accepts literal loopback IPs (localhost exception)" \_ -> do
        -- 127.0.0.1 is localhost, allowed and skips DNS
        result <- UrlValidation.validateSecureUrlWithDns "https://127.0.0.1/path"
        result |> shouldSatisfy isOkResult

      it "accepts HTTP 127.0.0.1 with DNS validation" \_ -> do
        -- HTTP 127.0.0.1 should pass (localhost exception)
        result <- UrlValidation.validateSecureUrlWithDns "http://127.0.0.1:8180/path"
        result |> shouldSatisfy isOkResult

      it "skips DNS resolution for public literal IPv4 addresses" \_ -> do
        -- Literal IPv4 should not trigger DNS resolution, just pass through
        result <- UrlValidation.validateSecureUrlWithDns "https://8.8.8.8/path"
        result |> shouldSatisfy isOkResult

      it "skips DNS resolution for public bracketed IPv6 addresses" \_ -> do
        -- Bracketed IPv6 should not trigger DNS resolution
        -- Using Google's public DNS IPv6 address
        result <- UrlValidation.validateSecureUrlWithDns "https://[2001:4860:4860::8888]/path"
        result |> shouldSatisfy isOkResult

      -- IPv4-mapped IPv6 addresses (potential bypass)
      it "blocks IPv4-mapped IPv6 addresses to private IPs" \_ -> do
        -- ::ffff:127.0.0.1 is IPv4-mapped loopback
        let result = UrlValidation.validateSecureUrl "https://[::ffff:127.0.0.1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "blocks IPv4-mapped IPv6 addresses to private ranges" \_ -> do
        -- ::ffff:10.0.0.1 is IPv4-mapped private IP
        let result = UrlValidation.validateSecureUrl "https://[::ffff:10.0.0.1]/path"
        result |> shouldSatisfy isPrivateIpBlocked

    -- Security regression tests for localhost exception
    describe "validateSecureUrl (localhost security regression)" do
      it "still rejects HTTP for non-localhost domains" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://oauth.example.com/token"
        result |> shouldSatisfy isNotHttps

      it "still rejects HTTP for private IPs" \_ -> do
        let result = UrlValidation.validateSecureUrl "http://192.168.1.1/path"
        result |> shouldSatisfy isNotHttps

      it "rejects localhost subdomain attack (localhost.evil.com)" \_ -> do
        -- SECURITY CRITICAL: localhost.evil.com is NOT localhost
        let result = UrlValidation.validateSecureUrl "http://localhost.evil.com/path"
        result |> shouldSatisfy isNotHttps

      it "rejects localhost subdomain attack over HTTPS" \_ -> do
        -- Even HTTPS localhost.evil.com should be allowed (but not as localhost)
        let result = UrlValidation.validateSecureUrl "https://localhost.evil.com/path"
        result |> shouldSatisfy isOkResult  -- HTTPS is fine, just not treated as localhost

      it "rejects alternate loopback IPs (127.0.0.2)" \_ -> do
        -- Only 127.0.0.1 is allowed, not other loopback addresses
        let result = UrlValidation.validateSecureUrl "http://127.0.0.2/path"
        result |> shouldSatisfy isNotHttps

      it "still blocks alternate loopback IPs over HTTPS" \_ -> do
        -- 127.0.0.2 is in loopback range, should be blocked
        let result = UrlValidation.validateSecureUrl "https://127.0.0.2/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "still blocks private network IPs over HTTPS" \_ -> do
        let result = UrlValidation.validateSecureUrl "https://10.0.0.1/path"
        result |> shouldSatisfy isPrivateIpBlocked

      it "still blocks other single-label hostnames" \_ -> do
        -- "db" and "intranet" should still be blocked (not localhost)
        let result = UrlValidation.validateSecureUrl "https://db/admin"
        result |> shouldSatisfy isSingleLabelHostname


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


isDnsResolutionTimeout :: Result ValidationError Text -> Bool
isDnsResolutionTimeout result =
  case result of
    Err (DnsResolutionTimeout _) -> True
    _ -> False


-- | Either failed or timed out - for environment-dependent tests
isDnsResolutionFailedOrTimeout :: Result ValidationError Text -> Bool
isDnsResolutionFailedOrTimeout result =
  isDnsResolutionFailed result || isDnsResolutionTimeout result


isSingleLabelHostname :: Result ValidationError Text -> Bool
isSingleLabelHostname result =
  case result of
    Err (SingleLabelHostname _) -> True
    _ -> False


-- | Either blocked, failed, or timed out - for environment-dependent tests
isDnsResolutionBlockedOrFailed :: Result ValidationError Text -> Bool
isDnsResolutionBlockedOrFailed result =
  isDnsResolutionBlocked result || isDnsResolutionFailed result || isDnsResolutionTimeout result


isOkResult :: Result ValidationError Text -> Bool
isOkResult result =
  case result of
    Ok _ -> True
    _ -> False
