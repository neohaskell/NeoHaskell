-- | URL validation for security-sensitive HTTP requests.
-- Enforces HTTPS and blocks SSRF attack vectors.
module Auth.UrlValidation (
  -- * Validation
  validateSecureUrl,
  validateSecureUrlWithDns,
  ValidationError (..),
) where

import Appendable ((++))
import Array (Array)
import Array qualified
import Basics
import Char (Char)
import Data.Char qualified as GhcChar
import Data.IP qualified as IP
import LinkedList qualified
import Maybe (Maybe (..))
import Network.URI qualified as URI
import Result (Result (..))
import System.Exit qualified as GhcExit
import System.Process qualified as GhcProcess
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Text.Read qualified as GhcRead


-- | Default DNS resolution timeout in seconds (5 seconds).
-- SECURITY: Prevents indefinite hangs from slow/unresponsive DNS servers.
-- Implementation uses subprocess isolation because getAddrInfo is a safe FFI
-- call that cannot be interrupted by async exceptions (timeout would be ineffective).
dnsTimeoutSeconds :: Int
dnsTimeoutSeconds = 5


-- | URL validation errors.
data ValidationError
  = -- | URL must use HTTPS scheme
    NotHttps Text
  | -- | URL points to private/loopback IP (SSRF risk)
    PrivateIpBlocked Text
  | -- | URL is malformed
    MalformedUrl Text
  | -- | Hostname is missing
    MissingHostname Text
  | -- | DNS resolution revealed private/loopback IP (SSRF risk)
    DnsResolutionBlocked Text Text
    -- ^ First Text is URL, second is the private IP that was resolved
  | -- | DNS resolution failed
    DnsResolutionFailed Text Text
    -- ^ First Text is URL, second is the error message
  | -- | DNS resolution timed out
    DnsResolutionTimeout Text
    -- ^ Text is the URL that timed out
  | -- | Single-label hostname (no dots) - requires FQDN
    SingleLabelHostname Text
    -- ^ Single-label hostnames may resolve via search domains to internal hosts
  deriving (Eq, Show)


-- | Validate that a URL is safe for security-sensitive requests.
-- Requirements:
-- 1. Must use HTTPS scheme
-- 2. Must not resolve to private/loopback IPs (SSRF prevention)
-- 3. Must have a valid, non-empty hostname
validateSecureUrl :: Text -> Result ValidationError Text
validateSecureUrl urlText = do
  let urlString = Text.toLinkedList urlText

  -- Parse the URL
  case URI.parseURI urlString of
    Nothing -> Err (MalformedUrl urlText)
    Just uri -> do
      -- Check scheme is HTTPS
      let scheme = URI.uriScheme uri
      case scheme of
        "https:" -> do
          -- Check hostname exists and is not a private IP
          case URI.uriAuthority uri of
            Nothing -> Err (MissingHostname urlText)
            Just auth -> do
              let host = URI.uriRegName auth
              -- Reject empty hostname
              case host of
                [] -> Err (MissingHostname urlText)
                _ -> do
                  -- Normalize to lowercase for consistent comparison
                  let normalizedHost = LinkedList.map toLowerChar host
                  -- SECURITY: Reject single-label hostnames (no dots)
                  -- Single-label names may resolve via DNS search domains to internal hosts
                  case isSingleLabelHostname normalizedHost of
                    True -> Err (SingleLabelHostname urlText)
                    False ->
                      case isPrivateOrLoopback normalizedHost of
                        True -> Err (PrivateIpBlocked urlText)
                        False -> Ok urlText
        _ -> Err (NotHttps urlText)


-- | Convert a character to lowercase.
toLowerChar :: Char -> Char
toLowerChar = GhcChar.toLower


-- | Check if hostname is a single-label name (no dots).
-- SECURITY: Single-label hostnames can resolve via DNS search domains,
-- potentially allowing SSRF via internal DNS names like "db" -> "db.corp.internal".
-- Literal IP addresses are allowed (they don't have dots in the hostname sense).
isSingleLabelHostname :: [Char] -> Bool
isSingleLabelHostname host =
  case host of
    [] -> False -- Empty is handled elsewhere
    _ ->
      -- Allow literal IPs (both IPv4 and bracketed IPv6) using strict parsing
      case isLiteralIpHostStr host of
        True -> False -- Literal IP, allow it
        False ->
          -- For hostnames, require at least one dot (FQDN)
          not (LinkedList.any (\c -> c == '.') host)


-- | Check if string is a valid literal IP address (IPv4 or bracketed IPv6).
-- Uses strict parsing via readMaybe - rejects malformed IPs like "999.999.999.999".
isLiteralIpHostStr :: [Char] -> Bool
isLiteralIpHostStr host =
  case host of
    [] -> False
    -- Bracketed IPv6 address (e.g., "[2001:db8::1]")
    '[' : _ -> do
      let strippedHost = stripIPv6Brackets host
      case GhcRead.readMaybe @IP.IPv6 strippedHost of
        Just _ -> True
        Nothing -> False
    -- Try to parse as IPv4
    _ ->
      case GhcRead.readMaybe @IP.IPv4 host of
        Just _ -> True
        Nothing -> False


-- | Check if a hostname is a private or loopback IP address.
-- This prevents SSRF attacks targeting internal services.
-- Handles both bare IPs and bracketed IPv6 (e.g., "[::1]" from URI parsing).
-- Assumes input is already normalized to lowercase.
isPrivateOrLoopback :: [Char] -> Bool
isPrivateOrLoopback host = do
  -- Reject empty hostname
  case host of
    [] -> True -- Treat empty as blocked for safety
    _ ->
      -- Check for localhost first (already lowercase from caller)
      case host == "localhost" of
        True -> True
        False ->
          -- Try to parse as IPv4
          case GhcRead.readMaybe @IP.IPv4 host of
            Just ipv4 -> isPrivateIPv4 ipv4
            Nothing ->
              -- Try to parse as IPv6 (handle bracketed form from URI)
              let strippedHost = stripIPv6Brackets host
               in case GhcRead.readMaybe @IP.IPv6 strippedHost of
                    Just ipv6 -> isPrivateIPv6 ipv6
                    Nothing -> False


-- | Strip brackets from IPv6 addresses.
-- URI parsing returns "[::1]" but IP.IPv6 parser expects "::1".
stripIPv6Brackets :: [Char] -> [Char]
stripIPv6Brackets hostChars =
  case hostChars of
    '[' : rest ->
      -- Strip leading bracket and trailing bracket if present
      case LinkedList.reverse rest of
        ']' : inner -> LinkedList.reverse inner
        _ -> rest -- Malformed, return as-is
    _ -> hostChars


-- | Check if IPv4 is in a non-globally-routable range.
-- Comprehensive list of special-use IPv4 addresses per IANA registries.
-- SECURITY: Block all non-public ranges for SSRF protection.
isNonRoutableIPv4 :: IP.IPv4 -> Bool
isNonRoutableIPv4 ip = do
  let ranges =
        [ -- RFC1918 Private Networks
          IP.makeAddrRange (IP.toIPv4 [10, 0, 0, 0]) 8, -- 10.0.0.0/8
          IP.makeAddrRange (IP.toIPv4 [172, 16, 0, 0]) 12, -- 172.16.0.0/12
          IP.makeAddrRange (IP.toIPv4 [192, 168, 0, 0]) 16, -- 192.168.0.0/16
          -- Loopback
          IP.makeAddrRange (IP.toIPv4 [127, 0, 0, 0]) 8, -- 127.0.0.0/8
          -- Link-local
          IP.makeAddrRange (IP.toIPv4 [169, 254, 0, 0]) 16, -- 169.254.0.0/16
          -- This network
          IP.makeAddrRange (IP.toIPv4 [0, 0, 0, 0]) 8, -- 0.0.0.0/8
          -- CGNAT (Carrier-Grade NAT)
          IP.makeAddrRange (IP.toIPv4 [100, 64, 0, 0]) 10, -- 100.64.0.0/10
          -- IETF Protocol Assignments
          IP.makeAddrRange (IP.toIPv4 [192, 0, 0, 0]) 24, -- 192.0.0.0/24
          -- Documentation/TEST-NET ranges
          IP.makeAddrRange (IP.toIPv4 [192, 0, 2, 0]) 24, -- 192.0.2.0/24 (TEST-NET-1)
          IP.makeAddrRange (IP.toIPv4 [198, 51, 100, 0]) 24, -- 198.51.100.0/24 (TEST-NET-2)
          IP.makeAddrRange (IP.toIPv4 [203, 0, 113, 0]) 24, -- 203.0.113.0/24 (TEST-NET-3)
          -- Benchmarking
          IP.makeAddrRange (IP.toIPv4 [198, 18, 0, 0]) 15, -- 198.18.0.0/15
          -- Multicast
          IP.makeAddrRange (IP.toIPv4 [224, 0, 0, 0]) 4, -- 224.0.0.0/4
          -- Reserved for future use
          IP.makeAddrRange (IP.toIPv4 [240, 0, 0, 0]) 4, -- 240.0.0.0/4
          -- Broadcast
          IP.makeAddrRange (IP.toIPv4 [255, 255, 255, 255]) 32 -- 255.255.255.255/32
        ]
  LinkedList.any (\range -> IP.isMatchedTo ip range) ranges


-- | Legacy alias for backward compatibility.
isPrivateIPv4 :: IP.IPv4 -> Bool
isPrivateIPv4 = isNonRoutableIPv4


-- | Check if IPv6 is in a non-globally-routable range.
-- Comprehensive list of special-use IPv6 addresses per IANA registries.
-- SECURITY: Block all non-public ranges for SSRF protection.
isNonRoutableIPv6 :: IP.IPv6 -> Bool
isNonRoutableIPv6 ip = do
  -- Unspecified address
  let unspecified = IP.toIPv6 [0, 0, 0, 0, 0, 0, 0, 0]
  let unspecifiedRange = IP.makeAddrRange unspecified 128

  -- Loopback (::1/128)
  let loopback = IP.toIPv6 [0, 0, 0, 0, 0, 0, 0, 1]
  let loopbackRange = IP.makeAddrRange loopback 128

  -- Unique local (fc00::/7)
  let uniqueLocal = IP.toIPv6 [0xfc00, 0, 0, 0, 0, 0, 0, 0]
  let uniqueLocalRange = IP.makeAddrRange uniqueLocal 7

  -- Link-local (fe80::/10)
  let linkLocal = IP.toIPv6 [0xfe80, 0, 0, 0, 0, 0, 0, 0]
  let linkLocalRange = IP.makeAddrRange linkLocal 10

  -- Multicast (ff00::/8)
  let multicast = IP.toIPv6 [0xff00, 0, 0, 0, 0, 0, 0, 0]
  let multicastRange = IP.makeAddrRange multicast 8

  -- IPv4-mapped IPv6 addresses (::ffff:0:0/96)
  let ipv4MappedPrefix = IP.toIPv6 [0, 0, 0, 0, 0, 0xffff, 0, 0]
  let ipv4MappedRange = IP.makeAddrRange ipv4MappedPrefix 96
  let isIPv4Mapped = IP.isMatchedTo ip ipv4MappedRange

  -- NAT64 well-known prefix (64:ff9b::/96)
  let nat64Prefix = IP.toIPv6 [0x64, 0xff9b, 0, 0, 0, 0, 0, 0]
  let nat64Range = IP.makeAddrRange nat64Prefix 96
  let isNat64 = IP.isMatchedTo ip nat64Range

  -- Discard-only (100::/64)
  let discardPrefix = IP.toIPv6 [0x100, 0, 0, 0, 0, 0, 0, 0]
  let discardRange = IP.makeAddrRange discardPrefix 64

  -- Documentation (2001:db8::/32)
  let docPrefix = IP.toIPv6 [0x2001, 0xdb8, 0, 0, 0, 0, 0, 0]
  let docRange = IP.makeAddrRange docPrefix 32

  -- 6to4 relay anycast (192.88.99.0/24 mapped)
  let relay6to4 = IP.toIPv6 [0x2002, 0xc058, 0x6300, 0, 0, 0, 0, 0]
  let relay6to4Range = IP.makeAddrRange relay6to4 24

  -- Extract embedded IPv4 from IPv4-mapped and check if private
  let ipv4MappedPrivate = case isIPv4Mapped of
        True -> extractAndCheckIPv4 ip
        False -> False

  -- Extract embedded IPv4 from NAT64 and check if private
  let nat64Private = case isNat64 of
        True -> extractAndCheckIPv4 ip
        False -> False

  IP.isMatchedTo ip unspecifiedRange
    || IP.isMatchedTo ip loopbackRange
    || IP.isMatchedTo ip uniqueLocalRange
    || IP.isMatchedTo ip linkLocalRange
    || IP.isMatchedTo ip multicastRange
    || IP.isMatchedTo ip discardRange
    || IP.isMatchedTo ip docRange
    || IP.isMatchedTo ip relay6to4Range
    || ipv4MappedPrivate
    || nat64Private


-- | Extract embedded IPv4 from IPv6 (last 32 bits) and check if non-routable.
extractAndCheckIPv4 :: IP.IPv6 -> Bool
extractAndCheckIPv4 ip = do
  let ipv6Words = IP.fromIPv6 ip
  case ipv6Words of
    [_, _, _, _, _, _, hi, lo] -> do
      let a = hi // 256
      let b = modBy 256 hi
      let c = lo // 256
      let d = modBy 256 lo
      let ipv4 = IP.toIPv4 [a, b, c, d]
      isNonRoutableIPv4 ipv4
    _ -> True -- SECURITY: Fail-closed on unexpected format


-- | Legacy alias for backward compatibility.
isPrivateIPv6 :: IP.IPv6 -> Bool
isPrivateIPv6 = isNonRoutableIPv6


-- | Validate a URL with DNS resolution for SSRF protection.
-- This is the stronger version that resolves DNS names and checks
-- ALL resolved IPs against private/loopback ranges.
--
-- Use this for user-provided URLs (e.g., IdP discovery endpoints).
-- For literal IP addresses (IPv4 or bracketed IPv6), DNS resolution is skipped
-- since they are already validated by validateSecureUrl.
validateSecureUrlWithDns ::
  forall error.
  Text ->
  Task error (Result ValidationError Text)
validateSecureUrlWithDns urlText = do
  -- First run the basic validation (scheme, format, literal IPs)
  case validateSecureUrl urlText of
    Err err -> Task.yield (Err err)
    Ok _ -> do
      -- Extract hostname and resolve DNS
      case extractHostname urlText of
        Nothing -> Task.yield (Err (MalformedUrl urlText))
        Just hostname -> do
          -- Skip DNS resolution for literal IP addresses
          -- (already validated by validateSecureUrl above)
          case isLiteralIpHost hostname of
            True -> Task.yield (Ok urlText)
            False -> do
              -- Resolve DNS and check all IPs
              resolveResult <- resolveDns hostname
              case resolveResult of
                DnsTimeout ->
                  Task.yield (Err (DnsResolutionTimeout urlText))
                DnsError errMsg ->
                  Task.yield (Err (DnsResolutionFailed urlText errMsg))
                DnsOk resolvedIps -> do
                  -- Check ALL resolved IPs
                  case findPrivateIp resolvedIps of
                    Just privateIp ->
                      Task.yield (Err (DnsResolutionBlocked urlText privateIp))
                    Nothing ->
                      Task.yield (Ok urlText)


-- | Check if a hostname is a literal IP address (IPv4 or bracketed IPv6).
-- Literal IPs don't need DNS resolution - they are validated directly.
-- Uses strict parsing via readMaybe - rejects malformed IPs like "999.999.999.999".
-- Examples:
--   "192.168.1.1" -> True (IPv4)
--   "[2001:db8::1]" -> True (bracketed IPv6)
--   "[::1]" -> True (bracketed IPv6 loopback)
--   "example.com" -> False (hostname)
--   "localhost" -> False (hostname, even though it resolves to loopback)
--   "999.999.999.999" -> False (invalid IPv4)
--   "1.2.3.4.5" -> False (invalid IPv4)
isLiteralIpHost :: Text -> Bool
isLiteralIpHost hostname = do
  let hostStr = Text.toLinkedList hostname
  isLiteralIpHostStr hostStr


-- | Extract hostname from a URL.
extractHostname :: Text -> Maybe Text
extractHostname urlText = do
  let urlString = Text.toLinkedList urlText
  case URI.parseURI urlString of
    Nothing -> Nothing
    Just uri ->
      case URI.uriAuthority uri of
        Nothing -> Nothing
        Just auth -> do
          let host = URI.uriRegName auth
          case host of
            [] -> Nothing
            _ -> Just (Text.fromLinkedList host)


-- | Result of DNS resolution with explicit timeout case.
data DnsResult
  = DnsOk [Text]
  | DnsError Text
  | DnsTimeout
  deriving (Eq, Show)


-- | Resolve DNS for a hostname, returning all A and AAAA records.
-- SECURITY: Uses subprocess isolation with enforced timeout.
--
-- Why subprocess? The standard getAddrInfo is a "safe" FFI call that masks
-- async exceptions during execution. This means System.Timeout.timeout
-- cannot interrupt it - the timeout only fires AFTER the FFI call completes.
-- A malicious DNS server could hang indefinitely, exhausting worker threads.
--
-- By spawning a subprocess (using system utilities like `getent` or `host`),
-- we can reliably kill the process on timeout, guaranteeing bounded execution.
resolveDns ::
  forall error.
  Text ->
  Task error DnsResult
resolveDns hostname = do
  -- Resolve both IPv4 (A records) and IPv6 (AAAA records)
  -- SECURITY: Must check both - attackers could use IPv6 for SSRF
  ipv4Result <- resolveDnsRecordType "A" hostname
  ipv6Result <- resolveDnsRecordType "AAAA" hostname
  -- Combine results - success if either family resolves
  case (ipv4Result, ipv6Result) of
    (DnsTimeout, _) -> Task.yield DnsTimeout
    (_, DnsTimeout) -> Task.yield DnsTimeout
    (DnsOk ipv4Ips, DnsOk ipv6Ips) ->
      Task.yield (DnsOk (ipv4Ips ++ ipv6Ips))
    (DnsOk ips, DnsError _) ->
      Task.yield (DnsOk ips)
    (DnsError _, DnsOk ips) ->
      Task.yield (DnsOk ips)
    (DnsError err1, DnsError _) ->
      Task.yield (DnsError err1)


-- | Resolve DNS for a specific record type (A or AAAA).
resolveDnsRecordType ::
  forall error.
  Text ->
  Text ->
  Task error DnsResult
resolveDnsRecordType recordType hostname = do
  -- Use `host` command which is available on both Linux and macOS
  -- -W sets the timeout in seconds (kills the query on timeout)
  -- -t specifies the record type (A for IPv4, AAAA for IPv6)
  let timeoutArg = Text.fromLinkedList [fmt|-W#{dnsTimeoutSeconds}|]
  let hostArgs =
        Array.fromLinkedList
          [ timeoutArg,
            "-t",
            recordType,
            hostname
          ]
  completion <- runDnsSubprocess "host" hostArgs
  case completion of
    DnsSubprocessTimeout -> Task.yield DnsTimeout
    DnsSubprocessError errMsg -> Task.yield (DnsError errMsg)
    DnsSubprocessSuccess output -> do
      let ips = parseHostOutput recordType output
      case ips of
        [] -> Task.yield (DnsError "No IP addresses resolved")
        _ -> Task.yield (DnsOk ips)


-- | Result of DNS subprocess execution.
data DnsSubprocessResult
  = DnsSubprocessSuccess Text
  | DnsSubprocessTimeout
  | DnsSubprocessError Text
  deriving (Eq, Show)


-- | Run DNS resolution in a subprocess.
-- The `host` command's -W flag provides built-in timeout support.
runDnsSubprocess ::
  forall error.
  Text ->
  Array Text ->
  Task error DnsSubprocessResult
runDnsSubprocess cmd args = do
  let cmdStr = Text.toLinkedList cmd
  let argsStr = Array.map Text.toLinkedList args |> Array.toLinkedList
  -- Use readProcessWithExitCode which captures stdout/stderr
  (exitCode, stdoutStr, stderrStr) <-
    GhcProcess.readProcessWithExitCode cmdStr argsStr ""
      |> Task.fromIO
  let out = Text.fromLinkedList stdoutStr
  let err = Text.fromLinkedList stderrStr
  case exitCode of
    GhcExit.ExitSuccess -> Task.yield (DnsSubprocessSuccess out)
    GhcExit.ExitFailure 1 -> do
      -- host returns 1 for "not found" - this is normal DNS failure
      -- Check output for NXDOMAIN or other messages
      Task.yield (DnsSubprocessError [fmt|Host not found: #{out}|])
    GhcExit.ExitFailure code -> do
      let errMsg = case Text.isEmpty err of
            True -> [fmt|DNS resolution failed (exit code #{code})|]
            False -> err
      Task.yield (DnsSubprocessError errMsg)


-- | Parse output from `host -t A` or `host -t AAAA` command.
-- Format for A records:
--   google.com has address 142.251.142.142
-- Format for AAAA records:
--   google.com has IPv6 address 2607:f8b0:4004:800::200e
-- Format for NXDOMAIN:
--   Host example.invalid not found: 3(NXDOMAIN)
-- We extract the IP addresses from lines containing "has address" or "has IPv6 address".
parseHostOutput :: Text -> Text -> [Text]
parseHostOutput recordType output = do
  let rawLines = Text.lines output |> Array.toLinkedList
  let extractIp line = do
        -- Look for "has address" (IPv4) or "has IPv6 address" (IPv6)
        let parts = Text.words line |> Array.toLinkedList
        case recordType of
          "A" -> extractIPv4FromHostLine parts
          "AAAA" -> extractIPv6FromHostLine parts
          _ -> Nothing
  let allIps = LinkedList.filterMap extractIp rawLines
  removeDuplicates allIps


-- | Extract IPv4 address from host output line.
-- Format: "domain.com has address 1.2.3.4"
extractIPv4FromHostLine :: [Text] -> Maybe Text
extractIPv4FromHostLine parts =
  -- Look for pattern: ... "has" "address" IP
  case parts of
    (_ : "has" : "address" : ip : _) -> Just ip
    _ -> Nothing


-- | Extract IPv6 address from host output line.
-- Format: "domain.com has IPv6 address 2001:db8::1"
extractIPv6FromHostLine :: [Text] -> Maybe Text
extractIPv6FromHostLine parts =
  -- Look for pattern: ... "has" "IPv6" "address" IP
  case parts of
    (_ : "has" : "IPv6" : "address" : ip : _) -> Just ip
    _ -> Nothing


-- | Remove duplicate elements from a list while preserving order.
-- PERF: O(n²) but DNS results are small (typically < 10 IPs).
removeDuplicates :: forall element. (Eq element) => [element] -> [element]
removeDuplicates items =
  case items of
    [] -> []
    (x : xs) -> x : removeDuplicates (LinkedList.filter (\y -> y != x) xs)


-- | Find the first private IP in a list of resolved IPs.
-- Returns the private IP as Text if found, Nothing otherwise.
findPrivateIp :: [Text] -> Maybe Text
findPrivateIp ips =
  case ips of
    [] -> Nothing
    (ip : rest) ->
      case isPrivateIpText ip of
        True -> Just ip
        False -> findPrivateIp rest


-- | Check if an IP address string is private/loopback.
-- SECURITY: Fail-closed - unknown formats are treated as blocked.
isPrivateIpText :: Text -> Bool
isPrivateIpText ipText = do
  let ipStr = Text.toLinkedList ipText
  -- Try IPv4 first
  case GhcRead.readMaybe @IP.IPv4 ipStr of
    Just ipv4 -> isNonRoutableIPv4 ipv4
    Nothing ->
      -- Try IPv6
      case GhcRead.readMaybe @IP.IPv6 ipStr of
        Just ipv6 -> isNonRoutableIPv6 ipv6
        -- SECURITY: Fail-closed - unknown format is blocked
        Nothing -> True
