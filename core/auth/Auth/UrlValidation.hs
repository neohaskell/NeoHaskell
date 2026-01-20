-- | URL validation for security-sensitive HTTP requests.
-- Enforces HTTPS and blocks SSRF attack vectors.
module Auth.UrlValidation (
  -- * Validation
  validateSecureUrl,
  validateSecureUrlWithDns,
  ValidationError (..),
) where

import Basics
import Char (Char)
import Control.Exception qualified as GhcException
import Data.Char qualified as GhcChar
import Data.Either qualified as GhcEither
import Data.IP qualified as IP
import LinkedList qualified
import Maybe (Maybe (..))
import Network.Socket qualified as GhcSocket
import Network.URI qualified as URI
import Prelude qualified as GhcPrelude
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Text.Read qualified as GhcRead
import ToText (toText)


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
                  case isPrivateOrLoopback normalizedHost of
                    True -> Err (PrivateIpBlocked urlText)
                    False -> Ok urlText
        _ -> Err (NotHttps urlText)


-- | Convert a character to lowercase.
toLowerChar :: Char -> Char
toLowerChar = GhcChar.toLower


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


-- | Check if IPv4 is in a private range.
-- Blocks: 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16, 127.0.0.0/8, 169.254.0.0/16
isPrivateIPv4 :: IP.IPv4 -> Bool
isPrivateIPv4 ip = do
  let ranges =
        [ IP.makeAddrRange (IP.toIPv4 [10, 0, 0, 0]) 8, -- 10.0.0.0/8 (private)
          IP.makeAddrRange (IP.toIPv4 [172, 16, 0, 0]) 12, -- 172.16.0.0/12 (private)
          IP.makeAddrRange (IP.toIPv4 [192, 168, 0, 0]) 16, -- 192.168.0.0/16 (private)
          IP.makeAddrRange (IP.toIPv4 [127, 0, 0, 0]) 8, -- 127.0.0.0/8 (loopback)
          IP.makeAddrRange (IP.toIPv4 [169, 254, 0, 0]) 16, -- 169.254.0.0/16 (link-local)
          IP.makeAddrRange (IP.toIPv4 [0, 0, 0, 0]) 8 -- 0.0.0.0/8 (this network)
        ]
  LinkedList.any (\range -> IP.isMatchedTo ip range) ranges


-- | Check if IPv6 is in a private range.
-- Blocks: ::1/128 (loopback), fc00::/7 (unique local), fe80::/10 (link-local)
isPrivateIPv6 :: IP.IPv6 -> Bool
isPrivateIPv6 ip = do
  -- ::1 loopback
  let loopback = IP.toIPv6 [0, 0, 0, 0, 0, 0, 0, 1]
  let loopbackRange = IP.makeAddrRange loopback 128

  -- Unique local (fc00::/7)
  let uniqueLocal = IP.toIPv6 [0xfc00, 0, 0, 0, 0, 0, 0, 0]
  let uniqueLocalRange = IP.makeAddrRange uniqueLocal 7

  -- Link-local (fe80::/10)
  let linkLocal = IP.toIPv6 [0xfe80, 0, 0, 0, 0, 0, 0, 0]
  let linkLocalRange = IP.makeAddrRange linkLocal 10

  -- Also check for IPv4-mapped IPv6 addresses (::ffff:x.x.x.x)
  let ipv4MappedPrefix = IP.toIPv6 [0, 0, 0, 0, 0, 0xffff, 0, 0]
  let ipv4MappedRange = IP.makeAddrRange ipv4MappedPrefix 96
  let isIPv4Mapped = IP.isMatchedTo ip ipv4MappedRange

  -- If it's an IPv4-mapped address, extract and check the IPv4 portion
  let ipv4MappedPrivate = case isIPv4Mapped of
        True -> do
          -- Extract the last 32 bits as IPv4
          -- fromIPv6 returns [Int] with 8 elements (each a 16-bit word)
          -- IPv4-mapped address is ::ffff:w.x.y.z
          -- The last two 16-bit words contain the IPv4 address
          let ipv6Words = IP.fromIPv6 ip
          case ipv6Words of
            [_, _, _, _, _, _, hi, lo] -> do
              let a = hi // 256
              let b = modBy 256 hi
              let c = lo // 256
              let d = modBy 256 lo
              let ipv4 = IP.toIPv4 [a, b, c, d]
              isPrivateIPv4 ipv4
            _ -> False -- Should never happen for valid IPv6
        False -> False

  IP.isMatchedTo ip loopbackRange
    || IP.isMatchedTo ip uniqueLocalRange
    || IP.isMatchedTo ip linkLocalRange
    || ipv4MappedPrivate


-- | Validate a URL with DNS resolution for SSRF protection.
-- This is the stronger version that resolves DNS names and checks
-- ALL resolved IPs against private/loopback ranges.
--
-- Use this for user-provided URLs (e.g., IdP discovery endpoints).
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
          -- Resolve DNS and check all IPs
          resolveResult <- resolveDns hostname
          case resolveResult of
            Err errMsg ->
              Task.yield (Err (DnsResolutionFailed urlText errMsg))
            Ok resolvedIps -> do
              -- Check ALL resolved IPs
              case findPrivateIp resolvedIps of
                Just privateIp ->
                  Task.yield (Err (DnsResolutionBlocked urlText privateIp))
                Nothing ->
                  Task.yield (Ok urlText)


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


-- | Resolve DNS for a hostname, returning all A and AAAA records.
resolveDns ::
  forall error.
  Text ->
  Task error (Result Text [Text])
resolveDns hostname = do
  let hostStr = Text.toLinkedList hostname
  let hints =
        GhcSocket.defaultHints
          { GhcSocket.addrFlags = [GhcSocket.AI_CANONNAME],
            GhcSocket.addrSocketType = GhcSocket.Stream
          }
  Task.fromIO do
    result <-
      GhcException.try @GhcException.SomeException
        (GhcSocket.getAddrInfo (Just hints) (Just hostStr) Nothing)
    case result of
      GhcEither.Left exc ->
        pure (Err (Text.fromLinkedList (GhcException.displayException exc)))
      GhcEither.Right addrInfos -> do
        let ips = LinkedList.map extractIpFromAddrInfo addrInfos
        let validIps = LinkedList.filterMap identity ips
        case validIps of
          [] -> pure (Err "No IP addresses resolved")
          _ -> pure (Ok validIps)


-- | Extract IP address string from AddrInfo.
extractIpFromAddrInfo :: GhcSocket.AddrInfo -> Maybe Text
extractIpFromAddrInfo addrInfo = do
  case GhcSocket.addrAddress addrInfo of
    GhcSocket.SockAddrInet _port hostAddr -> do
      -- IPv4 address
      let ipStr = GhcSocket.hostAddressToTuple hostAddr
      let (a, b, c, d) = ipStr
      Just
        ( Text.fromLinkedList
            [fmt|#{toText a}.#{toText b}.#{toText c}.#{toText d}|]
        )
    GhcSocket.SockAddrInet6 _port _flow hostAddr6 _scope -> do
      -- IPv6 address
      let (a, b, c, d, e, f, g, h) = GhcSocket.hostAddress6ToTuple hostAddr6
      Just
        ( Text.fromLinkedList
            [fmt|#{intToHex (GhcPrelude.fromIntegral a)}:#{intToHex (GhcPrelude.fromIntegral b)}:#{intToHex (GhcPrelude.fromIntegral c)}:#{intToHex (GhcPrelude.fromIntegral d)}:#{intToHex (GhcPrelude.fromIntegral e)}:#{intToHex (GhcPrelude.fromIntegral f)}:#{intToHex (GhcPrelude.fromIntegral g)}:#{intToHex (GhcPrelude.fromIntegral h)}|]
        )
    _ -> Nothing


-- | Convert an Int to 4-digit hexadecimal string.
intToHex :: Int -> [Char]
intToHex n = do
  let d1 = modBy 16 (n // 4096)
  let d2 = modBy 16 (n // 256)
  let d3 = modBy 16 (n // 16)
  let d4 = modBy 16 n
  [ hexDigit d1,
    hexDigit d2,
    hexDigit d3,
    hexDigit d4
    ]


-- | Convert a single digit (0-15) to hex character.
hexDigit :: Int -> Char
hexDigit digit =
  case digit of
    0 -> '0'
    1 -> '1'
    2 -> '2'
    3 -> '3'
    4 -> '4'
    5 -> '5'
    6 -> '6'
    7 -> '7'
    8 -> '8'
    9 -> '9'
    10 -> 'a'
    11 -> 'b'
    12 -> 'c'
    13 -> 'd'
    14 -> 'e'
    15 -> 'f'
    _ -> '0' -- Should never happen for valid hex digits


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
isPrivateIpText :: Text -> Bool
isPrivateIpText ipText = do
  let ipStr = Text.toLinkedList ipText
  -- Try IPv4 first
  case GhcRead.readMaybe @IP.IPv4 ipStr of
    Just ipv4 -> isPrivateIPv4 ipv4
    Nothing ->
      -- Try IPv6 (remove leading zeros for iproute compatibility)
      case GhcRead.readMaybe @IP.IPv6 ipStr of
        Just ipv6 -> isPrivateIPv6 ipv6
        Nothing -> False -- Unknown format, allow (fail-open for now)
