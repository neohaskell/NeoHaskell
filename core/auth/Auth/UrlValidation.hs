-- | URL validation for security-sensitive HTTP requests.
-- Enforces HTTPS and blocks SSRF attack vectors.
module Auth.UrlValidation (
  -- * Validation
  validateSecureUrl,
  ValidationError (..),
) where

import Basics
import Char (Char)
import Data.IP qualified as IP
import LinkedList qualified
import Maybe (Maybe (..))
import Network.URI qualified as URI
import Result (Result (..))
import Text (Text)
import Text qualified
import Text.Read qualified as GhcRead


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
  deriving (Eq, Show)


-- | Validate that a URL is safe for security-sensitive requests.
-- Requirements:
-- 1. Must use HTTPS scheme
-- 2. Must not resolve to private/loopback IPs (SSRF prevention)
-- 3. Must have a valid hostname
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
              case isPrivateOrLoopback host of
                True -> Err (PrivateIpBlocked urlText)
                False -> Ok urlText
        _ -> Err (NotHttps urlText)


-- | Check if a hostname is a private or loopback IP address.
-- This prevents SSRF attacks targeting internal services.
-- Handles both bare IPs and bracketed IPv6 (e.g., "[::1]" from URI parsing).
isPrivateOrLoopback :: [Char] -> Bool
isPrivateOrLoopback host = do
  -- Check for localhost first
  case host == "localhost" of
    True -> True
    False ->
      -- Try to parse as IPv4
      case GhcRead.readMaybe @IP.IPv4 host of
        Just ipv4 -> isPrivateIPv4 ipv4
        Nothing ->
          -- Try to parse as IPv6 (handle bracketed form from URI)
          let normalizedHost = stripIPv6Brackets host
           in case GhcRead.readMaybe @IP.IPv6 normalizedHost of
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

  IP.isMatchedTo ip loopbackRange
    || IP.isMatchedTo ip uniqueLocalRange
    || IP.isMatchedTo ip linkLocalRange
