-- | Hostname utilities for authentication modules.
--
-- Provides shared helpers for hostname detection, particularly
-- for localhost identification needed by both URL validation
-- and OAuth2 redirect URI handling.
module Auth.Hostname (
  -- * Localhost Detection
  isLocalhost,
) where

import Basics
import Char (Char)
import Data.List qualified as GhcList
import LinkedList qualified


-- | Check if a hostname is localhost (for development flows).
--
-- Returns True for:
--
-- * @"localhost"@ - standard hostname
-- * @"127.0.0.1"@ - IPv4 loopback
-- * @"[::1]"@ - IPv6 loopback (bracketed for URI compatibility)
-- * Any of the above with a port suffix (e.g., @"localhost:8080"@)
--
-- This function is used by:
--
-- * 'Auth.UrlValidation.validateSecureUrl' - to allow HTTP for localhost OAuth2 discovery
-- * 'Auth.OAuth2.Types.mkRedirectUri' - to allow HTTP for localhost redirect URIs
--
-- SECURITY: This is intentionally narrow. Only exact localhost patterns are matched.
-- Do not expand this to match subdomains (e.g., @"localhost.evil.com"@) or
-- other loopback variations (e.g., @"127.0.0.2"@).
--
-- @
-- isLocalhost "localhost"        -- True
-- isLocalhost "localhost:8080"   -- True
-- isLocalhost "127.0.0.1"        -- True
-- isLocalhost "[::1]"            -- True
-- isLocalhost "example.com"      -- False
-- isLocalhost "localhost.evil"   -- False
-- @
isLocalhost :: [Char] -> Bool
isLocalhost host =
  host == "localhost"
    || host == "127.0.0.1"
    || host == "[::1]"
    || LinkedList.any (\prefix -> GhcList.isPrefixOf prefix host) localhostPrefixes


-- | Localhost prefixes for port-suffixed variants.
-- Kept as top-level constant to avoid repeated allocation.
localhostPrefixes :: [[Char]]
localhostPrefixes = ["localhost:", "127.0.0.1:", "[::1]:"]
