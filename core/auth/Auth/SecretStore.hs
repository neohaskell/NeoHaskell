-- | Secure storage interface for OAuth2 tokens.
--
-- This module provides an interface for storing sensitive credentials
-- like OAuth2 tokens. Tokens should NEVER be stored in the event store.
--
-- = Why SecretStore?
--
-- Event stores are append-only and often replicated to logs/backups.
-- Storing tokens there means:
--
-- * Cannot truly delete tokens on revocation
-- * Tokens end up in backups and debug logs
-- * Replay tooling becomes a secret-exfiltration path
--
-- SecretStore provides proper secret lifecycle (create, update, delete).
--
-- = Usage
--
-- @
-- -- Create a store (in-memory for development)
-- store <- SecretStore.InMemory.new
--
-- -- Store tokens after OAuth2 exchange
-- let key = TokenKey [fmt|oauth2:oura:#{userId}|]
-- store.put key tokens
--
-- -- Retrieve tokens
-- maybeTokens <- store.get key
--
-- -- Delete on disconnect
-- store.delete key
-- @
--
-- = Implementations
--
-- * 'Auth.SecretStore.InMemory' - Development only, single process
-- * (Future) Postgres, Vault, etc.
module Auth.SecretStore (
  -- * Types
  TokenKey (..),
  SecretStore (..),
) where

import Auth.OAuth2.Types (TokenSet)
import Basics
import Maybe (Maybe)
import Task (Task)
import Text (Text)


-- | Key for storing tokens.
--
-- Typically includes provider and user/entity identifier:
--
-- @
-- let key = TokenKey [fmt|oauth2:#{provider}:#{userId}|]
-- @
--
-- SECURITY: Show instance is redacted to prevent PII leakage in logs.
-- Keys often contain user IDs or other identifying information.
newtype TokenKey = TokenKey Text
  deriving (Generic, Eq, Ord)


-- | Redacted Show instance - NEVER reveals the actual key content.
-- Keys may contain user IDs or other PII that shouldn't appear in logs.
instance Show TokenKey where
  show _ = "TokenKey <REDACTED>"


-- | Interface for secure token storage.
--
-- Implementations must ensure:
--
-- * Tokens are not logged
-- * Tokens can be deleted (not append-only)
--
-- @
-- data SecretStore = SecretStore
--   { get :: TokenKey -> Task Text (Maybe TokenSet)
--   , put :: TokenKey -> TokenSet -> Task Text Unit
--   , delete :: TokenKey -> Task Text Unit
--   }
-- @
data SecretStore = SecretStore
  { -- | Retrieve tokens by key.
    -- Returns 'Nothing' if no tokens are stored for this key.
    get :: TokenKey -> Task Text (Maybe TokenSet)
  , -- | Store tokens for a key.
    -- Overwrites any existing tokens.
    put :: TokenKey -> TokenSet -> Task Text Unit
  , -- | Delete tokens for a key.
    -- No-op if key doesn't exist.
    delete :: TokenKey -> Task Text Unit
  }
