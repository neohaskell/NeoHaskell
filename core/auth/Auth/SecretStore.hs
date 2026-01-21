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
-- -- Atomic refresh (prevents race conditions)
-- store.modify key \\maybeOld -> do
--   case maybeOld of
--     Nothing -> Task.yield Nothing
--     Just old -> do
--       newTokens <- OAuth2.refreshToken provider clientId clientSecret old.refreshToken
--       Task.yield (Just newTokens)
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
newtype TokenKey = TokenKey Text
  deriving (Generic, Show, Eq, Ord)


-- | Interface for secure token storage.
--
-- Implementations must ensure:
--
-- * Tokens are not logged
-- * Tokens can be deleted (not append-only)
-- * 'modify' is atomic (prevents refresh races)
--
-- @
-- data SecretStore = SecretStore
--   { get :: TokenKey -> Task Text (Maybe TokenSet)
--   , put :: TokenKey -> TokenSet -> Task Text Unit
--   , delete :: TokenKey -> Task Text Unit
--   , modify :: ...
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
  , -- | Atomically read and update tokens.
    --
    -- This prevents race conditions during token refresh where two
    -- workers might try to refresh simultaneously, potentially causing
    -- issues with refresh token rotation.
    --
    -- The function receives the current value (or Nothing) and returns
    -- the new value to store (or Nothing to delete).
    --
    -- @
    -- store.modify key \\maybeOld -> do
    --   case maybeOld of
    --     Nothing -> Task.yield Nothing  -- No change
    --     Just old -> do
    --       new <- refreshTokens old
    --       Task.yield (Just new)
    -- @
    modify :: TokenKey -> (Maybe TokenSet -> Task Text (Maybe TokenSet)) -> Task Text Unit
  }
