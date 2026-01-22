-- | # Auth.OAuth2.TransactionStore
--
-- Short-lived storage for OAuth2 flow transactions.
--
-- During the OAuth2 Authorization Code flow, we need to store temporary data
-- between the `/connect` and `/callback` requests:
--
-- * PKCE code verifier (needed to exchange the authorization code)
-- * Expiry time (for cleanup and validation)
--
-- = Key Properties
--
-- * **Short-lived**: Transactions expire after 5 minutes (configurable)
-- * **One-time use**: `consume` atomically retrieves and deletes
-- * **Replay protection**: Concurrent consume attempts - exactly one succeeds
--
-- = Implementations
--
-- * 'Auth.OAuth2.TransactionStore.InMemory' - Development only (single instance)
-- * Redis (future) - Production multi-instance deployments
--
-- = Usage
--
-- @
-- -- At /connect (store verifier)
-- let key = TransactionKey.fromText stateToken
-- let tx = Transaction { verifier = pkceVerifier, expiresAt = now + 300 }
-- store.put key tx
--
-- -- At /callback (consume verifier - one-time use)
-- let key = TransactionKey.fromText stateToken
-- maybeTx <- store.consume key
-- case maybeTx of
--   Nothing -> -- Replay attack or expired
--   Just tx -> -- Use tx.verifier for token exchange
-- @
--
-- = Security Notes
--
-- * Keys are hashed via TransactionKey to prevent raw state token storage
-- * The store does NOT validate expiry - caller must check `expiresAt`
-- * In-memory store breaks one-time use across multiple instances
module Auth.OAuth2.TransactionStore (
  -- * Types
  Transaction (..),
  TransactionStore (..),
  TransactionKey,

  -- * Key Construction
  TransactionKey.fromText,
  TransactionKey.toText,
) where

import Auth.OAuth2.TransactionStore.TransactionKey (TransactionKey)
import Auth.OAuth2.TransactionStore.TransactionKey qualified as TransactionKey
import Auth.OAuth2.Types (CodeVerifier)
import Basics
import Maybe (Maybe)
import Task (Task)
import Text (Text)


-- | A short-lived OAuth2 transaction.
--
-- Stored between `/connect` and `/callback` requests.
-- Contains all data needed for the callback that should NOT be in the state token.
data Transaction = Transaction
  { -- | PKCE code verifier for token exchange
    verifier :: CodeVerifier
  , -- | User ID from JWT (stored server-side for GDPR compliance)
    -- This is NOT in the state token to avoid PII exposure
    userId :: Text
  , -- | Unix timestamp when this transaction expires
    -- Caller is responsible for checking this
    expiresAt :: Int
  }
  deriving (Generic, Show, Eq)


-- | Interface for OAuth2 transaction storage.
--
-- Implementations must guarantee atomic `consume` semantics:
-- concurrent calls with the same key must return `Just` at most once.
data TransactionStore = TransactionStore
  { -- | Store a transaction.
    --
    -- If a transaction with the same key exists, it is overwritten.
    put :: TransactionKey -> Transaction -> Task Text Unit
  , -- | Retrieve a transaction without consuming it.
    --
    -- Returns `Nothing` if key doesn't exist.
    -- Does NOT check expiry - caller must validate `expiresAt`.
    get :: TransactionKey -> Task Text (Maybe Transaction)
  , -- | Atomically retrieve and delete a transaction.
    --
    -- This is the critical operation for one-time use:
    -- - Returns `Just tx` and deletes the entry atomically
    -- - Subsequent calls with same key return `Nothing`
    -- - Concurrent calls: exactly one returns `Just`
    --
    -- Returns `Nothing` if key doesn't exist (already consumed or never stored).
    consume :: TransactionKey -> Task Text (Maybe Transaction)
  , -- | Delete a transaction.
    --
    -- No-op if key doesn't exist.
    delete :: TransactionKey -> Task Text Unit
  }
