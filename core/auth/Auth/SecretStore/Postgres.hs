-- | Postgres-backed SecretStore skeleton.
--
-- W5-B requires encrypted persistence for OAuth2 secrets. This module currently
-- provides the interface plus documented AES-256-GCM integration points while
-- delegating storage to the in-memory implementation.
--
-- TODO(#294): Replace in-memory delegation with a real Postgres implementation
-- using parameterized queries and encrypted-at-rest token payloads.
module Auth.SecretStore.Postgres (
  -- * Construction
  new,
  -- * Encryption helpers (exported for testing)
  encryptToken,
  decryptToken,
  EncryptionKey (..),
) where

import Auth.OAuth2.Types (TokenSet)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.SecretStore.InMemory qualified as InMemory
import Core
import Data.ByteString qualified as GhcBS
import Task qualified


-- | Symmetric encryption key for AES-256-GCM.
--
-- The key must be 32 bytes. Validation and key rotation will be handled when
-- the Postgres implementation is wired in.
newtype EncryptionKey = EncryptionKey GhcBS.ByteString


-- | Encrypt a token payload before persistence.
--
-- Intended AES-256-GCM approach:
--
-- * Generate a random 96-bit nonce per write
-- * Encrypt serialized token payload with AES-256-GCM
-- * Store nonce + ciphertext + auth tag
--
-- TODO(#294): Implement real AES-256-GCM encryption.
{-# INLINE encryptToken #-}
encryptToken :: EncryptionKey -> TokenSet -> Task Text TokenSet
encryptToken encryptionKey tokenSet = do
  let _ = encryptionKey
  Task.yield tokenSet


-- | Decrypt a persisted token payload.
--
-- TODO(#294): Implement real AES-256-GCM decryption + tag verification.
{-# INLINE decryptToken #-}
decryptToken :: EncryptionKey -> TokenSet -> Task Text TokenSet
decryptToken encryptionKey tokenSet = do
  let _ = encryptionKey
  Task.yield tokenSet


-- | Create a Postgres SecretStore.
--
-- TODO(#294): Use Postgres tables and transactional updates instead of
-- Auth.SecretStore.InMemory.
new :: EncryptionKey -> Task Text SecretStore
new encryptionKey = do
  inMemoryStore <- InMemory.new
  Task.yield
    SecretStore
      { get = getImpl encryptionKey inMemoryStore
      , put = putImpl encryptionKey inMemoryStore
      , delete = deleteImpl inMemoryStore
      , atomicModify = atomicModifyImpl encryptionKey inMemoryStore
      }


getImpl :: EncryptionKey -> SecretStore -> TokenKey -> Task Text (Maybe TokenSet)
getImpl encryptionKey inMemoryStore key = do
  maybeStored <- inMemoryStore.get key
  case maybeStored of
    Just storedTokenSet -> do
      decrypted <- decryptToken encryptionKey storedTokenSet
      Task.yield (Just decrypted)
    Nothing ->
      Task.yield Nothing


putImpl :: EncryptionKey -> SecretStore -> TokenKey -> TokenSet -> Task Text Unit
putImpl encryptionKey inMemoryStore key tokenSet = do
  encrypted <- encryptToken encryptionKey tokenSet
  inMemoryStore.put key encrypted


deleteImpl :: SecretStore -> TokenKey -> Task Text Unit
deleteImpl inMemoryStore key = do
  inMemoryStore.delete key


atomicModifyImpl :: EncryptionKey -> SecretStore -> TokenKey -> (Maybe TokenSet -> Maybe TokenSet) -> Task Text Unit
atomicModifyImpl encryptionKey inMemoryStore key transform = do
  let _ = encryptionKey
  inMemoryStore.atomicModify key transform
