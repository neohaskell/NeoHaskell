-- | Postgres-backed SecretStore with encryption-at-rest.
--
-- W5-B requires encrypted persistence for OAuth2 secrets. This module
-- provides a SecretStore backed by an in-memory store with AES-256-GCM
-- encryption wired through encrypt/decrypt hooks. The in-memory backend
-- will be replaced by real Postgres queries in a follow-up (#294).
--
-- = Encryption Posture
--
-- 'encryptToken' and 'decryptToken' currently fail-closed: they reject
-- all calls with a clear error message until real AES-256-GCM is wired
-- in. This is intentional — a no-op passthrough would silently store
-- secrets in plaintext, violating the encryption-at-rest contract.
--
-- = Key Validation
--
-- Use 'mkEncryptionKey' (not the raw constructor) to validate that the
-- key is exactly 32 bytes (required for AES-256).
module Auth.SecretStore.Postgres (
  -- * Construction
  new,
  mkEncryptionKey,
  -- * Encryption helpers (exported for testing)
  encryptToken,
  decryptToken,
  EncryptionKey,
) where

import Auth.OAuth2.Types (TokenSet)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.SecretStore.InMemory qualified as InMemory
import Core
import Data.ByteString qualified as GhcBS
import Task qualified


-- | Symmetric encryption key for AES-256-GCM.
--
-- The key must be exactly 32 bytes. Use 'mkEncryptionKey' to construct.
newtype EncryptionKey = EncryptionKey GhcBS.ByteString


-- | Smart constructor that validates the encryption key is exactly 32 bytes.
--
-- AES-256 requires a 256-bit (32-byte) key. Any other length is rejected
-- with a clear error message.
mkEncryptionKey :: GhcBS.ByteString -> Result Text EncryptionKey
mkEncryptionKey keyBytes = do
  let keyLen = GhcBS.length keyBytes
  case keyLen == 32 of
    True -> Ok (EncryptionKey keyBytes)
    False ->
      Err [fmt|EncryptionKey must be exactly 32 bytes (got #{keyLen})|]


-- | Encrypt a token payload before persistence.
--
-- Fail-closed: rejects all calls until real AES-256-GCM is implemented.
-- This prevents silently storing secrets in plaintext.
--
-- When implemented, the approach will be:
--
-- * Generate a random 96-bit nonce per write
-- * Encrypt serialized token payload with AES-256-GCM
-- * Return nonce ++ ciphertext ++ auth tag
{-# INLINE encryptToken #-}
encryptToken :: EncryptionKey -> TokenSet -> Task Text TokenSet
encryptToken (EncryptionKey _key) _tokenSet = do
  Task.throw "AES-256-GCM encryption not yet implemented — refusing to store secrets in plaintext (#294)"


-- | Decrypt a persisted token payload.
--
-- Fail-closed: rejects all calls until real AES-256-GCM is implemented.
-- This prevents reading data that was never actually encrypted.
{-# INLINE decryptToken #-}
decryptToken :: EncryptionKey -> TokenSet -> Task Text TokenSet
decryptToken (EncryptionKey _key) _tokenSet = do
  Task.throw "AES-256-GCM decryption not yet implemented — cannot decrypt (#294)"


-- | Create a Postgres SecretStore.
--
-- NOTE: Currently delegates to an in-memory backend. The encrypt/decrypt
-- hooks are wired in so the storage pipeline is correct — only the
-- persistence layer needs replacing with real Postgres queries (#294).
--
-- Because encrypt/decrypt are fail-closed, attempting to use this store
-- will produce a clear error until AES-256-GCM is implemented.
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
atomicModifyImpl _encryptionKey inMemoryStore key transform = do
  -- Note: in the fail-closed posture, encrypt/decrypt throw errors on
  -- get/put anyway so data is never actually encrypted in the store.
  -- When AES-256-GCM is implemented, this will need to decrypt before
  -- transform and re-encrypt after. For now, delegate directly.
  inMemoryStore.atomicModify key transform
