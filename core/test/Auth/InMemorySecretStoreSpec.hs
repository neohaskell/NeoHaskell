-- | Tests for the in-memory SecretStore implementation.
--
-- Runs the shared SecretStore test suite against InMemory.new.
module Auth.InMemorySecretStoreSpec where

import Auth.SecretStore.InMemory qualified as InMemory
import Core
import Task qualified
import Test
import Test.Auth.SecretStore qualified as SecretStore


spec :: Spec Unit
spec = do
  let newStore = InMemory.new |> Task.mapError toText
  SecretStore.spec newStore
