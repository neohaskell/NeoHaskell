-- | In-memory SecretStore implementation.
--
-- WARNING: This implementation is for DEVELOPMENT ONLY.
--
-- * Data is lost on process restart
-- * Not suitable for multi-process deployments
-- * Not encrypted at rest
--
-- For production, use a persistent implementation (Postgres, Vault, etc.).
--
-- = Usage
--
-- @
-- store <- SecretStore.InMemory.new
--
-- let key = TokenKey "oauth2:oura:user-123"
-- store.put key tokens
-- maybeTokens <- store.get key
-- @
module Auth.SecretStore.InMemory (
  new,
) where

import Auth.OAuth2.Types (TokenSet)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Text (Text)


-- | Create a new in-memory SecretStore.
--
-- WARNING: Development only. See module documentation.
--
-- @
-- store <- SecretStore.InMemory.new
-- @
new :: Task Text SecretStore
new = do
  storage <- ConcurrentVar.containing Map.empty
  Task.yield
    SecretStore
      { get = getImpl storage
      , put = putImpl storage
      , delete = deleteImpl storage
      , atomicModify = atomicModifyImpl storage
      }


-- | Internal storage type.
type Storage = ConcurrentVar (Map TokenKey TokenSet)


getImpl :: Storage -> TokenKey -> Task Text (Maybe TokenSet)
getImpl storage key = do
  store <- storage |> ConcurrentVar.peek
  store |> Map.get key |> Task.yield


putImpl :: Storage -> TokenKey -> TokenSet -> Task Text Unit
putImpl storage key tokens = do
  storage |> ConcurrentVar.modify (\store -> store |> Map.set key tokens)


deleteImpl :: Storage -> TokenKey -> Task Text Unit
deleteImpl storage key = do
  storage |> ConcurrentVar.modify (\store -> store |> Map.remove key)


atomicModifyImpl :: Storage -> TokenKey -> (Maybe TokenSet -> Maybe TokenSet) -> Task Text Unit
atomicModifyImpl storage key transform = do
  storage
    |> ConcurrentVar.modify
      ( \store -> do
          let currentValue = store |> Map.get key
          let newValue = transform currentValue
          case newValue of
            Just tokens ->
              store |> Map.set key tokens
            Nothing ->
              store |> Map.remove key
      )
