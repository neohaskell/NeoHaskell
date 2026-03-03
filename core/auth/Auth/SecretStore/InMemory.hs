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
import Auth.SecretStore (SecretStore (..), TokenKey)
import Basics
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Text (Text)
import Tuple qualified


-- | Per-key storage: each token key gets its own ConcurrentVar.
-- This allows concurrent operations on different keys without serialization.
type PerKeyStore = ConcurrentMap TokenKey (ConcurrentVar (Maybe TokenSet))


-- | Create a new in-memory SecretStore.
--
-- WARNING: Development only. See module documentation.
--
-- @
-- store <- SecretStore.InMemory.new
-- @
new :: Task Text SecretStore
new = do
  store <- ConcurrentMap.new
  Task.yield (makeStore store)


makeStore :: PerKeyStore -> SecretStore
makeStore store =
  SecretStore
    { get = getImpl store
    , put = putImpl store
    , delete = deleteImpl store
    , atomicModify = atomicModifyImpl store
    , atomicModifyReturning = atomicModifyReturningImpl store
    }


-- | Get or create a ConcurrentVar for a key.
-- If the key already has a var, returns it; otherwise inserts a new one.
getOrCreateVar :: PerKeyStore -> TokenKey -> Task Text (ConcurrentVar (Maybe TokenSet))
getOrCreateVar store key = do
  newVar <- ConcurrentVar.containing Nothing
  resultPair <- store |> ConcurrentMap.getOrInsert key newVar
  Task.yield (Tuple.first resultPair)


getImpl :: PerKeyStore -> TokenKey -> Task Text (Maybe TokenSet)
getImpl store key = do
  maybeVar <- store |> ConcurrentMap.get key
  case maybeVar of
    Nothing -> Task.yield Nothing
    Just var -> ConcurrentVar.peek var


putImpl :: PerKeyStore -> TokenKey -> TokenSet -> Task Text Unit
putImpl store key tokens = do
  var <- getOrCreateVar store key
  var |> ConcurrentVar.modify (\_ -> Just tokens)


deleteImpl :: PerKeyStore -> TokenKey -> Task Text Unit
deleteImpl store key = do
  maybeVar <- store |> ConcurrentMap.get key
  case maybeVar of
    Nothing -> Task.yield ()
    Just var -> var |> ConcurrentVar.modify (\_ -> Nothing)


{-# INLINE atomicModifyImpl #-}
atomicModifyImpl :: PerKeyStore -> TokenKey -> (Maybe TokenSet -> Maybe TokenSet) -> Task Text Unit
atomicModifyImpl store key f = do
  var <- getOrCreateVar store key
  var |> ConcurrentVar.modify f


{-# INLINE atomicModifyReturningImpl #-}
atomicModifyReturningImpl ::
  forall result.
  PerKeyStore ->
  TokenKey ->
  (Maybe TokenSet -> Task Text (Maybe TokenSet, result)) ->
  Task Text result
atomicModifyReturningImpl store key f = do
  var <- getOrCreateVar store key
  -- ConcurrentVar.modifyReturning requires Task Never callback.
  -- We wrap f to run as IO (panicking on errors) then widen error type.
  -- In practice, all callbacks use Task.yield so errors never occur.
  let wrappedF value = f value |> Task.runOrPanic |> Task.fromIO
  (var |> ConcurrentVar.modifyReturning wrappedF) |> Task.mapError never
