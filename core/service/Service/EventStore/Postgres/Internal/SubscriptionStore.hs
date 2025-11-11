module Service.EventStore.Postgres.Internal.SubscriptionStore (
  SubscriptionStore (..),
  Error (..),
  new,
  addGlobalSubscription,
  addStreamSubscription,
) where

import Array qualified
import ConcurrentVar qualified
import Core
import Map qualified
import Maybe qualified
import Service.Event (StreamId)
import Service.EventStore (ReadAllMessage, ReadStreamMessage)
import Task qualified


data Error
  = UserHandlerError Text
  | OtherError


data SubscriptionStore eventType = SubscriptionStore
  { globalSubscriptions :: ConcurrentVar (Array (ReadAllMessage eventType -> Task Error Unit)),
    streamSubscriptions :: ConcurrentVar (Map StreamId (Array (ReadStreamMessage eventType -> Task Error Unit)))
  }


new :: Task Error (SubscriptionStore eventType)
new = do
  globalSubscriptions <- ConcurrentVar.containing Array.empty
  streamSubscriptions <- ConcurrentVar.containing Map.empty
  Task.yield (SubscriptionStore {globalSubscriptions, streamSubscriptions})


addGlobalSubscription :: (ReadAllMessage eventType -> Task Error Unit) -> SubscriptionStore eventType -> Task Error Unit
addGlobalSubscription subscription store = do
  store.globalSubscriptions
    |> ConcurrentVar.modify (Array.push subscription)


addStreamSubscription ::
  StreamId -> (ReadStreamMessage eventType -> Task Error Unit) -> SubscriptionStore eventType -> Task Error Unit
addStreamSubscription streamId subscription store = do
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.get streamId |> Maybe.withDefault Array.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Array.push subscription)
