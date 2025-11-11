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


type GlobalSubscriptionCallback eventType =
  ReadAllMessage eventType -> Task Error Unit


type StreamSubscriptionCallback eventType =
  ReadStreamMessage eventType -> Task Error Unit


data SubscriptionStore eventType = SubscriptionStore
  { globalSubscriptions :: ConcurrentVar (Array (GlobalSubscriptionCallback eventType)),
    streamSubscriptions :: ConcurrentVar (Map StreamId (Array (StreamSubscriptionCallback eventType)))
  }


new :: Task Error (SubscriptionStore eventType)
new = do
  globalSubscriptions <- ConcurrentVar.containing Array.empty
  streamSubscriptions <- ConcurrentVar.containing Map.empty
  Task.yield (SubscriptionStore {globalSubscriptions, streamSubscriptions})


addGlobalSubscription :: GlobalSubscriptionCallback eventType -> SubscriptionStore eventType -> Task Error Unit
addGlobalSubscription subscription store = do
  store.globalSubscriptions
    |> ConcurrentVar.modify (Array.push subscription)


addStreamSubscription ::
  StreamId -> StreamSubscriptionCallback eventType -> SubscriptionStore eventType -> Task Error Unit
addStreamSubscription streamId subscription store = do
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.get streamId |> Maybe.withDefault Array.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Array.push subscription)
