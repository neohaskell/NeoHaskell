module Service.EventStore.Postgres.Internal.SubscriptionStore (
  SubscriptionStore (..),
  Error (..),
  new,
  addGlobalSubscription,
  addStreamSubscription,
  getStreamSubscriptions,
  dispatch,
) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Map qualified
import Service.Event (Event, StreamId)
import Task qualified


data Error
  = UserHandlerError Text
  | OtherError
  deriving (Show)


type SubscriptionCallback eventType =
  Event eventType -> Task Error Unit


data SubscriptionStore eventType = SubscriptionStore
  { globalSubscriptions :: ConcurrentVar (Array (SubscriptionCallback eventType)),
    streamSubscriptions :: ConcurrentVar (Map StreamId (Array (SubscriptionCallback eventType)))
  }


new :: Task Error (SubscriptionStore eventType)
new = do
  globalSubscriptions <- ConcurrentVar.containing Array.empty
  streamSubscriptions <- ConcurrentVar.containing Map.empty
  Task.yield (SubscriptionStore {globalSubscriptions, streamSubscriptions})


addGlobalSubscription :: SubscriptionCallback eventType -> SubscriptionStore eventType -> Task Error Unit
addGlobalSubscription subscription store = do
  store.globalSubscriptions
    |> ConcurrentVar.modify (Array.push subscription)


addStreamSubscription ::
  StreamId -> SubscriptionCallback eventType -> SubscriptionStore eventType -> Task Error Unit
addStreamSubscription streamId subscription store = do
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse streamId Array.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Array.push subscription)


getStreamSubscriptions ::
  StreamId -> SubscriptionStore eventType -> Task Error (Array (SubscriptionCallback eventType))
getStreamSubscriptions streamId store = do
  subscriptionsMap <- store.streamSubscriptions |> ConcurrentVar.peek
  subscriptionsMap
    |> Map.getOrElse streamId Array.empty
    |> Task.yield


dispatch :: StreamId -> Event eventType -> SubscriptionStore eventType -> Task Error Unit
dispatch streamId message store = do
  streamSubs <- store |> getStreamSubscriptions streamId
  globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek

  let wrapCallback :: msg -> (msg -> Task Error Unit) -> Task Text Unit
      wrapCallback msg callback = do
        callback msg |> Task.asResult |> discard

  let streamCallbacks = streamSubs |> Array.map (wrapCallback message)
  let globalCallbacks = globalSubs |> Array.map (wrapCallback message)
  let allCallbacks = streamCallbacks |> Array.append globalCallbacks

  allCallbacks |> AsyncTask.forEachConcurrently
