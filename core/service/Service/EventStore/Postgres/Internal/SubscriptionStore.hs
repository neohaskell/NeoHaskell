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
import Service.EventStore.Core (SubscriptionId (..))
import Task qualified
import Uuid qualified


data Error
  = UserHandlerError Text
  | OtherError
  deriving (Show)


type SubscriptionCallback eventType =
  Event eventType -> Task Error Unit


type Subscriptions eventType =
  Map SubscriptionId (SubscriptionCallback eventType)


data SubscriptionStore eventType = SubscriptionStore
  { globalSubscriptions :: ConcurrentVar (Subscriptions eventType),
    streamSubscriptions :: ConcurrentVar (Map StreamId (Subscriptions eventType))
  }


new :: Task Error (SubscriptionStore eventType)
new = do
  globalSubscriptions <- ConcurrentVar.containing Map.empty
  streamSubscriptions <- ConcurrentVar.containing Map.empty
  Task.yield (SubscriptionStore {globalSubscriptions, streamSubscriptions})


addGlobalSubscription :: SubscriptionCallback eventType -> SubscriptionStore eventType -> Task Error SubscriptionId
addGlobalSubscription subscription store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  store.globalSubscriptions
    |> ConcurrentVar.modify (Map.set subId subscription)
  Task.yield subId


addStreamSubscription ::
  StreamId -> SubscriptionCallback eventType -> SubscriptionStore eventType -> Task Error SubscriptionId
addStreamSubscription streamId subscription store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse streamId Map.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Map.set subId subscription)
  Task.yield subId


getStreamSubscriptions ::
  StreamId -> SubscriptionStore eventType -> Task Error (Subscriptions eventType)
getStreamSubscriptions streamId store = do
  subscriptionsMap <- store.streamSubscriptions |> ConcurrentVar.peek
  subscriptionsMap
    |> Map.getOrElse streamId Map.empty
    |> Task.yield


dispatch :: StreamId -> Event eventType -> SubscriptionStore eventType -> Task Error Unit
dispatch streamId message store = do
  streamSubs <- store |> getStreamSubscriptions streamId
  globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek

  let wrapCallback :: msg -> (msg -> Task Error Unit) -> Task Text Unit
      wrapCallback msg callback = do
        callback msg |> Task.asResult |> discard

  let streamCallbacks = streamSubs |> Map.values |> Array.map (wrapCallback message)
  let globalCallbacks = globalSubs |> Map.values |> Array.map (wrapCallback message)
  let allCallbacks = streamCallbacks |> Array.append globalCallbacks

  allCallbacks |> AsyncTask.forEachConcurrently
