module Service.EventStore.Postgres.SubscriptionStore (
  SubscriptionStore (..),
  Error (..),
  new,
  addGlobalSubscription,
  addGlobalSubscriptionFromPosition,
  addStreamSubscription,
  addStreamSubscriptionFromPosition,
  addEntitySubscription,
  addEntitySubscriptionFromPosition,
  getStreamSubscriptions,
  getEntitySubscriptions,
  dispatch,
  removeSubscription,
) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Json qualified
import Log qualified
import Map qualified
import Service.Event (EntityName, Event (..), StreamPosition)
import Service.Event.EventMetadata (EventMetadata (..))
import Service.EventStore.Core (SubscriptionId (..))
import Task qualified
import Uuid qualified


data Error
  = UserHandlerError Text
  | OtherError
  deriving (Eq, Show)


type SubscriptionCallback =
  Event Json.Value -> Task Text Unit


data SubscriptionInfo = SubscriptionInfo
  { callback :: SubscriptionCallback,
    startingGlobalPosition :: Maybe StreamPosition,
    entityNameFilter :: Maybe EntityName
  }
  deriving (Show)


type Subscriptions =
  Map SubscriptionId SubscriptionInfo


data SubscriptionStore = SubscriptionStore
  { globalSubscriptions :: ConcurrentVar Subscriptions,
    streamSubscriptions :: ConcurrentVar (Map StreamId Subscriptions),
    entitySubscriptions :: ConcurrentVar (Map EntityName Subscriptions)
  }


new :: Task Error SubscriptionStore
new = do
  globalSubscriptions <- ConcurrentVar.containing Map.empty
  streamSubscriptions <- ConcurrentVar.containing Map.empty
  entitySubscriptions <- ConcurrentVar.containing Map.empty
  Task.yield (SubscriptionStore {globalSubscriptions, streamSubscriptions, entitySubscriptions})


addGlobalSubscription :: SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addGlobalSubscription callback store = do
  subId <- Uuid.generate |> Task.map (\result -> result |> toText |> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = Nothing, entityNameFilter = Nothing}
  store.globalSubscriptions
    |> ConcurrentVar.modify (Map.set subId subscriptionInfo)
  Task.yield subId


addGlobalSubscriptionFromPosition ::
  Maybe StreamPosition -> SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addGlobalSubscriptionFromPosition startingPosition callback store = do
  subId <- Uuid.generate |> Task.map (\result -> result |> toText |> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = startingPosition, entityNameFilter = Nothing}
  store.globalSubscriptions
    |> ConcurrentVar.modify (Map.set subId subscriptionInfo)
  Task.yield subId


addStreamSubscription ::
  EntityName -> StreamId -> SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addStreamSubscription entityName streamId callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = Nothing, entityNameFilter = Just entityName}
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse streamId Map.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Map.set subId subscriptionInfo)
  Task.yield subId


addStreamSubscriptionFromPosition ::
  EntityName ->
  StreamId ->
  Maybe StreamPosition ->
  SubscriptionCallback ->
  SubscriptionStore ->
  Task Error SubscriptionId
addStreamSubscriptionFromPosition entityName streamId startingPosition callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = startingPosition, entityNameFilter = Just entityName}
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse streamId Map.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Map.set subId subscriptionInfo)
  Task.yield subId


addEntitySubscription ::
  EntityName -> SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addEntitySubscription entityName callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = Nothing, entityNameFilter = Just entityName}
  store.entitySubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse entityName Map.empty
    subscriptionsMap |> Map.set entityName (currentSubscriptions |> Map.set subId subscriptionInfo)
  Task.yield subId


addEntitySubscriptionFromPosition ::
  EntityName ->
  Maybe StreamPosition ->
  SubscriptionCallback ->
  SubscriptionStore ->
  Task Error SubscriptionId
addEntitySubscriptionFromPosition entityName startingPosition callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = startingPosition, entityNameFilter = Just entityName}
  store.entitySubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse entityName Map.empty
    subscriptionsMap |> Map.set entityName (currentSubscriptions |> Map.set subId subscriptionInfo)
  Task.yield subId


getStreamSubscriptions ::
  StreamId -> SubscriptionStore -> Task Error Subscriptions
getStreamSubscriptions streamId store = do
  subscriptionsMap <- store.streamSubscriptions |> ConcurrentVar.peek
  subscriptionsMap
    |> Map.getOrElse streamId Map.empty
    |> Task.yield


getEntitySubscriptions ::
  EntityName -> SubscriptionStore -> Task Error Subscriptions
getEntitySubscriptions entityName store = do
  subscriptionsMap <- store.entitySubscriptions |> ConcurrentVar.peek
  subscriptionsMap
    |> Map.getOrElse entityName Map.empty
    |> Task.yield


dispatch :: StreamId -> Event Json.Value -> SubscriptionStore -> Task Error Unit
dispatch streamId message store = do
  streamSubs <- store |> getStreamSubscriptions streamId
  entitySubs <- store |> getEntitySubscriptions message.entityName
  globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek

  let shouldDispatchToSubscription :: SubscriptionInfo -> Bool
      shouldDispatchToSubscription subInfo = do
        -- Note: using >= because if subscriber starts at position 0, they should receive event at position 0
        let positionCheck = case (subInfo.startingGlobalPosition, message.metadata.globalPosition) of
              (Just startPos, Just eventPos) -> eventPos >= startPos
              _ -> True
        let entityNameCheck = case subInfo.entityNameFilter of
              Just filterEntityName -> message.entityName == filterEntityName
              Nothing -> True
        positionCheck && entityNameCheck

  let wrapCallback :: msg -> (msg -> Task Text Unit) -> Task Text Unit
      wrapCallback msg callback = do
        callbackResult <- callback msg |> Task.asResult
        case callbackResult of
          Ok _ -> Task.yield unit
          Err err -> do
            Log.warn [fmt|Subscription callback failed: #{toText err}|]
              |> Task.ignoreError
            Task.yield unit

  -- Global subscriptions receive ALL events
  let globalCallbacks =
        globalSubs
          |> Map.values
          |> Array.takeIf shouldDispatchToSubscription
          |> Array.map (\subInfo -> wrapCallback message subInfo.callback)

  -- Stream subscriptions only receive events for their specific stream
  let streamCallbacks =
        streamSubs
          |> Map.values
          |> Array.takeIf shouldDispatchToSubscription
          |> Array.map (\subInfo -> wrapCallback message subInfo.callback)

  -- Entity subscriptions receive all events for their specific entity
  let entityCallbacks =
        entitySubs
          |> Map.values
          |> Array.takeIf shouldDispatchToSubscription
          |> Array.map (\subInfo -> wrapCallback message subInfo.callback)

  -- Combine all and execute all callbacks
  let allCallbacks = globalCallbacks |> Array.append streamCallbacks |> Array.append entityCallbacks

  allCallbacks |> AsyncTask.runAllIgnoringErrors


removeSubscription :: SubscriptionId -> SubscriptionStore -> Task Error Unit
removeSubscription subId store = do
  -- Remove from global subscriptions
  store.globalSubscriptions
    |> ConcurrentVar.modify (Map.remove subId)

  -- Remove from all stream subscriptions
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    subscriptionsMap
      |> Map.mapValues (\streamSubs -> streamSubs |> Map.remove subId)

  -- Remove from all entity subscriptions
  store.entitySubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    subscriptionsMap
      |> Map.mapValues (\entitySubs -> entitySubs |> Map.remove subId)

  Task.yield ()