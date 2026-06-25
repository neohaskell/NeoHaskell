module Service.EventStore.Postgres.SubscriptionStore (
  SubscriptionStore (..),
  Error (..),
  new,
  addGlobalSubscription,
  addGlobalSubscriptionFromPosition,
  addStreamSubscription,
  addStreamSubscriptionFromPosition,
  addStreamSubscriptionWithCleanup,
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
import Text qualified
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
    entityNameFilter :: Maybe EntityName,
    -- | Run once when this subscription is removed. Releases any dedicated
    -- resource the subscription owns (the per-stream LISTEN connection). A
    -- no-op ('Task.yield unit') for subscriptions that own no dedicated resource.
    onRemove :: Task Text Unit
  }


-- Hand-written Show: 'callback' is a bare function ('SubscriptionCallback' is a
-- type synonym for @Event Json.Value -> Task Text Unit@), which has no sensible
-- 'Show' instance, so full @deriving Show@ is not possible. The function field is
-- rendered as a @<function>@ placeholder; 'onRemove' is a 'Task', shown via its
-- placeholder 'Show' instance. (ADR-0063 §1.)
instance Show SubscriptionInfo where
  show info = renderSubscriptionInfo info |> Text.toLinkedList


-- | Render a 'SubscriptionInfo' as 'Text'. The record-dot field accesses live
-- here (outside the 'fmt' interpolation) because 'OverloadedRecordDot' is not in
-- scope inside the quasi-quoter's @#{}@ expressions.
renderSubscriptionInfo :: SubscriptionInfo -> Text
renderSubscriptionInfo info = do
  let startingGlobalPosition = toText info.startingGlobalPosition
  let entityNameFilter = toText info.entityNameFilter
  let onRemove = toText info.onRemove
  [fmt|SubscriptionInfo {startingGlobalPosition = #{startingGlobalPosition}, entityNameFilter = #{entityNameFilter}, callback = <function>, onRemove = #{onRemove}}|]


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
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = Nothing, entityNameFilter = Nothing, onRemove = Task.yield unit}
  store.globalSubscriptions
    |> ConcurrentVar.modify (Map.set subId subscriptionInfo)
  Task.yield subId


addGlobalSubscriptionFromPosition ::
  Maybe StreamPosition -> SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addGlobalSubscriptionFromPosition startingPosition callback store = do
  subId <- Uuid.generate |> Task.map (\result -> result |> toText |> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = startingPosition, entityNameFilter = Nothing, onRemove = Task.yield unit}
  store.globalSubscriptions
    |> ConcurrentVar.modify (Map.set subId subscriptionInfo)
  Task.yield subId


addStreamSubscription ::
  EntityName -> StreamId -> SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addStreamSubscription entityName streamId callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = Nothing, entityNameFilter = Just entityName, onRemove = Task.yield unit}
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
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = startingPosition, entityNameFilter = Just entityName, onRemove = Task.yield unit}
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse streamId Map.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Map.set subId subscriptionInfo)
  Task.yield subId


addStreamSubscriptionWithCleanup ::
  EntityName ->
  StreamId ->
  Maybe StreamPosition ->
  Task Text Unit ->
  SubscriptionCallback ->
  SubscriptionStore ->
  Task Error SubscriptionId
addStreamSubscriptionWithCleanup entityName streamId startingPosition onRemove callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo =
        SubscriptionInfo
          { callback,
            startingGlobalPosition = startingPosition,
            entityNameFilter = Just entityName,
            onRemove
          }
  store.streamSubscriptions |> ConcurrentVar.modify \subscriptionsMap -> do
    let currentSubscriptions = subscriptionsMap |> Map.getOrElse streamId Map.empty
    subscriptionsMap |> Map.set streamId (currentSubscriptions |> Map.set subId subscriptionInfo)
  Task.yield subId


addEntitySubscription ::
  EntityName -> SubscriptionCallback -> SubscriptionStore -> Task Error SubscriptionId
addEntitySubscription entityName callback store = do
  subId <- Uuid.generate |> Task.map (toText .> SubscriptionId)
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = Nothing, entityNameFilter = Just entityName, onRemove = Task.yield unit}
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
  let subscriptionInfo = SubscriptionInfo {callback, startingGlobalPosition = startingPosition, entityNameFilter = Just entityName, onRemove = Task.yield unit}
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


-- | Find a subscription's 'onRemove' cleanup action across the global, stream,
-- and entity maps. Returns 'Task.yield unit' when the id is unknown, so removing
-- an already-removed or never-registered id is a no-op rather than an error.
findOnRemove :: SubscriptionId -> SubscriptionStore -> Task Error (Task Text Unit)
findOnRemove subId store = do
  globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
  streamSubsMap <- store.streamSubscriptions |> ConcurrentVar.peek
  entitySubsMap <- store.entitySubscriptions |> ConcurrentVar.peek
  -- Search the global map, then every stream's map, then every entity's map for
  -- the id. SubscriptionIds are unique, so at most one map holds it.
  let lookupIn subs acc =
        case acc of
          Just info -> Just info
          Nothing -> subs |> Map.get subId
  let fromStream = streamSubsMap |> Map.values |> Array.reduce lookupIn Nothing
  let fromEntity = entitySubsMap |> Map.values |> Array.reduce lookupIn Nothing
  let found =
        case globalSubs |> Map.get subId of
          Just info -> Just info
          Nothing -> case fromStream of
            Just info -> Just info
            Nothing -> fromEntity
  case found of
    Just info -> Task.yield info.onRemove
    Nothing -> Task.yield (Task.yield unit)


removeSubscription :: SubscriptionId -> SubscriptionStore -> Task Error Unit
removeSubscription subId store = do
  -- Read the subscription's cleanup BEFORE deleting, so the cleanup is taken
  -- (not left in place) and a double-remove cannot run it twice.
  cleanup <- store |> findOnRemove subId

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

  -- Run the cleanup best-effort: a failed release is logged and swallowed so
  -- unsubscribe stays total (ADR-0063 §2, design goal 4).
  cleanup
    |> Task.recover \err -> do
      Log.warn [fmt|Subscription #{toText subId} cleanup failed: #{err}|]
        |> Task.ignoreError
      Task.yield unit
    |> Task.mapError (\_ -> OtherError)