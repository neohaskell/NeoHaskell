module Service.EventStore.Postgres.Notifications (
  ReconnectConfig (..),
  NotificationConnection (..),
  HeartbeatSignal (..),
  mkReconnectConfig,
  connectTo,
  connect,
  startKeepalive,
  subscribeToStream,
) where

import AsyncTask qualified
import Basics
import Bytes qualified
import Channel qualified
import Core
import Data.ByteString qualified
import Data.Time.Clock.POSIX qualified as GhcPosix
import GHC.Float (Double)
import GHC.Real qualified as GhcReal
import Hasql.Connection qualified as Hasql
import Hasql.Notifications qualified as HasqlNotifications
import Json qualified
import Log qualified
import Service.Event (Event (..))
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Service.EventStore.Postgres.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Task qualified
import Var qualified


-- | Signal sent on the heartbeat channel.
data HeartbeatSignal
  = Heartbeat
  | Shutdown
  deriving (Show, Eq)


-- | Configuration for exponential-backoff reconnect.
-- Use 'mkReconnectConfig' to construct — validates invariants.
data ReconnectConfig = ReconnectConfig
  { initialBackoff :: !Int
  -- ^ Initial backoff in milliseconds. Must be > 0.
  , maxBackoff :: !Int
  -- ^ Maximum backoff in milliseconds. Must be >= initialBackoff.
  , backoffMultiplier :: !Float
  -- ^ Backoff growth factor. Must be > 1.0.
  }
  deriving (Show, Eq)


-- | Smart constructor for 'ReconnectConfig'.
-- Returns Err if invariants are violated.
mkReconnectConfig :: Int -> Int -> Float -> Result Text ReconnectConfig
mkReconnectConfig initial maxB multiplier =
  case initial > 0 && maxB >= initial && multiplier > 1.0 of
    True ->
      Ok
        ReconnectConfig
          { initialBackoff = initial
          , maxBackoff = maxB
          , backoffMultiplier = multiplier
          }
    False ->
      Err
        [fmt|Invalid ReconnectConfig: initialBackoff={initial} must be > 0, maxBackoff={maxB} must be >= initialBackoff, backoffMultiplier={multiplier} must be > 1.0|]


-- | A live LISTEN/NOTIFY connection with keepalive support.
data NotificationConnection = NotificationConnection
  { rawListenConnection :: !Hasql.Connection
  , rawQueryConnection :: !Hasql.Connection
  , heartbeatChannel :: !(Channel HeartbeatSignal)
  , reconnectConfig :: !ReconnectConfig
  , subscriptionStore :: !SubscriptionStore
  , shutdownRef :: !(Var Bool)
  }


-- | Default reconnect config: 500ms initial, 30s max, 2x multiplier.
defaultReconnectConfig :: ReconnectConfig
defaultReconnectConfig =
  ReconnectConfig
    { initialBackoff = 500
    , maxBackoff = 30000
    , backoffMultiplier = 2.0
    }


-- | Legacy entry point used by Internal.hs.
-- Wraps 'connect' with default reconnect config and starts keepalive.
connectTo ::
  Hasql.Connection ->
  Hasql.Connection ->
  SubscriptionStore ->
  Task Text Unit
connectTo listenConnection queryConnection store = do
  conn <- connect defaultReconnectConfig listenConnection queryConnection store
  startKeepalive conn


-- | Create a 'NotificationConnection' and register the LISTEN channel.
connect ::
  ReconnectConfig ->
  Hasql.Connection ->
  Hasql.Connection ->
  SubscriptionStore ->
  Task Text NotificationConnection
connect cfg listenConn queryConn store = do
  let channelToListen = HasqlNotifications.toPgIdentifier "global"
  HasqlNotifications.listen listenConn channelToListen
    |> Task.fromIO
    |> discard
  heartbeat <- Channel.newBounded 10
  shutdown <- Var.new False
  Log.info "LISTEN/NOTIFY listener started"
    |> Task.ignoreError
  Task.yield
    NotificationConnection
      { rawListenConnection = listenConn
      , rawQueryConnection = queryConn
      , heartbeatChannel = heartbeat
      , reconnectConfig = cfg
      , subscriptionStore = store
      , shutdownRef = shutdown
      }


-- | Start the keepalive/reconnect supervisor in the background.
-- Runs 'waitForNotifications' and restarts it with exponential backoff
-- on unexpected exits. Resets backoff after a long-running successful session.
startKeepalive :: NotificationConnection -> Task Text Unit
startKeepalive conn = do
  AsyncTask.run (keepaliveLoop conn conn.reconnectConfig.initialBackoff)
    |> Task.map (\_ -> ())


-- | Get current time in milliseconds since epoch.
getCurrentTimeMs :: Task Text Int
getCurrentTimeMs = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  Task.yield (round (GhcReal.realToFrac posixTime * 1000 :: Double))


-- | Inner reconnect loop. Runs waitForNotifications, detects exit type,
-- and either stops (clean) or restarts with backoff (unexpected).
-- Resets backoff to initialBackoff if the listener ran for longer than
-- maxBackoff ms (indicating a successful connection that later failed).
--
-- After any restart (Ok or Err), re-subscribes the LISTEN channel since
-- the Postgres connection may have been reset.
keepaliveLoop :: NotificationConnection -> Int -> Task Text Unit
keepaliveLoop conn currentBackoff = do
  isShuttingDown <- Var.get conn.shutdownRef
  case isShuttingDown of
    True ->
      Task.yield ()
    False -> do
      startTime <- getCurrentTimeMs
      result <-
        (conn.rawListenConnection
          |> HasqlNotifications.waitForNotifications
            (handler conn.rawQueryConnection conn.subscriptionStore)
          |> Task.fromIO :: Task Text Unit)
          |> Task.asResultSafe
      endTime <- getCurrentTimeMs
      let runDurationMs = endTime - startTime
      case result of
        Ok _ -> do
          -- waitForNotifications returned normally — this is unexpected since
          -- it should block indefinitely. Restart unless shutdown requested.
          Log.warn "LISTEN/NOTIFY listener returned unexpectedly, restarting"
            |> Task.ignoreError
          resubscribeListens conn
          keepaliveLoop conn conn.reconnectConfig.initialBackoff
        Err err -> do
          -- Unexpected failure — log with error details and reconnect with backoff
          -- Reset backoff if the listener ran for longer than maxBackoff ms
          -- (indicates a successful connection that later failed, not an immediate failure)
          let resetBackoff =
                runDurationMs > conn.reconnectConfig.maxBackoff
          let effectiveBackoff =
                case resetBackoff of
                  True -> conn.reconnectConfig.initialBackoff
                  False -> currentBackoff
          Log.critical
            [fmt|LISTEN/NOTIFY listener failed: #{err}, reconnecting in {effectiveBackoff}ms|]
            |> Task.ignoreError
          AsyncTask.sleep effectiveBackoff
          resubscribeListens conn
          let nextBackoff =
                min
                  conn.reconnectConfig.maxBackoff
                  (round (fromIntegral effectiveBackoff * conn.reconnectConfig.backoffMultiplier))
          keepaliveLoop conn nextBackoff


-- | Re-subscribe the global LISTEN channel after a connection reset.
-- Without this, a reconnected Postgres connection would silently miss
-- all notifications until the next explicit LISTEN.
resubscribeListens :: NotificationConnection -> Task Text Unit
resubscribeListens conn = do
  let channelToListen = HasqlNotifications.toPgIdentifier "global"
  HasqlNotifications.listen conn.rawListenConnection channelToListen
    |> Task.fromIO
    |> discard
  Log.info "LISTEN/NOTIFY re-subscribed after reconnect"
    |> Task.ignoreError


subscribeToStream ::
  Hasql.Connection ->
  StreamId ->
  Task Text Unit
subscribeToStream connection streamId = do
  let channelToListen = HasqlNotifications.toPgIdentifier (toText streamId)
  HasqlNotifications.listen connection channelToListen
    |> Task.fromIO
    |> discard


handler ::
  Hasql.Connection ->
  SubscriptionStore ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  IO ()
handler queryConnection store _channelName payloadLegacyBytes = do
  result <- processNotification queryConnection store payloadLegacyBytes |> Task.runResult
  case result of
    Err err ->
      ((Log.warn [fmt|#{err}|] |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
    Ok _ ->
      pass


processNotification ::
  Hasql.Connection ->
  SubscriptionStore ->
  Data.ByteString.ByteString ->
  Task Text Unit
processNotification queryConnection store payloadLegacyBytes = do
  Log.withScope [("component", "Notifications")] do
    notification <- decodeNotification payloadLegacyBytes
    event <- fetchFullEvent queryConnection notification.globalPosition
    Log.withScope [("component", "Notifications"), ("streamId", toText event.streamId)] do
      store |> SubscriptionStore.dispatch event.streamId event |> Task.mapError toText
      Log.debug "Event dispatched from notification" |> Task.ignoreError


decodeNotification ::
  Data.ByteString.ByteString ->
  Task Text Sessions.EventNotificationPayload
decodeNotification payloadLegacyBytes = do
  let result =
        payloadLegacyBytes
          |> Bytes.fromLegacy
          |> Json.decodeBytes :: Result Text Sessions.EventNotificationPayload
  case result of
    Err err -> Task.throw [fmt|Notification decode failed: #{err}|]
    Ok notification -> Task.yield notification


fetchFullEvent ::
  Hasql.Connection ->
  Int64 ->
  Task Text (Event Json.Value)
fetchFullEvent queryConnection globalPosition = do
  maybeRecord <-
    Sessions.selectEventByGlobalPositionSession globalPosition
      |> Sessions.runConnection queryConnection
      |> Task.mapError toText
  record <- case maybeRecord of
    Nothing -> Task.throw [fmt|Event not found for globalPosition #{globalPosition}|]
    Just r -> Task.yield r
  case Sessions.postgresRecordToEvent record of
    Err err -> Task.throw [fmt|Event record decode failed: #{err}|]
    Ok event -> Task.yield event
