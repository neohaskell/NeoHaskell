module Service.Internal.Routes (
  handleInternalLogs,
) where

import Array qualified
import Basics
import Bytes qualified
import Data.Time.Clock qualified as GhcClock
import Data.Time.Clock.POSIX qualified as GhcPosix
import Data.Time.Format.ISO8601 qualified as GhcISO8601
import DateTime qualified
import GHC.Real qualified
import Json qualified
import LinkedList qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Result (Result (..))
import Service.Event (Event (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId (StreamId (..))
import Service.EventStore.Core (EventStore (..), collectStreamEvents)
import Service.Internal.Log.Events (LogRecorded (..))
import Stream qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Handle GET /internal/logs requests.
--
-- Supports query parameters:
--   - limit: max number of logs to return (default 100, clamped [1, 1000])
--   - since: ISO 8601 timestamp to filter logs after
handleInternalLogs ::
  EventStore Json.Value ->
  Wai.Request ->
  (Wai.Response -> Task Text Wai.ResponseReceived) ->
  Task Text Wai.ResponseReceived
handleInternalLogs eventStore request respond = do
  -- Parse query parameters
  let queryParams = Wai.queryString request
  let getParam name = do
        let matches = queryParams |> LinkedList.filter (\(k, _) -> k == name)
        case matches of
          [] -> Nothing
          ((_, v) : _) -> v |> fmap (\bs -> Bytes.fromLegacy bs |> Text.fromBytes)

  -- Parse limit parameter (default 100, clamp [1, 1000])
  let limitParam = getParam "limit"
  let limit = case limitParam of
        Nothing -> 100
        Just text ->
          case Text.toInt text of
            Nothing -> 100
            Just n ->
              if n < 1 then 1
              else if n > 1000 then 1000
              else n

  -- Parse since parameter (optional ISO 8601 DateTime)
  let sinceParam = getParam "since"
  let maybeSinceDateTime = case sinceParam of
        Nothing -> Nothing
        Just text -> do
          let str = Text.toLinkedList text
          case GhcISO8601.iso8601ParseM str of
            Nothing -> Nothing
            Just (utcTime :: GhcClock.UTCTime) -> do
              let posixTime = GhcPosix.utcTimeToPOSIXSeconds utcTime
              let epochSeconds = GHC.Real.truncate posixTime :: Int64
              Just (DateTime.fromEpochSeconds epochSeconds)

  -- Read events from the event store
  readResult <-
    eventStore.readAllStreamEvents (EntityName "InternalLog") (StreamId "internal-logs")
      |> Task.asResult

  case readResult of
    Err _ -> do
      -- Stream might not exist yet (no logs recorded)
      let emptyResponse = Json.object
            [ "logs" Json..= ([] :: [Json.Value]),
              "count" Json..= (0 :: Int)
            ]
      let responseBytes = Json.encodeText emptyResponse |> Text.toBytes |> Bytes.toLazyLegacy
      let response200 = Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/json")] responseBytes
      respond response200
    Ok stream -> do
      -- Collect all stream messages into an array
      messages <- Stream.toArray stream
      let events = collectStreamEvents messages

      -- Decode events from Json.Value to LogRecorded and filter
      let decodedLogs = events |> Array.flatMap \evt -> do
            case Json.decode @LogRecorded evt.event of
              Ok logRecord -> Array.wrap logRecord
              Err _ -> Array.empty

      -- Filter by since timestamp if provided
      let filteredLogs = case maybeSinceDateTime of
            Nothing -> decodedLogs
            Just sinceTime -> decodedLogs |> Array.takeIf (\logRecord -> logRecord.timestamp > sinceTime)

      -- Take the last N logs
      let logCount = Array.length filteredLogs
      let finalLogs = if logCount > limit
            then filteredLogs |> Array.drop (logCount - limit)
            else filteredLogs

      -- Build JSON response
      let finalCount = Array.length finalLogs
      let responseValue = Json.object
            [ "logs" Json..= finalLogs,
              "count" Json..= finalCount
            ]
      let responseBytes = Json.encodeText responseValue |> Text.toBytes |> Bytes.toLazyLegacy
      let response200 = Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/json")] responseBytes
      respond response200
