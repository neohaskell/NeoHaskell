module Service.Internal.Log.Worker (
  startWorker,
) where

import Array qualified
import AsyncTask qualified
import Basics
import Channel (Channel)
import Channel qualified
import Json qualified
import Service.Event (InsertionPayload (..), Insertion (..), InsertionType (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId (StreamId (..))
import Service.EventStore.Core (EventStore (..))
import Service.Internal.Log.Events (LogRecorded)
import Task (Task)
import Task qualified
import Text (Text)
import Uuid qualified


-- | Start a background worker that drains the log channel and writes to the EventStore.
--
-- The worker runs in a background thread and processes log records one at a time.
-- Insert errors are silently ignored (best-effort logging).
startWorker :: EventStore Json.Value -> Channel LogRecorded -> Task _ Unit
startWorker eventStore channel = do
  let workerLoop :: Task Text Unit
      workerLoop = do
        record <- Channel.read channel |> Task.mapError (\_ -> "Channel read failed" :: Text)
        _ <- insertLogRecord eventStore record |> Task.mapError (\_ -> "Insert failed" :: Text) |> Task.asResult
        workerLoop
  AsyncTask.run workerLoop
    |> Task.mapError (\_ -> "Worker start failed" :: Text)
    |> discard
  Task.yield unit


-- | Insert a single log record into the EventStore.
insertLogRecord :: EventStore Json.Value -> LogRecorded -> Task _ Unit
insertLogRecord eventStore record = do
  insertionId <- Uuid.generate
  metadata <- EventMetadata.new
  let jsonEvent = Json.encode record
  let insertion = Insertion
        { id = insertionId,
          event = jsonEvent,
          metadata = metadata
        }
  let payload = InsertionPayload
        { streamId = StreamId "internal-logs",
          entityName = EntityName "InternalLog",
          insertionType = AnyStreamState,
          insertions = Array.wrap insertion
        }
  eventStore.insert payload
    |> discard
