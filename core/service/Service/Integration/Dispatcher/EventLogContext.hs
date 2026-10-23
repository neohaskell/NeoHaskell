{-# LANGUAGE Strict #-}

-- | Carries event identity and optional attribution through integration logs.
module Service.Integration.Dispatcher.EventLogContext (
  EventLogContext,
  RetryDetails (..),
  fromEvent,
  withScope,
  logRetry,
  logResult,
) where

import Array (Array)
import Array qualified
import Basics
import Json qualified
import Log qualified
import Maybe (Maybe (..))
import Service.Event (Event (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EntityName qualified as EntityName
import Service.Event.StreamId qualified as StreamId
import Service.Integration.Dispatcher.IntegrationResult (IntegrationResult)
import Service.Integration.Dispatcher.IntegrationResult qualified as IntegrationResult
import Task (Task)
import Task qualified
import Text (Text)
import Uuid qualified


data EventLogContext = EventLogContext
  { eventId :: Text,
    streamId :: Text,
    entityName :: Text,
    correlationId :: Text,
    causationId :: Maybe Text,
    relatedUserSub :: Maybe Text
  }


data RetryDetails = RetryDetails
  { attempt :: Int,
    maxRetries :: Int,
    delayMs :: Int
  }


fromEvent :: Event Json.Value -> EventLogContext
fromEvent event =
  EventLogContext
    { eventId = event.metadata.eventId |> Uuid.toText,
      streamId = event.streamId |> StreamId.toText,
      entityName = event.entityName |> EntityName.toText,
      correlationId = sourceCorrelationId event,
      causationId = event.metadata.causationId,
      relatedUserSub = event.metadata.relatedUserSub
    }


withScope :: EventLogContext -> Task err value -> Task err value
withScope context task = Log.withScope (scopeFields context) task


logRetry :: EventLogContext -> RetryDetails -> Task err Unit
logRetry context details =
  withScope context do
    Log.record
      Log.Warn
      ( Array.fromLinkedList
          [ ("kind" Json..= ("retry" :: Text)),
            ("attempt" Json..= details.attempt),
            ("maxRetries" Json..= details.maxRetries),
            ("delayMs" Json..= details.delayMs)
          ]
      )
      "Retrying integration runner after transient failure"
      |> Task.ignoreError


logResult :: EventLogContext -> IntegrationResult -> Task err Unit
logResult context result =
  withScope context do
    Log.record
      (IntegrationResult.logLevel result)
      (IntegrationResult.logFields result)
      (IntegrationResult.logMessage result)
      |> Task.ignoreError


sourceCorrelationId :: Event Json.Value -> Text
sourceCorrelationId event = case event.metadata.correlationId of
  Just correlationId -> correlationId
  Nothing -> event.metadata.eventId |> Uuid.toText


scopeFields :: EventLogContext -> Array (Text, Text)
scopeFields context =
  Array.fromLinkedList
    [ ("component", "Dispatcher"),
      ("eventId", context.eventId),
      ("streamId", context.streamId),
      ("entityName", context.entityName),
      ("correlationId", context.correlationId)
    ]
    |> appendOptionalField "causationId" context.causationId
    |> appendOptionalField "relatedUserSub" context.relatedUserSub


appendOptionalField :: Text -> Maybe Text -> Array (Text, Text) -> Array (Text, Text)
appendOptionalField fieldName maybeValue fields = case maybeValue of
  Just value -> fields |> Array.append (Array.fromLinkedList [(fieldName, value)])
  Nothing -> fields
