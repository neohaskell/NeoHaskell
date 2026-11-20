-- | Carry the origin and request context of a command. Normalize a source
-- before using it to attribute events and populate trace fields.
module Service.CommandExecutor.Source (
  CommandSource (..),
  sourceContext,
  sourceMetadata,
  tenantFromRequest,
  normalize,
  attributeInsertionMetadata,
  commandTraceFields,
) where

import Array (Array)
import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.Auth (RequestContext (..))
import Service.AccessControl (AccessError (..))
import Service.CommandExecutor.CommandExecutionFailure (CommandExecutionFailure)
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata)
import Service.Event.EventMetadata qualified as EventMetadata
import Task (Task)
import Task qualified
import Text (Text)
import Uuid qualified


data CommandSource
  = FromRequest RequestContext
  | FromEvent RequestContext EventMetadata


sourceContext :: CommandSource -> RequestContext
sourceContext source = case source of
  FromRequest context -> context
  FromEvent context _ -> context


sourceMetadata :: CommandSource -> Maybe EventMetadata
sourceMetadata source = case source of
  FromRequest _ -> Nothing
  FromEvent _ metadata -> Just metadata


tenantFromRequest :: CommandSource -> Result AccessError Uuid.Uuid
tenantFromRequest source =
  case (sourceContext source).user of
    Nothing -> Err Unauthenticated
    Just claims ->
      case claims.tenantId of
        Nothing -> Err Forbidden
        Just tenantIdText ->
          case Uuid.fromText tenantIdText of
            Nothing -> Err Forbidden
            Just tenantUuid -> Ok tenantUuid


normalize :: CommandSource -> Task CommandExecutionFailure CommandSource
normalize source = do
  let context = sourceContext source
  if context.requestId == Uuid.nil
    then do
      requestId <- Uuid.generate
      let normalizedContext = context {requestId}
      case source of
        FromRequest _ -> Task.yield (FromRequest normalizedContext)
        FromEvent _ metadata -> Task.yield (FromEvent normalizedContext metadata)
    else Task.yield source


eventMetadataFor :: CommandSource -> EventMetadata -> EventMetadata
eventMetadataFor source freshMetadata = case source of
  FromRequest context -> do
    let relatedUserSub = context.user |> Maybe.map (\claims -> claims.sub)
    freshMetadata
      { EventMetadata.relatedUserSub = relatedUserSub,
        EventMetadata.correlationId = Just (Uuid.toText context.requestId),
        EventMetadata.causationId = Nothing
      }
  FromEvent _ metadata -> do
    let sourceEventId = Uuid.toText metadata.eventId
    let correlationId = metadata.correlationId |> Maybe.withDefault sourceEventId
    freshMetadata
      { EventMetadata.relatedUserSub = metadata.relatedUserSub,
        EventMetadata.correlationId = Just correlationId,
        EventMetadata.causationId = Just sourceEventId
      }


attributeInsertionMetadata :: CommandSource -> Event.Insertion event -> Event.Insertion event
attributeInsertionMetadata source insertion =
  Event.Insertion
    { id = insertion.id,
      event = insertion.event,
      metadata = eventMetadataFor source insertion.metadata
    }


optionalTextField :: Text -> Maybe Text -> Array (Text, Json.Value)
optionalTextField fieldName maybeValue = case maybeValue of
  Just value -> Array.wrap (fieldName Json..= value)
  Nothing -> Array.empty


commandTraceFields :: CommandSource -> Array (Text, Json.Value)
commandTraceFields source = do
  let context = sourceContext source
  let sourceEventMetadata = sourceMetadata source
  let requestIdText =
        if context.requestId == Uuid.nil
          then Nothing
          else Just (Uuid.toText context.requestId)
  let sourceEventId = sourceEventMetadata |> Maybe.map (\metadata -> Uuid.toText metadata.eventId)
  let correlationId = case sourceEventMetadata of
        Just metadata -> Just (metadata.correlationId |> Maybe.withDefault (Uuid.toText metadata.eventId))
        Nothing -> requestIdText
  let relatedUserSub = case sourceEventMetadata of
        Just metadata -> metadata.relatedUserSub
        Nothing -> context.user |> Maybe.map (\claims -> claims.sub)
  optionalTextField "requestId" requestIdText
    |> Array.append (optionalTextField "correlationId" correlationId)
    |> Array.append (optionalTextField "causationId" sourceEventId)
    |> Array.append (optionalTextField "relatedUserSub" relatedUserSub)
