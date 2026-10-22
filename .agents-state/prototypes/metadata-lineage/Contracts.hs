module Main (main) where

import Prelude (IO)
import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import DateTime (DateTime)
import DateTime qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Service.AccessControl (AccessError (..))
import Service.Auth (RequestContext (..))
import Service.Auth qualified as Auth
import Service.Command.Core (Command (..))
import Service.Event qualified as Event
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as Metadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.FileUpload.Core (FileRef)
import Service.FileUpload.Resolver (ResolvedFile)
import Stream qualified
import Task (Task)
import Task qualified
import Test
import Test.Hspec qualified as Hspec
import Test.Hspec.QuickCheck qualified as HspecQuickCheck
import Test.QuickCheck qualified as QuickCheck
import Test.Service.Command.Core (AuthenticatedAddItem)
import Text (Text)
import ToText (toText)
import Uuid (Uuid)
import Uuid qualified

data Lineage = Lineage
  { actor :: Maybe Text,
    correlation :: Text,
    cause :: Maybe Text
  }
  deriving (Eq, Show)

data ExtendedRequest = ExtendedRequest
  { user :: Maybe UserClaims,
    files :: Map FileRef ResolvedFile,
    requestId :: Uuid,
    timestamp :: DateTime,
    trustedBypass :: Bool,
    lineage :: Lineage
  }

data Execution = Execution
  { authorization :: RequestContext,
    lineage :: Lineage
  }

data Variant = ExtendRequest | SeparateEnvelope
  deriving (Eq, Show)

data Carried = InRequest ExtendedRequest | InEnvelope Execution

root :: Variant -> RequestContext -> Task Text Carried
root variant request = do
  requestId <- if request.requestId == Uuid.nil then Uuid.generate else Task.yield request.requestId
  let normalized = request {Auth.requestId = requestId}
  let actor = case request.user of
        Nothing -> Nothing
        Just claims -> Just claims.sub
  let lineage = Lineage {actor, correlation = Uuid.toText requestId, cause = Nothing}
  Task.yield (carry variant normalized lineage)

triggered :: Variant -> EventMetadata -> Task Text Carried
triggered variant source = do
  request <- Auth.trustedContext
  let lineage = fromEvent source
  Task.yield (carry variant request lineage)

fromEvent :: EventMetadata -> Lineage
fromEvent source = do
  let sourceId = Uuid.toText source.eventId
  let correlation = case source.correlationId of
        Just value -> value
        Nothing -> sourceId
  Lineage {actor = source.relatedUserSub, correlation, cause = Just sourceId}

carry :: Variant -> RequestContext -> Lineage -> Carried
carry variant request lineage =
  case variant of
    ExtendRequest -> InRequest ExtendedRequest
      { user = request.user,
        files = request.files,
        requestId = request.requestId,
        timestamp = request.timestamp,
        trustedBypass = request.trustedBypass,
        lineage
      }
    SeparateEnvelope -> InEnvelope Execution {authorization = request, lineage}

authorization :: Carried -> RequestContext
authorization carried =
  case carried of
    InRequest request -> RequestContext
      { user = request.user,
        files = request.files,
        requestId = request.requestId,
        timestamp = request.timestamp,
        trustedBypass = request.trustedBypass
      }
    InEnvelope execution -> execution.authorization

lineage :: Carried -> Lineage
lineage carried =
  case carried of
    InRequest request -> request.lineage
    InEnvelope execution -> execution.lineage

stamp :: Lineage -> EventMetadata -> EventMetadata
stamp context metadata = metadata
  { Metadata.relatedUserSub = context.actor,
    Metadata.correlationId = Just context.correlation,
    Metadata.causationId = context.cause
  }

claims :: UserClaims
claims = UserClaims {sub = "user-42", email = Nothing, name = Nothing, permissions = [], tenantId = Nothing, rawClaims = Map.empty}

newPayload :: Lineage -> Task Text (Event.InsertionPayload Text)
newPayload context = do
  payload <- Event.payloadFromEvents (EntityName "LineagePrototype") (StreamId.toStreamId Uuid.nil) ["First", "Second"]
  let insertions = payload.insertions |> Array.map (stampInsertion context)
  Task.yield payload {Event.insertions = insertions}

stampInsertion :: Lineage -> Event.Insertion event -> Event.Insertion event
stampInsertion context insertion = Event.Insertion
  { id = insertion.id,
    event = insertion.event,
    metadata = stamp context insertion.metadata
  }

variantSpec :: Variant -> Spec Unit
variantSpec variant = do
  it "root uses verified subject and request UUID, with no cause" \_ -> do
    request <- Auth.authenticatedContext claims
    carried <- root variant request
    lineage carried |> shouldBe Lineage {actor = Just "user-42", correlation = Uuid.toText request.requestId, cause = Nothing}
    let effective = authorization carried
    effective.requestId |> shouldBe request.requestId
    effective.timestamp |> shouldBe request.timestamp
    effective.trustedBypass |> shouldBe False

  it "nil root context is normalized once per invocation" \_ -> do
    first <- root variant Auth.emptyContext
    second <- root variant Auth.emptyContext
    (authorization first).requestId |> shouldNotBe Uuid.nil
    (lineage first).correlation |> shouldNotBe (lineage second).correlation
    (lineage first).actor |> shouldBe Nothing
    (lineage first).correlation |> shouldBe (Uuid.toText (authorization first).requestId)
    authenticated <- root variant (Auth.emptyContext {Auth.user = Just claims})
    (lineage authenticated).actor |> shouldBe (Just "user-42")

  it "event callback copies actor and correlation but advances cause" \_ -> do
    source <- Metadata.new
    let sourceWithLineage = source {Metadata.relatedUserSub = Just "user-42", Metadata.correlationId = Just "root-correlation", Metadata.causationId = Just "earlier-event"}
    carried <- triggered variant sourceWithLineage
    lineage carried |> shouldBe Lineage {actor = Just "user-42", correlation = "root-correlation", cause = Just (Uuid.toText source.eventId)}
    let request = authorization carried
    request.user |> shouldSatisfy (\value -> case value of
      Nothing -> True
      Just _ -> False)
    request.trustedBypass |> shouldBe True
    canExecuteImpl @AuthenticatedAddItem request.user |> shouldBe (Just Unauthenticated)

  it "historical source fallback joins fan-out and redelivery without rewriting history" \_ -> do
    source <- Metadata.new
    first <- triggered variant source
    second <- triggered variant source
    lineage first |> shouldBe Lineage {actor = Nothing, correlation = Uuid.toText source.eventId, cause = Just (Uuid.toText source.eventId)}
    lineage second |> shouldBe (lineage first)
    (authorization first).requestId |> shouldNotBe (authorization second).requestId
    source.correlationId |> shouldBe Nothing

  it "callback cause uses persisted metadata identity rather than insertion wrapper identity" \_ -> do
    payload <- Event.payloadFromEvents (EntityName "LineagePrototype") (StreamId.toStreamId Uuid.nil) ["Source" :: Text]
    case Array.first payload.insertions of
      Nothing -> Task.throw "Expected source insertion"
      Just insertion -> do
        carried <- triggered variant insertion.metadata
        (lineage carried).cause |> shouldBe (Just (Uuid.toText insertion.metadata.eventId))
        (lineage carried).cause |> shouldNotBe (Just (Uuid.toText insertion.id))

  it "present historical correlations remain opaque and unchanged" \_ -> do
    source <- Metadata.new
    ["", "  ", "legacy-trace-value", Uuid.toText Uuid.nil]
      |> Task.forEach (\correlation -> do
        carried <- triggered variant (source {Metadata.correlationId = Just correlation})
        (lineage carried).correlation |> shouldBe correlation
        (lineage carried).cause |> shouldBe (Just (Uuid.toText source.eventId)))

  it "autonomous invocations have separate correlations and no invented actor or cause" \_ -> do
    firstRequest <- Auth.trustedContext
    secondRequest <- Auth.trustedContext
    first <- root variant firstRequest
    second <- root variant secondRequest
    (lineage first).actor |> shouldBe Nothing
    (lineage first).cause |> shouldBe Nothing
    (lineage first).correlation |> shouldNotBe (lineage second).correlation

  it "multi-event payloads keep sibling lineage and preserve event identity and timestamp" \_ -> do
    request <- Auth.authenticatedContext claims
    carried <- root variant request
    raw <- Metadata.new
    let stamped = stamp (lineage carried) raw
    stamped.eventId |> shouldBe raw.eventId
    stamped.createdAt |> shouldBe raw.createdAt
    stamped.localPosition |> shouldBe raw.localPosition
    stamped.globalPosition |> shouldBe raw.globalPosition
    payload <- newPayload (lineage carried)
    let metadata = payload.insertions |> Array.map (\item -> item.metadata)
    Array.length metadata |> shouldBe 2
    Array.map (\item -> item.relatedUserSub) metadata |> shouldBe [Just "user-42", Just "user-42"]
    Array.map (\item -> item.correlationId) metadata |> shouldBe [Just (lineage carried).correlation, Just (lineage carried).correlation]
    Array.map (\item -> item.causationId) metadata |> shouldBe [Nothing, Nothing]
    Array.map (\item -> item.eventId) metadata |> shouldSatisfy (\ids -> Array.first ids != Array.last ids)

  it "rebuilding retry payloads preserves lineage while regenerating unpersisted event IDs" \_ -> do
    source <- Metadata.new
    carried <- triggered variant source
    first <- newPayload (lineage carried)
    second <- newPayload (lineage carried)
    let snapshots payload = payload.insertions |> Array.map (\item -> (item.metadata.relatedUserSub, item.metadata.correlationId, item.metadata.causationId))
    snapshots first |> shouldBe (snapshots second)
    let ids payload = payload.insertions |> Array.map (\item -> item.metadata.eventId)
    ids first |> shouldNotBe (ids second)

  it "real in-memory persistence retains proposed lineage" \_ -> do
    request <- Auth.authenticatedContext claims
    carried <- root variant request
    store <- InMemory.new |> Task.map (EventStore.castEventStore @Text) |> Task.mapError toText
    payload <- newPayload (lineage carried)
    store.insert payload |> Task.mapError toText |> Task.map (\_ -> unit)
    stream <- store.readAllStreamEvents payload.entityName payload.streamId |> Task.mapError toText
    messages <- Stream.toArray stream
    let events = EventStore.collectStreamEvents messages
    Array.length events |> shouldBe 2
    events |> Task.forEach (\event -> do
      event.metadata.relatedUserSub |> shouldBe (Just "user-42")
      event.metadata.correlationId |> shouldBe (Just (lineage carried).correlation))
    store.close |> Task.mapError toText

  HspecQuickCheck.prop "correlation and actor survive arbitrary callback depth while cause advances" do
    \(QuickCheck.Positive rawDepth) -> do
      let depth = 1 + modBy 40 (rawDepth :: Int)
      let build eventNumber context = do
            let sourceId = Uuid.generateV5 Uuid.nil (toText eventNumber)
            let request = Auth.emptyContext
            let child = carry variant request context
            let source = EventMetadata
                  { eventId = sourceId,
                    relatedUserSub = Nothing,
                    correlationId = Nothing,
                    causationId = Nothing,
                    createdAt = DateTime.fromEpochSeconds 0,
                    localPosition = Nothing,
                    globalPosition = Nothing
                  }
            source |> stamp (lineage child) |> fromEvent
      let initial = Lineage {actor = Just "user-42", correlation = "root-correlation", cause = Nothing}
      let final = Array.initialize depth identity |> Array.foldl build initial
      (final.actor, final.correlation, final.cause) == (Just "user-42", "root-correlation", Just (Uuid.toText (Uuid.generateV5 Uuid.nil (toText (depth - 1)))))

main :: IO Unit
main = Hspec.hspec do
  describe "A. Extended RequestContext representation" (variantSpec ExtendRequest)
  describe "B. Separate execution envelope representation" (variantSpec SeparateEnvelope)
