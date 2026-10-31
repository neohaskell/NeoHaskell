{-| Verify PostgreSQL key storage and private erasure through the service APIs.
Run 'spec' in the service test suite.
-}
module Service.EventStore.Postgres.ErasableKeysSpec (spec) where

import Core
import Array qualified
import AsyncTask qualified
import Bytes qualified
import Data.ByteString qualified as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Decider (Decision (..), DecisionContext (..))
import Decider qualified
import Erasable qualified
import Environment qualified
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Json qualified
import Result qualified
import Service.Event qualified as Event
import Service.AccessControl qualified as AccessControl
import Service.Event.EventMetadata (EventMetadata (..))
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.Postgres qualified as PostgresStore
import Service.EventStore.Postgres.Internal qualified as Postgres
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Service.QueryObjectStore.Postgres qualified as QueryStore
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.Query.Endpoint qualified as QueryEndpoint
import Service.Query.Pagination (QueryPageRequest (..))
import Task qualified
import Test
import Text qualified
import Uuid qualified
import Stream qualified
import Data.UUID qualified as UUID


data ProtectedRead = ProtectedRead
  { secret :: Erasable.Erasable Json.Value
  }
  deriving (Generic)


instance Json.ToJSON ProtectedRead
instance Json.FromJSON ProtectedRead


data StoredEnvelope = StoredEnvelope
  { reference :: Text,
    context :: Text,
    envelope :: Json.Value
  }


instance Json.FromJSON StoredEnvelope where
  parseJSON = Json.withObject "StoredEnvelope" \object -> do
    reference <- object Json..: "reference"
    context <- object Json..: "context"
    envelope <- object Json..: "envelope"
    Json.yield StoredEnvelope {reference, context, envelope}


instance Json.ToJSON StoredEnvelope where
  toJSON stored = Json.object
    [ ("$erasable", Json.toJSON (1 :: Int)),
      ("reference", Json.toJSON stored.reference),
      ("context", Json.toJSON stored.context),
      ("envelope", stored.envelope)
    ]


data CollisionPair = CollisionPair
  { literal :: StoredEnvelope,
    nested :: StoredEnvelope
  }


instance Json.FromJSON CollisionPair where
  parseJSON = Json.withObject "CollisionPair" \object -> do
    literal <- object Json..: "a/b"
    nestedObject <- object Json..: "a"
    nested <- nestedObject Json..: "b"
    Json.yield CollisionPair {literal, nested}


instance Query ProtectedRead where
  canAccessImpl _ = Nothing
  canViewImpl _ _ = Nothing


runReal :: Postgres.Connection -> Session.Session value -> Task Text value
runReal connection session =
  case connection of
    Postgres.MockConnection -> Task.throw "Expected PostgreSQL connection"
    Postgres.Connection pool -> do
      outcome <- HasqlPool.use pool session |> Task.fromIO
      case Result.fromEither outcome of
        Err err -> Task.throw (toText (show err))
        Ok value -> Task.yield value


config :: Text -> Int -> Postgres.PostgresEventStore
config databaseName databasePort =
  Postgres.PostgresEventStore
    { host = "127.0.0.1",
      databaseName,
      user = "neohaskell",
      password = "neohaskell",
      port = databasePort,
      poolSize = 2,
      sslMode = ConnectionConfig.SslModeUnset,
      sslRootCert = Nothing,
      erasableApplicationScope = "erasable-key-spec"
    }


queryConfig :: Text -> Int -> QueryStore.PostgresQueryObjectStoreConfig
queryConfig databaseName databasePort = QueryStore.PostgresQueryObjectStoreConfig
  { host = "127.0.0.1",
    databaseName,
    user = "neohaskell",
    password = "neohaskell",
    port = databasePort,
    poolSize = 2,
    sslMode = ConnectionConfig.SslModeUnset,
    sslRootCert = Nothing
  }


privateDatabase :: Task Text Text
privateDatabase = do
  databaseName <- Environment.getVariable "CRYPTOERASURE_TEST_DB"
  if Text.startsWith "nh_cryptoerasure_" databaseName
    then Task.yield databaseName
    else Task.throw "CRYPTOERASURE_TEST_DB must name a fresh nh_cryptoerasure_ database"


privatePort :: Task Text Int
privatePort = do
  configured <- Environment.getVariable "CRYPTOERASURE_TEST_DB_PORT" |> Task.asResult
  case configured of
    Err _ -> Task.yield 5432
    Ok value -> case Text.toInt value of
      Nothing -> Task.throw "Invalid CRYPTOERASURE_TEST_DB_PORT"
      Just port -> Task.yield port


commitProxyPort :: Task Text Int
commitProxyPort = do
  configured <- Environment.getVariable "CRYPTOERASURE_COMMIT_PROXY_PORT"
  case Text.toInt configured of
    Nothing -> Task.throw "Invalid CRYPTOERASURE_COMMIT_PROXY_PORT"
    Just port -> Task.yield port


spec :: Spec Unit
spec = do
  describe "Erasable key store" do
    whenEnvVar "CRYPTOERASURE_TEST_DB" do
      it "keeps a stale protected reference erased across concurrent pools and reconstruction" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        let privateQueryConfig = queryConfig databaseName databasePort
        writer <- PostgresStore.new privateConfig
        eraser <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        queryId <- Uuid.generate
        queryNameId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-race-fixture"
        let decision = do
              first <- Erasable.new (Json.toJSON ("private-one" :: Text))
              second <- Erasable.new (Json.toJSON ("private-two" :: Text))
              mapped <- Erasable.map identity first
              Accept Event.StreamCreation [(first, second, mapped)]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        (first, second, mapped) <- case outcome of
          AcceptCommand _ values -> case Array.first values of
            Just protected -> Task.yield protected
            Nothing -> Task.throw "Expected protected values"
          RejectCommand _ -> Task.throw "Expected protected values"
        Erasable.reference first |> shouldBe (Erasable.reference mapped)
        Erasable.reference first |> shouldNotBe (Erasable.reference second)
        let initial = Json.object
              [ ("first", Json.toJSON first),
                ("second", Json.toJSON second),
                ("mapped", Json.toJSON mapped),
                ("retained", Json.toJSON ("approved fact" :: Text))
              ]
        created <- Event.payloadFromEvents entityName streamId [initial]
        writer.insertWithErasableKeys keys (created {Event.insertionType = Event.StreamCreation}) |> Task.mapError toText |> discard
        staleMessages <- writer.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        stale <- case Array.first (EventStore.collectStreamEvents staleMessages) of
          Nothing -> Task.throw "Expected a stored event"
          Just event -> Task.yield event.event
        let queryName = [fmt|erasable-race-#{Uuid.toText queryNameId}|]
        queryWriter <- QueryStore.newFromConfig @ProtectedRead privateQueryConfig queryName |> Task.mapError toText
        queryWriter.atomicUpdate queryId (\_ -> Just (ProtectedRead {secret = second})) |> Task.mapError toText
        postEraseQueryId <- Uuid.generate
        queryWriter.atomicUpdate postEraseQueryId (\_ -> Just (ProtectedRead {secret = second})) |> Task.mapError toText
        let target = EventStore.ErasureTarget
              { reference = Erasable.reference first,
                entityName,
                streamId
              }
        staleAppend <- Event.payloadFromEvents entityName streamId [stale]
        eraseTask <- AsyncTask.run (eraser.eraseErasableKey target |> Task.mapError toText)
        appendTask <- AsyncTask.run (writer.insert (staleAppend {Event.insertionType = Event.AnyStreamState}) |> Task.mapError toText)
        queryTask <- AsyncTask.run (queryWriter.atomicUpdate queryId (\_ -> Just (ProtectedRead {secret = mapped})) |> Task.mapError toText)
        erasedNow <- AsyncTask.waitFor eraseTask
        erasedNow |> shouldBe EventStore.ErasedNow
        AsyncTask.waitFor appendTask |> discard
        AsyncTask.waitFor queryTask |> discard
        postEraseAppend <- Event.payloadFromEvents entityName streamId [stale]
        writer.insert (postEraseAppend {Event.insertionType = Event.AnyStreamState}) |> Task.mapError toText |> discard
        queryWriter.atomicUpdate postEraseQueryId (\_ -> Just (ProtectedRead {secret = mapped})) |> Task.mapError toText
        connection <- Postgres.defaultOps.acquire privateConfig
        tombstone <- runReal connection (keyRecord (Erasable.reference first))
        tombstone |> shouldBe (Just Nothing)
        raceQuery <- runReal connection (queryJsonText queryId)
        postEraseQuery <- runReal connection (queryJsonText postEraseQueryId)
        raceQuery |> shouldSatisfy (\value -> case value of
          Just stored -> Text.contains (Erasable.reference first) stored
            && not (Text.contains (Erasable.reference second) stored)
            && not (Text.contains "private-one" stored)
            && not (Text.contains "private-two" stored)
          Nothing -> False)
        postEraseQuery |> shouldSatisfy (\value -> case value of
          Just stored -> Text.contains (Erasable.reference first) stored
            && not (Text.contains (Erasable.reference second) stored)
            && not (Text.contains "private-one" stored)
            && not (Text.contains "private-two" stored)
          Nothing -> False)
        Postgres.defaultOps.release connection
        let renderQuery store query = QueryEndpoint.createQueryEndpointWithProtection
              (\value -> store.renderProtectedJson value |> Task.mapError (\_ -> AccessControl.StorageError "Protected response resolution failed"))
              query Nothing Nothing (QueryPageRequest {limit = 100, offset = 0})
        after <- renderQuery writer queryWriter |> Task.mapError toText
        after |> shouldSatisfy (Text.contains "Erased")
        after |> shouldSatisfy (\value -> not (Text.contains "Available" value))
        after |> shouldSatisfy (\value -> not (Text.contains "private-one" value))
        after |> shouldSatisfy (\value -> not (Text.contains "private-two" value))
        writer.close
        eraser.close
        reconnected <- PostgresStore.new privateConfig
        queryReader <- QueryStore.newFromConfig @ProtectedRead privateQueryConfig queryName |> Task.mapError toText
        afterReconnect <- renderQuery reconnected queryReader |> Task.mapError toText
        afterReconnect |> shouldSatisfy (Text.contains "Erased")
        afterReconnect |> shouldSatisfy (\value -> not (Text.contains "Available" value))
        afterReconnect |> shouldSatisfy (\value -> not (Text.contains "private-one" value))
        afterReconnect |> shouldSatisfy (\value -> not (Text.contains "private-two" value))
        replay <- reconnected.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        let events = EventStore.collectStreamEvents replay
        Array.length events |> shouldBe 3
        events |> Task.mapArray (\event -> do
          rendered <- reconnected.renderProtectedJson event.event |> Task.mapError toText
          let serialized = Json.encodeText rendered
          serialized |> shouldSatisfy (Text.contains "Erased")
          serialized |> shouldSatisfy (Text.contains "private-two")
          serialized |> shouldSatisfy (Text.contains "approved fact")
          serialized |> shouldSatisfy (\value -> not (Text.contains "private-one" value))) |> discard
        repeated <- reconnected.eraseErasableKey target |> Task.mapError toText
        repeated |> shouldBe EventStore.AlreadyErased
        reconnected.close

      it "erases only a matching application and stream, and survives a new pool" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        store <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        otherId <- Uuid.generate
        unknownId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "scoped-erasure-fixture"
        let decision = do
              first <- Erasable.new ("first" :: Text)
              second <- Erasable.new ("second" :: Text)
              Accept Event.StreamCreation [(first, second)]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        (first, second) <- case outcome of
          AcceptCommand _ values -> case Array.first values of
            Just pair -> Task.yield pair
            Nothing -> Task.throw "Expected protected values"
          RejectCommand _ -> Task.throw "Expected protected values"
        payload <- Event.payloadFromEvents entityName streamId [Json.toJSON (first, second)]
        store.insertWithErasableKeys keys (payload {Event.insertionType = Event.StreamCreation}) |> Task.mapError toText |> discard
        let target = EventStore.ErasureTarget
              { reference = Erasable.reference first,
                entityName,
                streamId
              }
        let unknown = target {EventStore.reference = Uuid.toText unknownId}
        unknownResult <- store.eraseErasableKey unknown |> Task.mapError toText
        unknownResult |> shouldBe EventStore.UnknownReference
        let wrongStream = target {EventStore.streamId = Event.StreamId (Uuid.toText otherId)}
        wrongStreamResult <- store.eraseErasableKey wrongStream |> Task.asResult
        wrongStreamResult |> shouldSatisfy (\result -> case result of Err _ -> True; Ok _ -> False)
        let wrongEntity = target {EventStore.entityName = Event.EntityName "other-entity"}
        wrongEntityResult <- store.eraseErasableKey wrongEntity |> Task.asResult
        wrongEntityResult |> shouldSatisfy (\result -> case result of Err _ -> True; Ok _ -> False)
        otherApplication <- PostgresStore.new (privateConfig {Postgres.erasableApplicationScope = "other-application"})
        wrongApplicationResult <- otherApplication.eraseErasableKey target |> Task.asResult
        wrongApplicationResult |> shouldSatisfy (\result -> case result of Err _ -> True; Ok _ -> False)
        otherApplication.close
        missingScope <- PostgresStore.new (privateConfig {Postgres.erasableApplicationScope = ""})
        missingScopeResult <- missingScope.eraseErasableKey target |> Task.asResult
        missingScopeResult |> shouldSatisfy (\result -> case result of Err _ -> True; Ok _ -> False)
        missingScope.close
        before <- store.renderProtectedJson (Json.toJSON first) |> Task.mapError toText
        Json.encodeText before |> shouldSatisfy (Text.contains "first")
        firstErasure <- store.eraseErasableKey target |> Task.mapError toText
        firstErasure |> shouldBe EventStore.ErasedNow
        repeatedErasure <- store.eraseErasableKey target |> Task.mapError toText
        repeatedErasure |> shouldBe EventStore.AlreadyErased
        store.close
        restarted <- PostgresStore.new privateConfig
        afterRestart <- restarted.eraseErasableKey target |> Task.mapError toText
        afterRestart |> shouldBe EventStore.AlreadyErased
        erased <- restarted.renderProtectedJson (Json.toJSON first) |> Task.mapError toText
        Json.encodeText erased |> shouldSatisfy (Text.contains "Erased")
        remaining <- restarted.renderProtectedJson (Json.toJSON second) |> Task.mapError toText
        Json.encodeText remaining |> shouldSatisfy (Text.contains "second")
        connection <- Postgres.defaultOps.acquire privateConfig
        runReal connection (corruptFixtureKey (Erasable.reference second))
        corruptResult <- restarted.eraseErasableKey (target {EventStore.reference = Erasable.reference second}) |> Task.asResult
        corruptResult |> shouldSatisfy (\result -> case result of Err _ -> True; Ok _ -> False)
        stillStored <- runReal connection (keyRecord (Erasable.reference second))
        stillStored |> shouldSatisfy (\value -> case value of Just (Just _) -> True; _ -> False)
        Postgres.defaultOps.release connection
        restarted.close

    whenEnvVar "CRYPTOERASURE_TEST_DB" do
      it "rejects ciphertext swapped between a slash key and nested keys" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        store <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-path-collision"
        let decision = do
              first <- Erasable.new ("first" :: Text)
              second <- Erasable.map (\_ -> "second" :: Text) first
              Accept Event.StreamCreation [(first, second)]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        (first, second) <- case outcome of
          RejectCommand _ -> Task.throw "Expected protected values"
          AcceptCommand _ values ->
            case Array.first values of
              Nothing -> Task.throw "Expected protected values"
              Just pair -> Task.yield pair
        Erasable.reference first |> shouldBe (Erasable.reference second)
        let original = Json.object
              [ ("a/b", Json.toJSON first),
                ("a", Json.object [("b", Json.toJSON second)])
              ]
        payload <- Event.payloadFromEvents entityName streamId [original]
        store.insertWithErasableKeys keys (payload {Event.insertionType = Event.StreamCreation}) |> Task.mapError toText |> discard
        messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        stored <- case Array.first (EventStore.collectStreamEvents messages) of
          Nothing -> Task.throw "Expected protected event"
          Just event -> Task.yield event.event
        pair <- case Json.decode @CollisionPair stored of
          Err reason -> Task.throw reason
          Ok value -> Task.yield value
        pair.literal.reference |> shouldBe pair.nested.reference
        pair.literal.context |> shouldNotBe pair.nested.context
        available <- store.renderProtectedJson stored |> Task.mapError toText
        Json.encodeText available |> shouldSatisfy (Text.contains "first")
        Json.encodeText available |> shouldSatisfy (Text.contains "second")
        let swapped = Json.object
              [ ("a/b", Json.toJSON (pair.literal {envelope = pair.nested.envelope})),
                ("a", Json.object [("b", Json.toJSON (pair.nested {envelope = pair.literal.envelope}))])
              ]
        swappedPayload <- Event.payloadFromEvents entityName streamId [swapped]
        result <- store.insert (swappedPayload {Event.insertionType = Event.AnyStreamState}) |> Task.asResult
        case result of
          Ok _ -> fail "Swapped envelopes authenticated at distinct JSON locations"
          Err _ -> pass
        corruptEnvelope <- case Json.parts pair.literal.envelope of
          Json.ObjectParts fields ->
            fields
              |> Array.map (\(key, value) ->
                if key == "tag"
                  then (key, Json.toJSON ("AAAAAAAAAAAAAAAAAAAAAA==" :: Text))
                  else (key, value))
              |> Array.toLinkedList
              |> Json.object
              |> Task.yield
          _ -> Task.throw "Expected persisted envelope object"
        let corrupt = Json.object
              [ ("a/b", Json.toJSON (pair.literal {envelope = corruptEnvelope})),
                ("a", Json.object [("b", Json.toJSON pair.nested)])
              ]
        corruptPayload <- Event.payloadFromEvents entityName streamId [corrupt]
        corruptResult <- store.insert (corruptPayload {Event.insertionType = Event.AnyStreamState}) |> Task.asResult
        case corruptResult of
          Ok _ -> fail "Corrupt active authentication tag was stored"
          Err _ -> pass
        after <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        Array.length (EventStore.collectStreamEvents after) |> shouldBe 1
        store.close

      it "distinguishes unknown, active, and erased across a new pool" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        connection <- Postgres.defaultOps.acquire privateConfig
        runReal connection createKeyTable
        firstId <- Uuid.generate
        secondId <- Uuid.generate
        let first = Uuid.toText firstId
        let second = Uuid.toText secondId
        firstKey <- Bytes.getRandom 32
        secondKey <- Bytes.getRandom 32
        unknown <- runReal connection (keyRecord first)
        case unknown of
          Nothing -> pass
          _ -> fail "Fresh reference must be unknown"
        runReal connection (insertFixtureKey first firstKey)
        runReal connection (insertFixtureKey second secondKey)
        Postgres.defaultOps.release connection
        restarted <- Postgres.defaultOps.acquire privateConfig
        runReal restarted (eraseFixtureKey first)
        erased <- runReal restarted (keyRecord first)
        case erased of
          Just Nothing -> pass
          _ -> fail "Erasure must leave a tombstone"
        runReal restarted (eraseFixtureKey first)
        repeated <- runReal restarted (keyRecord first)
        case repeated of
          Just Nothing -> pass
          _ -> fail "Repeated erasure must stay erased"
        resurrection <- runReal restarted (insertFixtureKey first firstKey) |> Task.asResult
        case resurrection of
          Ok _ -> fail "An erased reference must not accept a new key"
          Err _ -> pass
        remaining <- runReal restarted (keyRecord second)
        case remaining of
          Just (Just _) -> pass
          _ -> fail "Independent reference must remain active"
        Postgres.defaultOps.release restarted

      it "stores encrypted events atomically and resolves fresh key state after restart" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        let privateQueryConfig = queryConfig databaseName databasePort
        store <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-fixture"
        let marker = "Álvaro ñ 😀"
        let secret = Json.object [("name", Json.toJSON marker), ("nested", Json.toJSON ([Just ("café" :: Text), Nothing] :: [Maybe Text]))]
        let decision = do
              protected <- Erasable.new secret
              Accept Event.StreamCreation [protected]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        protected <- case outcome of
          RejectCommand _ -> Task.throw "Expected protected value"
          AcceptCommand _ values ->
            case Array.first values of
              Nothing -> Task.throw "Expected one protected value"
              Just value -> Task.yield value
        let eventJson = Json.object [("private", Json.toJSON protected)]
        payload <- Event.payloadFromEvents entityName streamId [eventJson]
        store.insertWithErasableKeys keys (payload {Event.insertionType = Event.StreamCreation}) |> Task.mapError toText
        repeated <- store.insertWithErasableKeys keys (payload {Event.insertionType = Event.StreamCreation}) |> Task.asResult
        repeated |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        let eventId = case Array.first payload.insertions of
              Just insertion -> insertion.metadata.eventId
              Nothing -> panic "Payload must have one insertion"
        equivalentRetry <- store.insert (payload {Event.insertionType = Event.AnyStreamState}) |> Task.asResult
        equivalentRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        conflictingPayload <- Event.payloadFromEvents entityName streamId
          [Json.object [("private", Json.toJSON protected), ("retained", Json.toJSON ("changed" :: Text))]]
        conflictingInsertions <- case Array.first conflictingPayload.insertions of
          Nothing -> Task.throw "Expected conflicting insertion"
          Just insertion -> do
            let repeatedMetadata = insertion.metadata {eventId = eventId}
            let repeated = Event.Insertion {id = insertion.id, event = insertion.event, metadata = repeatedMetadata}
            Task.yield (Array.fromLinkedList [repeated])
        conflict <- store.insert (conflictingPayload
          { Event.insertions = conflictingInsertions,
            Event.insertionType = Event.AnyStreamState
          }) |> Task.asResult
        conflict |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        let otherDecision = do
              other <- Erasable.new (Json.toJSON ("other secret" :: Text))
              Accept Event.AnyStreamState [other]
        (otherOutcome, otherKeys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") otherDecision
        other <- case otherOutcome of
          RejectCommand _ -> Task.throw "Expected another protected value"
          AcceptCommand _ values -> case Array.first values of
            Nothing -> Task.throw "Expected another protected value"
            Just value -> Task.yield value
        otherPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON other]
        otherInsertions <- case Array.first otherPayload.insertions of
          Nothing -> Task.throw "Expected another insertion"
          Just insertion -> do
            let repeatedMetadata = insertion.metadata {eventId = eventId}
            let repeated = Event.Insertion {id = insertion.id, event = insertion.event, metadata = repeatedMetadata}
            Task.yield (Array.fromLinkedList [repeated])
        otherRetry <- store.insertWithErasableKeys otherKeys (otherPayload
          { Event.insertions = otherInsertions,
            Event.insertionType = Event.AnyStreamState
          }) |> Task.asResult
        otherRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        strippedPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON ("stripped" :: Text)]
        strippedInsertions <- case Array.first strippedPayload.insertions of
          Nothing -> Task.throw "Expected stripped insertion"
          Just insertion -> do
            let repeatedMetadata = insertion.metadata {eventId = eventId}
            let repeated = Event.Insertion {id = insertion.id, event = insertion.event, metadata = repeatedMetadata}
            Task.yield (Array.fromLinkedList [repeated])
        strippedRetry <- store.insert (strippedPayload
          { Event.insertions = strippedInsertions,
            Event.insertionType = Event.AnyStreamState
          }) |> Task.asResult
        strippedRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        mixedPayload <- Event.payloadFromEvents entityName streamId
          [Json.toJSON ("stripped" :: Text), Json.toJSON ("new event" :: Text)]
        (mixedInsertions, newEventId) <- case (Array.get 0 mixedPayload.insertions, Array.get 1 mixedPayload.insertions) of
          (Just repeated, Just newEvent) -> do
            let repeatedMetadata = repeated.metadata {eventId = eventId}
            let collision = Event.Insertion {id = repeated.id, event = repeated.event, metadata = repeatedMetadata}
            Task.yield (Array.fromLinkedList [collision, newEvent], newEvent.metadata.eventId)
          _ -> Task.throw "Expected two mixed insertions"
        mixedRetry <- store.insert (mixedPayload
          { Event.insertions = mixedInsertions,
            Event.insertionType = Event.AnyStreamState
          }) |> Task.asResult
        mixedRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        recoveredPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON protected]
        store.insert (recoveredPayload {Event.insertionType = Event.AnyStreamState}) |> Task.mapError toText |> discard
        recovered <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        Array.length (EventStore.collectStreamEvents recovered) |> shouldBe 2
        connection <- Postgres.defaultOps.acquire privateConfig
        rawStored <- runReal connection (eventJsonText eventId)
        orphan <- runReal connection (keyRecord (Erasable.reference other))
        orphan |> shouldBe Nothing
        omitted <- runReal connection (eventJsonText newEventId)
        omitted |> shouldBe Nothing
        case rawStored of
          Nothing -> fail "Expected committed event"
          Just storedText -> storedText |> shouldSatisfy (\value -> not (Text.contains marker value) && not (Text.contains "changed" value))
        Postgres.defaultOps.release connection
        store.close
        restarted <- PostgresStore.new privateConfig
        messages <- restarted.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        let events = EventStore.collectStreamEvents messages
        storedEvent <- case Array.first events of
          Nothing -> Task.throw "Expected event after restart"
          Just value -> Task.yield value
        available <- restarted.renderProtectedJson storedEvent.event |> Task.mapError toText
        Json.encodeText available |> shouldSatisfy (Text.contains marker)
        queryName <- Uuid.generate
        queryStore <- QueryStore.newFromConfig @Json.Value privateQueryConfig [fmt|erasable-m1-fixture-#{Uuid.toText queryName}|] |> Task.mapError toText
        queryId <- Uuid.generate
        queryStore.atomicUpdate queryId (\_ -> Just storedEvent.event) |> Task.mapError toText
        queryConnection <- Postgres.defaultOps.acquire privateConfig
        queryStored <- runReal queryConnection (queryJsonText queryId)
        case queryStored of
          Nothing -> fail "Expected protected query row"
          Just queryText -> do
            queryText |> shouldSatisfy (\value -> not (Text.contains marker value))
            rawStored |> shouldSatisfy (\eventText -> eventText != Just queryText)
        Postgres.defaultOps.release queryConnection
        authorizedName <- Uuid.generate
        authorizedStore <- QueryStore.newFromConfig @ProtectedRead privateQueryConfig [fmt|erasable-m1-authorized-#{Uuid.toText authorizedName}|] |> Task.mapError toText
        authorizedId <- Uuid.generate
        authorizedStore.atomicUpdate authorizedId (\_ -> Just (ProtectedRead {secret = protected})) |> Task.mapError toText
        let readAuthorized = QueryEndpoint.createQueryEndpointWithProtection
              (\value -> restarted.renderProtectedJson value |> Task.mapError (\_ -> AccessControl.StorageError "Protected response resolution failed"))
              authorizedStore Nothing Nothing (QueryPageRequest {limit = 100, offset = 0})
        beforeErasure <- readAuthorized |> Task.mapError toText
        beforeErasure |> shouldSatisfy (Text.contains "Available")
        beforeErasure |> shouldSatisfy (Text.contains marker)
        keyConnection <- Postgres.defaultOps.acquire privateConfig
        runReal keyConnection (eraseFixtureKey (Erasable.reference protected))
        erased <- runReal keyConnection (keyRecord (Erasable.reference protected))
        case erased of
          Just Nothing -> pass
          _ -> fail "Expected erased tombstone"
        Postgres.defaultOps.release keyConnection
        unavailable <- restarted.renderProtectedJson storedEvent.event |> Task.mapError toText
        Json.encodeText unavailable |> shouldSatisfy (\value -> not (Text.contains marker value))
        Json.encodeText unavailable |> shouldSatisfy (Text.contains "Erased")
        afterErasure <- readAuthorized |> Task.mapError toText
        afterErasure |> shouldSatisfy (Text.contains "Erased")
        afterErasure |> shouldSatisfy (\value -> not (Text.contains marker value))
        restarted.close

      it "rolls back a new key when its event conflicts with the stream" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        store <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-conflict-fixture"
        firstPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON ("first" :: Text)]
        store.insert (firstPayload {Event.insertionType = Event.StreamCreation}) |> Task.mapError toText |> discard
        let decision = do
              protected <- Erasable.new ("private" :: Text)
              Accept Event.AnyStreamState [protected]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        protected <- case outcome of
          RejectCommand _ -> Task.throw "Expected protected value"
          AcceptCommand _ values ->
            case Array.first values of
              Nothing -> Task.throw "Expected protected value"
              Just value -> Task.yield value
        secondPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON protected]
        result <- store.insertWithErasableKeys keys (secondPayload {Event.insertionType = Event.InsertAfter (Event.StreamPosition (-1))}) |> Task.asResult
        case result of
          Ok _ -> fail "Conflicting event must fail"
          Err _ -> pass
        connection <- Postgres.defaultOps.acquire privateConfig
        state <- runReal connection (keyRecord (Erasable.reference protected))
        case state of
          Nothing -> pass
          _ -> fail "A failed event insert must leave no key"
        let secondEventId = case Array.first secondPayload.insertions of
              Just insertion -> insertion.metadata.eventId
              Nothing -> panic "Payload must have one insertion"
        stored <- runReal connection (eventJsonText secondEventId)
        stored |> shouldBe Nothing
        Postgres.defaultOps.release connection
        store.close

      it "rolls back keys when event insertion fails inside the same statement" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        store <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-statement-failure"
        let decision = do
              first <- Erasable.new ("first" :: Text)
              second <- Erasable.new ("second" :: Text)
              Accept Event.StreamCreation [first, second]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        values <- case outcome of
          RejectCommand _ -> Task.throw "Expected protected values"
          AcceptCommand _ accepted -> Task.yield accepted
        payload <- Event.payloadFromEvents entityName streamId (Array.map Json.toJSON values)
        badInsertions <- case (Array.get 0 payload.insertions, Array.get 1 payload.insertions) of
          (Just first, Just second) -> do
            let repeatedMetadata = second.metadata {eventId = first.metadata.eventId}
            let repeated = Event.Insertion {id = second.id, event = second.event, metadata = repeatedMetadata}
            Task.yield (Array.fromLinkedList [first, repeated])
          _ -> Task.throw "Expected two event insertions"
        result <- store.insertWithErasableKeys keys (payload {Event.insertions = badInsertions}) |> Task.asResult
        case result of
          Ok _ -> fail "Duplicate event IDs in one batch must fail"
          Err _ -> pass
        connection <- Postgres.defaultOps.acquire privateConfig
        values |> Task.mapArray (\value -> do
          state <- runReal connection (keyRecord (Erasable.reference value))
          state |> shouldBe Nothing)
          |> discard
        stored <- runReal connection (eventJsonText (case Array.first payload.insertions of
          Just insertion -> insertion.metadata.eventId
          Nothing -> panic "Payload must have insertions"))
        stored |> shouldBe Nothing
        Postgres.defaultOps.release connection
        store.close

      it "rolls back the key and event when a writer disconnects inside the atomic statement" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        store <- PostgresStore.new privateConfig
        gateOwner <- Postgres.defaultOps.acquire (privateConfig {Postgres.poolSize = 1})
        observer <- Postgres.defaultOps.acquire (privateConfig {Postgres.poolSize = 1})
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-disconnect"
        let decision = do
              protected <- Erasable.new ("disconnect secret" :: Text)
              Accept Event.AnyStreamState [protected]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        protected <- case outcome of
          RejectCommand _ -> Task.throw "Expected protected value"
          AcceptCommand _ values -> case Array.first values of
            Nothing -> Task.throw "Expected protected value"
            Just value -> Task.yield value
        payload <- Event.payloadFromEvents entityName streamId [Json.toJSON protected]
        let eventId = case Array.first payload.insertions of
              Nothing -> panic "Expected protected insertion"
              Just insertion -> insertion.metadata.eventId
        installInsertGate observer
        holdInsertGate gateOwner
        Task.finally
          (do
            releaseInsertGate gateOwner |> Task.asResult |> discard
            Postgres.defaultOps.release gateOwner
            removeInsertGate observer
            Postgres.defaultOps.release observer)
          do
            attempt <- AsyncTask.run (store.insertWithErasableKeys keys payload |> Task.asResult)
            waitForBlockedWriters observer 1 100
            backendPid <- blockedWriterPid observer
            terminated <- case backendPid of
              Nothing -> Task.throw "Expected blocked writer PID"
              Just pid -> terminateWriter observer pid
            terminated |> shouldBe True
            result <- AsyncTask.waitFor attempt
            case result of
              Err (EventStore.InsertionError (Event.InsertionFailed _)) -> pass
              _ -> fail "A disconnected SQL writer must return an insertion storage error"
            absentEvent <- runReal observer (eventJsonText eventId)
            absentKey <- runReal observer (keyRecord (Erasable.reference protected))
            absentEvent |> shouldBe Nothing
            absentKey |> shouldBe Nothing
        store.close
        reconnected <- PostgresStore.new privateConfig
        reconnected.insertWithErasableKeys keys payload |> Task.mapError toText |> discard
        recovered <- reconnected.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        Array.length (EventStore.collectStreamEvents recovered) |> shouldBe 1
        let target = EventStore.ErasureTarget
              { reference = Erasable.reference protected,
                entityName,
                streamId
              }
        reconnected.eraseErasableKey target |> Task.mapError toText |> discard
        repeated <- reconnected.insertWithErasableKeys keys payload |> Task.asResult
        repeated |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        verifyConnection <- Postgres.defaultOps.acquire privateConfig
        tombstone <- runReal verifyConnection (keyRecord (Erasable.reference protected))
        tombstone |> shouldBe (Just Nothing)
        Postgres.defaultOps.release verifyConnection
        reconnected.close

      whenEnvVar "CRYPTOERASURE_COMMIT_PROXY_PORT" do
        it "recovers a committed protected event after its database response is lost" \_ -> do
          databaseName <- privateDatabase
          databasePort <- privatePort
          proxyPort <- commitProxyPort
          let privateConfig = config databaseName databasePort
          let proxyConfig = config databaseName proxyPort
          writer <- PostgresStore.new proxyConfig
          observer <- Postgres.defaultOps.acquire privateConfig
          entityId <- Uuid.generate
          let streamId = Event.StreamId (Uuid.toText entityId)
          let entityName = Event.EntityName "erasable-uncertain-commit"
          let decision = do
                protected <- Erasable.new ("uncertain secret" :: Text)
                Accept Event.AnyStreamState [protected]
          (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
          protected <- case outcome of
            RejectCommand _ -> Task.throw "Expected protected value"
            AcceptCommand _ values -> case Array.first values of
              Nothing -> Task.throw "Expected protected value"
              Just value -> Task.yield value
          payload <- Event.payloadFromEvents entityName streamId [Json.toJSON protected]
          let eventId = case Array.first payload.insertions of
                Nothing -> panic "Expected protected insertion"
                Just insertion -> insertion.metadata.eventId
          uncertain <- writer.insertWithErasableKeys keys payload |> Task.asResult
          case uncertain of
            Err (EventStore.InsertionError (Event.InsertionFailed _)) -> pass
            _ -> fail "A lost commit response must leave the caller uncertain"
          durableEvent <- runReal observer (eventJsonText eventId)
          durableKey <- runReal observer (keyRecord (Erasable.reference protected))
          durableEvent |> shouldSatisfy (\value -> case value of
            Nothing -> False
            Just stored -> Text.contains (Erasable.reference protected) stored && not (Text.contains "uncertain secret" stored))
          durableKey |> shouldSatisfy (\value -> case value of Just (Just _) -> True; _ -> False)
          writer.close
          reconnected <- PostgresStore.new privateConfig
          exactRetry <- reconnected.insertWithErasableKeys keys payload |> Task.asResult
          exactRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
          conflicting <- Event.payloadFromEvents entityName streamId
            [Json.object [("protected", Json.toJSON protected), ("retained", Json.toJSON ("changed" :: Text))]]
          let conflictingInsertions = conflicting.insertions |> Array.map (\insertion -> do
                let repeatedMetadata = insertion.metadata {eventId = eventId}
                Event.Insertion {id = insertion.id, event = insertion.event, metadata = repeatedMetadata})
          conflictResult <- reconnected.insert (conflicting
            {Event.insertions = conflictingInsertions, Event.insertionType = Event.AnyStreamState}) |> Task.asResult
          conflictResult |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
          stripped <- Event.payloadFromEvents entityName streamId [Json.toJSON ("stripped" :: Text)]
          let strippedInsertions = stripped.insertions |> Array.map (\insertion -> do
                let repeatedMetadata = insertion.metadata {eventId = eventId}
                Event.Insertion {id = insertion.id, event = insertion.event, metadata = repeatedMetadata})
          strippedResult <- reconnected.insert (stripped
            {Event.insertions = strippedInsertions, Event.insertionType = Event.AnyStreamState}) |> Task.asResult
          strippedResult |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
          messages <- reconnected.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
          Array.length (EventStore.collectStreamEvents messages) |> shouldBe 1
          let target = EventStore.ErasureTarget
                { reference = Erasable.reference protected,
                  entityName,
                  streamId
                }
          reconnected.eraseErasableKey target |> Task.mapError toText |> discard
          afterErasure <- reconnected.insertWithErasableKeys keys payload |> Task.asResult
          afterErasure |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
          tombstone <- runReal observer (keyRecord (Erasable.reference protected))
          tombstone |> shouldBe (Just Nothing)
          reconnected.close
          Postgres.defaultOps.release observer

      it "keeps one protected event and key across concurrent pool retries and reconnect" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        firstStore <- PostgresStore.new privateConfig
        secondStore <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-duplicate-race"
        let firstDecision = do
              protected <- Erasable.new ("first race secret" :: Text)
              Accept Event.AnyStreamState [protected]
        (firstOutcome, firstKeys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") firstDecision
        firstProtected <- case firstOutcome of
          RejectCommand _ -> Task.throw "Expected protected value"
          AcceptCommand _ values -> case Array.first values of
            Nothing -> Task.throw "Expected protected value"
            Just value -> Task.yield value
        let secondDecision = do
              protected <- Erasable.new ("second race secret" :: Text)
              Accept Event.AnyStreamState [protected]
        (secondOutcome, secondKeys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") secondDecision
        secondProtected <- case secondOutcome of
          RejectCommand _ -> Task.throw "Expected second protected value"
          AcceptCommand _ values -> case Array.first values of
            Nothing -> Task.throw "Expected second protected value"
            Just value -> Task.yield value
        firstPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON firstProtected]
        secondPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON secondProtected]
        let eventId = case Array.first firstPayload.insertions of
              Nothing -> panic "Expected first insertion"
              Just insertion -> insertion.metadata.eventId
        let collision = secondPayload.insertions |> Array.map (\insertion -> do
              let repeatedMetadata = insertion.metadata {eventId = eventId}
              Event.Insertion {id = insertion.id, event = insertion.event, metadata = repeatedMetadata})
        let conflictingPayload = secondPayload {Event.insertions = collision}
        gateOwner <- Postgres.defaultOps.acquire (privateConfig {Postgres.poolSize = 1})
        observer <- Postgres.defaultOps.acquire (privateConfig {Postgres.poolSize = 1})
        installInsertGate observer
        holdInsertGate gateOwner
        winner <- Task.finally
          (do
            Postgres.defaultOps.release gateOwner
            removeInsertGate observer
            Postgres.defaultOps.release observer)
          do
            firstAttempt <- AsyncTask.run (firstStore.insertWithErasableKeys firstKeys firstPayload |> Task.asResult)
            secondAttempt <- AsyncTask.run (secondStore.insertWithErasableKeys secondKeys conflictingPayload |> Task.asResult)
            waitForBlockedWriters observer 2 100
            releaseInsertGate gateOwner
            firstResult <- AsyncTask.waitFor firstAttempt
            secondResult <- AsyncTask.waitFor secondAttempt
            case (firstResult, secondResult) of
              (Ok _, Err (EventStore.InsertionError (Event.InsertionFailed error))) -> do
                error |> shouldSatisfy (Text.contains "23505")
                Task.yield firstProtected
              (Err (EventStore.InsertionError (Event.InsertionFailed error)), Ok _) -> do
                error |> shouldSatisfy (Text.contains "23505")
                Task.yield secondProtected
              _ -> Task.throw "Exactly one overlapping SQL writer must commit"
        firstStore.close
        secondStore.close
        reconnected <- PostgresStore.new privateConfig
        firstRetry <- reconnected.insertWithErasableKeys firstKeys firstPayload |> Task.asResult
        secondRetry <- reconnected.insertWithErasableKeys secondKeys conflictingPayload |> Task.asResult
        firstRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        secondRetry |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        messages <- reconnected.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        Array.length (EventStore.collectStreamEvents messages) |> shouldBe 1
        connection <- Postgres.defaultOps.acquire privateConfig
        winnerKey <- runReal connection (keyRecord (Erasable.reference winner))
        winnerKey |> shouldSatisfy (\value -> case value of Just (Just _) -> True; _ -> False)
        let loser = if Erasable.reference winner == Erasable.reference firstProtected then secondProtected else firstProtected
        loserKey <- runReal connection (keyRecord (Erasable.reference loser))
        loserKey |> shouldBe Nothing
        storedEvent <- runReal connection (eventJsonText eventId)
        storedEvent |> shouldSatisfy (\value -> case value of Just text -> Text.contains (Erasable.reference winner) text; Nothing -> False)
        Postgres.defaultOps.release connection
        let target = EventStore.ErasureTarget
              { reference = Erasable.reference winner,
                entityName,
                streamId
              }
        reconnected.eraseErasableKey target |> Task.mapError toText |> discard
        firstAfterErasure <- reconnected.insertWithErasableKeys firstKeys firstPayload |> Task.asResult
        secondAfterErasure <- reconnected.insertWithErasableKeys secondKeys conflictingPayload |> Task.asResult
        firstAfterErasure |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        secondAfterErasure |> shouldBe (Err (EventStore.InsertionError Event.ConsistencyCheckFailed))
        erased <- reconnected.renderProtectedJson (Json.toJSON winner) |> Task.mapError toText
        Json.encodeText erased |> shouldSatisfy (Text.contains "Erased")
        finalConnection <- Postgres.defaultOps.acquire privateConfig
        tombstone <- runReal finalConnection (keyRecord (Erasable.reference winner))
        tombstone |> shouldBe (Just Nothing)
        remainingLoser <- runReal finalConnection (keyRecord (Erasable.reference loser))
        remainingLoser |> shouldBe Nothing
        Postgres.defaultOps.release finalConnection
        reconnected.close

      it "does not insert an event when its key write fails" \_ -> do
        databaseName <- privateDatabase
        databasePort <- privatePort
        let privateConfig = config databaseName databasePort
        store <- PostgresStore.new privateConfig
        entityId <- Uuid.generate
        let streamId = Event.StreamId (Uuid.toText entityId)
        let entityName = Event.EntityName "erasable-key-failure"
        let decision = do
              protected <- Erasable.new ("private" :: Text)
              Accept Event.AnyStreamState [protected]
        (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
        protected <- case outcome of
          RejectCommand _ -> Task.throw "Expected protected value"
          AcceptCommand _ values -> case Array.first values of
            Nothing -> Task.throw "Expected protected value"
            Just value -> Task.yield value
        firstPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON protected]
        store.insertWithErasableKeys keys firstPayload |> Task.mapError toText |> discard
        secondPayload <- Event.payloadFromEvents entityName streamId [Json.toJSON protected]
        failed <- store.insertWithErasableKeys keys secondPayload |> Task.asResult
        case failed of
          Ok _ -> fail "A duplicate key write must reject the new event"
          Err _ -> pass
        let secondEventId = case Array.first secondPayload.insertions of
              Just insertion -> insertion.metadata.eventId
              Nothing -> panic "Expected a second insertion"
        connection <- Postgres.defaultOps.acquire privateConfig
        absent <- runReal connection (eventJsonText secondEventId)
        absent |> shouldBe Nothing
        storedKey <- runReal connection (keyRecord (Erasable.reference protected))
        storedKey |> shouldSatisfy (\value -> case value of Just (Just _) -> True; _ -> False)
        Postgres.defaultOps.release connection
        messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
        Array.length (EventStore.collectStreamEvents messages) |> shouldBe 1
        store.close


eventJsonText :: Uuid -> Session.Session (Maybe Text)
eventJsonText eventId = do
  let statement :: Statement UUID.UUID (Maybe Text) =
        Statement
          "SELECT eventData::text FROM Events WHERE eventId = $1"
          (Encoders.param (Encoders.nonNullable Encoders.uuid))
          (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.text)))
          True
  Session.statement (Uuid.toLegacy eventId) statement


installInsertGate :: Postgres.Connection -> Task Text Unit
installInsertGate connection =
  runReal connection do
    Session.sql "CREATE FUNCTION cryptoerasure_insert_gate() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_advisory_xact_lock(782391); RETURN NEW; END $$"
    Session.sql "CREATE TRIGGER cryptoerasure_insert_gate BEFORE INSERT ON Events FOR EACH ROW EXECUTE FUNCTION cryptoerasure_insert_gate()"


removeInsertGate :: Postgres.Connection -> Task Text Unit
removeInsertGate connection =
  runReal connection do
    Session.sql "DROP TRIGGER IF EXISTS cryptoerasure_insert_gate ON Events"
    Session.sql "DROP FUNCTION IF EXISTS cryptoerasure_insert_gate()"


holdInsertGate :: Postgres.Connection -> Task Text Unit
holdInsertGate connection = runReal connection (Session.sql "DO $$ BEGIN PERFORM pg_advisory_lock(782391); END $$")


releaseInsertGate :: Postgres.Connection -> Task Text Unit
releaseInsertGate connection = runReal connection (Session.sql "DO $$ BEGIN PERFORM pg_advisory_unlock(782391); END $$")


blockedWriterCount :: Postgres.Connection -> Task Text Int64
blockedWriterCount connection =
  runReal connection do
    let statement :: Statement () Int64 = Statement
          "SELECT count(*)::bigint FROM pg_stat_activity WHERE datname = current_database() AND wait_event = 'advisory' AND query LIKE '%inserted_keys%'"
          Encoders.noParams
          (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
          True
    Session.statement () statement


blockedWriterPid :: Postgres.Connection -> Task Text (Maybe Int64)
blockedWriterPid connection =
  runReal connection do
    let statement :: Statement () (Maybe Int64) = Statement
          "SELECT pid::bigint FROM pg_stat_activity WHERE datname = current_database() AND wait_event = 'advisory' AND query LIKE '%inserted_keys%' LIMIT 1"
          Encoders.noParams
          (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.int8)))
          True
    Session.statement () statement


waitForBlockedWriters :: Postgres.Connection -> Int64 -> Int -> Task Text Unit
waitForBlockedWriters connection expected attempts = do
  observed <- blockedWriterCount connection
  Task.unless (observed >= expected) do
    if attempts <= 0
      then Task.throw [fmt|Expected #{expected} SQL writers at the database gate, observed #{observed}|]
      else do
        AsyncTask.sleep 20
        waitForBlockedWriters connection expected (attempts - 1)


terminateWriter :: Postgres.Connection -> Int64 -> Task Text Bool
terminateWriter connection backendPid =
  runReal connection do
    let statement :: Statement Int64 Bool = Statement
          "SELECT pg_terminate_backend(($1::bigint)::integer)"
          (Encoders.param (Encoders.nonNullable Encoders.int8))
          (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          True
    Session.statement backendPid statement


createKeyTable :: Session.Session ()
createKeyTable =
  Session.sql "CREATE TABLE IF NOT EXISTS erasable_keys (reference TEXT PRIMARY KEY, key_bytes BYTEA NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now())"


insertFixtureKey :: Text -> Bytes.Bytes -> Session.Session ()
insertFixtureKey reference key = do
  let statement :: Statement (Text, ByteString.ByteString) () =
        Statement
          "INSERT INTO erasable_keys (reference, key_bytes) VALUES ($1, $2)"
          (((\(ref, _) -> ref) >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> ((\(_, bytes) -> bytes) >$< Encoders.param (Encoders.nonNullable Encoders.bytea)))
          Decoders.noResult
          True
  Session.statement (reference, Bytes.unwrap key) statement


keyRecord :: Text -> Session.Session (Maybe (Maybe ByteString.ByteString))
keyRecord reference = do
  let statement :: Statement Text (Maybe (Maybe ByteString.ByteString)) =
        Statement
          "SELECT key_bytes FROM erasable_keys WHERE reference = $1"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          (Decoders.rowMaybe (Decoders.column (Decoders.nullable Decoders.bytea)))
          True
  Session.statement reference statement


corruptFixtureKey :: Text -> Session.Session ()
corruptFixtureKey reference = do
  let statement :: Statement Text () =
        Statement
          "UPDATE erasable_keys SET key_bytes = decode('00', 'hex') WHERE reference = $1"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          Decoders.noResult
          True
  Session.statement reference statement


eraseFixtureKey :: Text -> Session.Session ()
eraseFixtureKey reference = do
  let statement :: Statement Text () =
        Statement
          "UPDATE erasable_keys SET key_bytes = NULL WHERE reference = $1 AND key_bytes IS NOT NULL"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          Decoders.noResult
          True
  Session.statement reference statement


queryJsonText :: Uuid -> Session.Session (Maybe Text)
queryJsonText queryId = do
  let statement :: Statement UUID.UUID (Maybe Text) =
        Statement
          "SELECT state_json::text FROM query_object_store WHERE instance_uuid = $1"
          (Encoders.param (Encoders.nonNullable Encoders.uuid))
          (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.text)))
          True
  Session.statement (Uuid.toLegacy queryId) statement
