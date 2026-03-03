module Service.EventStore.Postgres.NotificationsSpec (spec) where

import Core

import Result qualified
import Text qualified
import Json qualified
import Service.Event (EntityName (..), Event (..), StreamId (..), StreamPosition (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore.Postgres.Notifications (mkReconnectConfig, ReconnectConfig (..))
import Service.EventStore.Postgres.Notifications qualified as Notifications
import Service.EventStore.Postgres.PostgresEventRecord (PostgresEventRecord (..))
import Service.EventStore.Postgres.Sessions (EventNotificationPayload (..))
import Service.EventStore.Postgres.Sessions qualified as Sessions

import Test


spec :: Spec Unit
spec = do
  describe "Notification Payload Flow" do
    describe "EventNotificationPayload JSON decoding" do
      it "decodes a valid lightweight payload with all 5 fields" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Expected successful decode, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 42
            notification.localPosition |> shouldBe 5
            notification.inlinedStreamId |> shouldBe "cart-123"
            notification.entity |> shouldBe "CartEntity"

      it "fails to decode when globalPosition is missing" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "fails to decode when entity is missing" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "fails to decode when eventId is missing" \_ -> do
        let payload =
              "{\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "fails to decode when globalPosition is a string instead of number" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":\"forty-two\",\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "ignores extra fields in payload (forward compatibility)" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\",\"extraField\":\"ignored\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Expected successful decode with extra fields, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 42
            notification.entity |> shouldBe "CartEntity"

      it "decodes zero positions correctly (boundary case)" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":0,\"localPosition\":0,\"inlinedStreamId\":\"cart-first\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Expected successful decode, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 0
            notification.localPosition |> shouldBe 0

    describe "postgresRecordToEvent conversion" do
      it "converts a valid record to an Event with correct positions" \_ -> do
        metadata <- EventMetadata.new
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 100,
                  localPosition = 7,
                  inlinedStreamId = "cart-456",
                  entityName = "CartEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode metadata
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.entityName |> shouldBe (EntityName "CartEntity")
            event.streamId |> shouldBe (StreamId "cart-456")
            event.metadata.globalPosition |> shouldBe (Just (StreamPosition 100))
            event.metadata.localPosition |> shouldBe (Just (StreamPosition 7))

      it "preserves event data through conversion" \_ -> do
        metadata <- EventMetadata.new
        let testData = Json.object ["itemId" Json..= ("abc" :: Text), "amount" Json..= (42 :: Int)]
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 0,
                  localPosition = 0,
                  inlinedStreamId = "cart-789",
                  entityName = "CartEntity",
                  eventData = testData,
                  metadata = Json.encode metadata
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.event |> shouldBe testData

      it "fails when metadata JSON is not a valid EventMetadata object" \_ -> do
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 0,
                  localPosition = 0,
                  inlinedStreamId = "cart-000",
                  entityName = "CartEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode ("not-valid-metadata" :: Text)
                }
        Sessions.postgresRecordToEvent record |> shouldSatisfy Result.isErr

      it "sets positions from record columns, not from metadata JSON" \_ -> do
        -- The record's position columns should override whatever is in the metadata JSON.
        -- This ensures positions come from the authoritative source (DB columns).
        metadata <- EventMetadata.new
        let metadataWithPositions =
              metadata
                { EventMetadata.globalPosition = Just (StreamPosition 999),
                  EventMetadata.localPosition = Just (StreamPosition 888)
                }
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 42,
                  localPosition = 7,
                  inlinedStreamId = "cart-override",
                  entityName = "CartEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode metadataWithPositions
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.metadata.globalPosition |> shouldBe (Just (StreamPosition 42))
            event.metadata.localPosition |> shouldBe (Just (StreamPosition 7))

      it "extracts entityName and streamId from record columns" \_ -> do
        metadata <- EventMetadata.new
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 0,
                  localPosition = 0,
                  inlinedStreamId = "order-abc-123",
                  entityName = "OrderEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode metadata
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.entityName |> shouldBe (EntityName "OrderEntity")
            event.streamId |> shouldBe (StreamId "order-abc-123")

    describe "Notification payload design properties" do
      it "lightweight payload is well under PostgreSQL 8000-byte NOTIFY limit" \_ -> do
        -- The whole point of the fix: notification payload must be lightweight.
        -- Even with long identifiers, the payload should be well under 8000 bytes.
        -- inlinedStreamId is VARCHAR(4000), entity is VARCHAR(255).
        let longStreamId = Text.repeat 200 "stream-id-"
        let longEntity = Text.repeat 25 "EntityName"
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":9999999999,\"localPosition\":9999999999,\"inlinedStreamId\":\""
              ++ longStreamId
              ++ "\",\"entity\":\""
              ++ longEntity
              ++ "\"}"
        let payloadLength = Text.length payload
        payloadLength |> shouldSatisfy (\len -> len < 8000)
        -- And it should still decode correctly
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isOk

      it "global and stream-specific triggers produce identical payload shape" \_ -> do
        -- Both pg_notify calls in the trigger use the same json_build_object structure.
        -- This test documents that the payload shape is consistent across channels.
        -- The trigger sends: eventId, globalPosition, localPosition, inlinedStreamId, entity
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":1,\"localPosition\":0,\"inlinedStreamId\":\"my-stream\",\"entity\":\"MyEntity\"}"
        -- Both channels produce the same JSON structure — one decode type handles both
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Payload shape should decode for both channels, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 1
            notification.localPosition |> shouldBe 0
            notification.inlinedStreamId |> shouldBe "my-stream"
            notification.entity |> shouldBe "MyEntity"

  describe "ReconnectConfig" do
    describe "mkReconnectConfig" do
      -- Valid configs
      it "accepts valid config: initial=500, max=30000, multiplier=2.0" \_ -> do
        mkReconnectConfig 500 30000 2.0 |> shouldSatisfy Result.isOk

      it "accepts minimum valid config: initial=1, max=1, multiplier=1.001" \_ -> do
        mkReconnectConfig 1 1 1.001 |> shouldSatisfy Result.isOk

      it "accepts boundary: initial equals max" \_ -> do
        mkReconnectConfig 250 250 1.0001 |> shouldSatisfy Result.isOk

      -- Invalid configs
      it "rejects initialBackoff=0" \_ -> do
        mkReconnectConfig 0 1000 2.0 |> shouldSatisfy Result.isErr

      it "rejects negative initialBackoff" \_ -> do
        mkReconnectConfig (-1) 1000 2.0 |> shouldSatisfy Result.isErr

      it "rejects maxBackoff < initialBackoff" \_ -> do
        mkReconnectConfig 1000 500 2.0 |> shouldSatisfy Result.isErr

      it "rejects backoffMultiplier=1.0 (no growth)" \_ -> do
        mkReconnectConfig 500 30000 1.0 |> shouldSatisfy Result.isErr

      it "rejects backoffMultiplier < 1.0 (shrinking backoff)" \_ -> do
        mkReconnectConfig 500 30000 0.5 |> shouldSatisfy Result.isErr

      -- Field preservation
      it "preserves field values in valid config" \_ -> do
        let result = mkReconnectConfig 100 5000 1.5
        case result of
          Err err -> Test.fail [fmt|Expected Ok, got: #{err}|]
          Ok cfg -> do
            cfg.initialBackoff |> shouldBe 100
            cfg.maxBackoff |> shouldBe 5000
            cfg.backoffMultiplier |> shouldBe 1.5
