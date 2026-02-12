module Service.Internal.LogSpec where

import Core
import Data.Aeson qualified as Aeson
import Data.Either (Either (..))
import DateTime qualified
import Service.Internal.Log.Events (LogLevel (..), LogRecorded (..))
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "LogLevel" do
    describe "JSON serialization" do
      it "serializes Debug to \"debug\"" \_ -> do
        let json = Aeson.encode Debug
        json |> shouldBe (Aeson.encode ("debug" :: Text))

      it "serializes Info to \"info\"" \_ -> do
        let json = Aeson.encode Info
        json |> shouldBe (Aeson.encode ("info" :: Text))

      it "serializes Warn to \"warn\"" \_ -> do
        let json = Aeson.encode Warn
        json |> shouldBe (Aeson.encode ("warn" :: Text))

      it "serializes Error to \"error\"" \_ -> do
        let json = Aeson.encode Error
        json |> shouldBe (Aeson.encode ("error" :: Text))

      it "deserializes \"debug\" to Debug" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"debug\"" of
          Right level -> level |> shouldBe Debug
          Left _ -> True |> shouldBe False

      it "deserializes \"info\" to Info" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"info\"" of
          Right level -> level |> shouldBe Info
          Left _ -> True |> shouldBe False

      it "deserializes \"warn\" to Warn" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"warn\"" of
          Right level -> level |> shouldBe Warn
          Left _ -> True |> shouldBe False

      it "deserializes \"error\" to Error" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"error\"" of
          Right level -> level |> shouldBe Error
          Left _ -> True |> shouldBe False

      it "fails to parse unknown values" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"unknown\"" of
          Right _ -> True |> shouldBe False
          Left _ -> True |> shouldBe True

      it "fails to parse uppercase values" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"DEBUG\"" of
          Right _ -> True |> shouldBe False
          Left _ -> True |> shouldBe True

      it "fails to parse empty string" \_ -> do
        case Aeson.eitherDecodeStrict @LogLevel "\"\"" of
          Right _ -> True |> shouldBe False
          Left _ -> True |> shouldBe True

      it "round-trips Debug through JSON" \_ -> do
        let result = Aeson.eitherDecode @LogLevel (Aeson.encode Debug)
        result |> shouldBe (Right Debug)

      it "round-trips Info through JSON" \_ -> do
        let result = Aeson.eitherDecode @LogLevel (Aeson.encode Info)
        result |> shouldBe (Right Info)

      it "round-trips Warn through JSON" \_ -> do
        let result = Aeson.eitherDecode @LogLevel (Aeson.encode Warn)
        result |> shouldBe (Right Warn)

      it "round-trips Error through JSON" \_ -> do
        let result = Aeson.eitherDecode @LogLevel (Aeson.encode Error)
        result |> shouldBe (Right Error)

    describe "Ord instance" do
      it "Debug < Info" \_ -> do
        (Debug < Info) |> shouldBe True

      it "Info < Warn" \_ -> do
        (Info < Warn) |> shouldBe True

      it "Warn < Error" \_ -> do
        (Warn < Error) |> shouldBe True

      it "Debug is the minimum level" \_ -> do
        (Debug < Error) |> shouldBe True

    describe "Show instance" do
      it "shows Debug" \_ -> do
        toText Debug |> shouldBe "Debug"

      it "shows Info" \_ -> do
        toText Info |> shouldBe "Info"

      it "shows Warn" \_ -> do
        toText Warn |> shouldBe "Warn"

      it "shows Error" \_ -> do
        toText Error |> shouldBe "Error"

  describe "LogRecorded" do
    describe "JSON serialization" do
      it "round-trips through JSON" \_ -> do
        let testUuid = Uuid.nil
        let testTimestamp = DateTime.fromEpochSeconds 1700000000
        let record = LogRecorded
              { logId = testUuid
              , timestamp = testTimestamp
              , level = Info
              , message = "test message"
              }
        let result = Aeson.eitherDecode @LogRecorded (Aeson.encode record)
        result |> shouldBe (Right record)

      it "JSON contains expected field names" \_ -> do
        let testUuid = Uuid.nil
        let testTimestamp = DateTime.fromEpochSeconds 1700000000
        let record = LogRecorded
              { logId = testUuid
              , timestamp = testTimestamp
              , level = Warn
              , message = "warning message"
              }
        let jsonStr = Aeson.encode record |> toText
        jsonStr |> shouldSatisfy (\text -> Text.contains "logId" text)
        jsonStr |> shouldSatisfy (\text -> Text.contains "timestamp" text)
        jsonStr |> shouldSatisfy (\text -> Text.contains "level" text)
        jsonStr |> shouldSatisfy (\text -> Text.contains "message" text)
