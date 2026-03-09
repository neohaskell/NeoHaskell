module Integration.Audio.Transcribe.InternalSpec (spec) where

import Basics
import Integration qualified
import Integration.Audio.Transcribe (Config (..))
import Integration.Audio.Transcribe qualified as AudioTranscribe
import Integration.Audio.Transcribe.Internal
import Json qualified
import Maybe (Maybe (..))
import Result (Result)
import Result qualified
import Service.FileUpload.Core (FileAccessError (..), FileRef (..))
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do
  describe "buildSystemPrompt" do
    it "produces base transcription prompt with default config" do
      let prompt = buildSystemPrompt AudioTranscribe.defaultConfig
      Text.contains "Transcribe the spoken content" prompt `shouldBe` True

    it "base prompt includes faithfulness instruction" do
      let prompt = buildSystemPrompt AudioTranscribe.defaultConfig
      Text.contains "faithfully and completely" prompt `shouldBe` True

    it "base prompt includes output-only instruction" do
      let prompt = buildSystemPrompt AudioTranscribe.defaultConfig
      Text.contains "Output only the transcribed text" prompt `shouldBe` True

    it "appends language hint when language is Just \"es\"" do
      let config = AudioTranscribe.defaultConfig {language = Just "es"}
      let prompt = buildSystemPrompt config
      Text.contains "es" prompt `shouldBe` True

    it "appends language hint when language is Just \"en\"" do
      let config = AudioTranscribe.defaultConfig {language = Just "en"}
      let prompt = buildSystemPrompt config
      Text.contains "en" prompt `shouldBe` True

    it "language hint includes \"Respond in the same language\"" do
      let config = AudioTranscribe.defaultConfig {language = Just "ja"}
      let prompt = buildSystemPrompt config
      Text.contains "Respond in the same language" prompt `shouldBe` True

    it "no language hint when language is Nothing" do
      let prompt = buildSystemPrompt AudioTranscribe.defaultConfig
      Text.contains "Respond in the same language" prompt `shouldBe` False

    it "appends duration hint when maxDurationSeconds is Just 300" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 300}
      let prompt = buildSystemPrompt config
      Text.contains "300" prompt `shouldBe` True

    it "duration hint includes \"seconds of audio\"" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 300}
      let prompt = buildSystemPrompt config
      Text.contains "seconds of audio" prompt `shouldBe` True

    it "ignores maxDurationSeconds when Just 0" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 0}
      let prompt = buildSystemPrompt config
      Text.contains "seconds" prompt `shouldBe` False

    it "ignores maxDurationSeconds when Just (-5)" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just (-5)}
      let prompt = buildSystemPrompt config
      Text.contains "seconds" prompt `shouldBe` False

    it "ignores maxDurationSeconds when Just (-1)" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just (-1)}
      let prompt = buildSystemPrompt config
      Text.contains "seconds" prompt `shouldBe` False

    it "no duration hint when maxDurationSeconds is Nothing" do
      let prompt = buildSystemPrompt AudioTranscribe.defaultConfig
      Text.contains "seconds of audio" prompt `shouldBe` False

    it "uses custom systemPrompt when provided" do
      let config = AudioTranscribe.defaultConfig {systemPrompt = Just "Custom prompt"}
      let prompt = buildSystemPrompt config
      prompt `shouldBe` "Custom prompt"

    it "custom systemPrompt ignores language hint" do
      let config = AudioTranscribe.defaultConfig {systemPrompt = Just "Custom", language = Just "es"}
      let prompt = buildSystemPrompt config
      prompt `shouldBe` "Custom"

    it "custom systemPrompt ignores duration hint" do
      let config = AudioTranscribe.defaultConfig {systemPrompt = Just "Custom", maxDurationSeconds = Just 300}
      let prompt = buildSystemPrompt config
      prompt `shouldBe` "Custom"

    it "empty custom systemPrompt is used as-is" do
      let config = AudioTranscribe.defaultConfig {systemPrompt = Just ""}
      let prompt = buildSystemPrompt config
      prompt `shouldBe` ""

    it "combines language and duration hints" do
      let config = AudioTranscribe.defaultConfig {language = Just "es", maxDurationSeconds = Just 600}
      let prompt = buildSystemPrompt config
      Text.contains "es" prompt `shouldBe` True
      Text.contains "600" prompt `shouldBe` True

    it "combined hints: language + non-positive duration only includes language" do
      let config = AudioTranscribe.defaultConfig {language = Just "de", maxDurationSeconds = Just 0}
      let prompt = buildSystemPrompt config
      Text.contains "de" prompt `shouldBe` True
      Text.contains "seconds" prompt `shouldBe` False

    it "maxDurationSeconds = Just 1 (minimum positive) is included" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 1}
      let prompt = buildSystemPrompt config
      Text.contains "seconds of audio" prompt `shouldBe` True

    it "maxDurationSeconds = Just 999999 (very large) is included" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 999999}
      let prompt = buildSystemPrompt config
      Text.contains "999999" prompt `shouldBe` True

  describe "buildTranscriptionPrompt" do
    it "produces transcription prompt with default config" do
      let prompt = buildTranscriptionPrompt AudioTranscribe.defaultConfig
      prompt `shouldBe` "Please transcribe the spoken content from this audio file."

    it "produces same prompt regardless of language override" do
      let config = AudioTranscribe.defaultConfig {language = Just "es"}
      let prompt = buildTranscriptionPrompt config
      prompt `shouldBe` "Please transcribe the spoken content from this audio file."

    it "produces same prompt regardless of maxDurationSeconds override" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 300}
      let prompt = buildTranscriptionPrompt config
      prompt `shouldBe` "Please transcribe the spoken content from this audio file."

    it "produces same prompt regardless of systemPrompt override" do
      let config = AudioTranscribe.defaultConfig {systemPrompt = Just "Custom"}
      let prompt = buildTranscriptionPrompt config
      prompt `shouldBe` "Please transcribe the spoken content from this audio file."

    it "prompt contains \"transcribe\" keyword" do
      let prompt = buildTranscriptionPrompt AudioTranscribe.defaultConfig
      Text.contains "transcribe" prompt `shouldBe` True

  describe "fileAccessToIntegrationError" do
    it "converts FileNotFound to ValidationError" do
      let err = fileAccessToIntegrationError (FileNotFound makeTestFileRef)
      err `shouldBe` Integration.ValidationError "Audio file access failed: file not found"

    it "converts StateLookupFailed to UnexpectedError" do
      let err = fileAccessToIntegrationError (StateLookupFailed makeTestFileRef "some error")
      err `shouldBe` Integration.UnexpectedError "Audio file access failed: state lookup error"

    it "converts NotOwner to AuthenticationError" do
      let err = fileAccessToIntegrationError (NotOwner makeTestFileRef)
      err `shouldBe` Integration.AuthenticationError "Audio file access failed: access denied"

    it "converts FileExpired to ValidationError" do
      let err = fileAccessToIntegrationError (FileExpired makeTestFileRef)
      err `shouldBe` Integration.ValidationError "Audio file access failed: file expired"

    it "converts FileIsDeleted to ValidationError" do
      let err = fileAccessToIntegrationError (FileIsDeleted makeTestFileRef)
      err `shouldBe` Integration.ValidationError "Audio file access failed: file deleted"

    it "converts BlobMissing to UnexpectedError" do
      let err = fileAccessToIntegrationError (BlobMissing makeTestFileRef)
      err `shouldBe` Integration.UnexpectedError "Audio file access failed: storage unavailable"

    it "converts StorageError to UnexpectedError" do
      let err = fileAccessToIntegrationError (StorageError "disk full")
      err `shouldBe` Integration.UnexpectedError "Audio file access failed: storage error"

    it "FileNotFound does not leak FileRef in message" do
      let err = fileAccessToIntegrationError (FileNotFound makeTestFileRef)
      case err of
        Integration.ValidationError msg ->
          Text.contains "00000000" msg `shouldBe` False
        _ ->
          expectationFailure "expected ValidationError constructor"

    it "StateLookupFailed does not leak error text in message" do
      let err = fileAccessToIntegrationError (StateLookupFailed makeTestFileRef "internal db error")
      case err of
        Integration.UnexpectedError msg ->
          Text.contains "internal db error" msg `shouldBe` False
        _ ->
          expectationFailure "expected UnexpectedError constructor"

    it "StorageError does not leak storage error text in message" do
      let err = fileAccessToIntegrationError (StorageError "disk full")
      case err of
        Integration.UnexpectedError msg ->
          Text.contains "disk full" msg `shouldBe` False
        _ ->
          expectationFailure "expected UnexpectedError constructor"

  describe "integrationErrorToText" do
    it "converts ValidationError to its message" do
      let result = integrationErrorToText (Integration.ValidationError "bad input")
      result `shouldBe` "bad input"

    it "converts AuthenticationError to its message" do
      let result = integrationErrorToText (Integration.AuthenticationError "not allowed")
      result `shouldBe` "not allowed"

    it "converts NetworkError to its message" do
      let result = integrationErrorToText (Integration.NetworkError "timeout")
      result `shouldBe` "timeout"

    it "converts RateLimited to \"Rate limited\"" do
      let result = integrationErrorToText (Integration.RateLimited 30)
      result `shouldBe` "Rate limited"

    it "converts PermanentFailure to its message" do
      let result = integrationErrorToText (Integration.PermanentFailure "gone")
      result `shouldBe` "gone"

    it "converts UnexpectedError to its message" do
      let result = integrationErrorToText (Integration.UnexpectedError "oops")
      result `shouldBe` "oops"

    it "ValidationError with empty message" do
      let result = integrationErrorToText (Integration.ValidationError "")
      result `shouldBe` ""

    it "RateLimited with 0 seconds" do
      let result = integrationErrorToText (Integration.RateLimited 0)
      result `shouldBe` "Rate limited"

    it "RateLimited with negative seconds" do
      let result = integrationErrorToText (Integration.RateLimited (-1))
      result `shouldBe` "Rate limited"

    it "UnexpectedError with unicode message" do
      let result = integrationErrorToText (Integration.UnexpectedError "エラー発生")
      result `shouldBe` "エラー発生"

  describe "defaultConfig" do
    it "has 180 second timeout" do
      AudioTranscribe.defaultConfig.timeoutSeconds `shouldBe` 180

    it "has no language hint (Nothing)" do
      AudioTranscribe.defaultConfig.language `shouldBe` Nothing

    it "has no maxDurationSeconds (Nothing)" do
      AudioTranscribe.defaultConfig.maxDurationSeconds `shouldBe` Nothing

    it "has no custom system prompt (Nothing)" do
      AudioTranscribe.defaultConfig.systemPrompt `shouldBe` Nothing

  describe "TranscriptionResult JSON" do
    it "roundtrips with all fields populated" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "Hello world"
            , duration = Just 45.5
            , confidence = Just 0.95
            , language = Just "en"
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with all optional fields Nothing" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "Minimal"
            , duration = Nothing
            , confidence = Nothing
            , language = Nothing
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with only duration populated" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "Some text"
            , duration = Just 120.0
            , confidence = Nothing
            , language = Nothing
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with duration and language, no confidence" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "Meeting notes"
            , duration = Just 45.5
            , confidence = Nothing
            , language = Just "en"
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with only confidence populated" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "Confident"
            , duration = Nothing
            , confidence = Just 0.99
            , language = Nothing
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with only language populated" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "Hola mundo"
            , duration = Nothing
            , confidence = Nothing
            , language = Just "es"
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with empty text" do
      let result = AudioTranscribe.TranscriptionResult
            { text = ""
            , duration = Nothing
            , confidence = Nothing
            , language = Nothing
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with unicode text" do
      let result = AudioTranscribe.TranscriptionResult
            { text = "こんにちは世界"
            , duration = Nothing
            , confidence = Nothing
            , language = Just "ja"
            }
      let encoded = Json.encode result
      let decoded = Json.decode encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

    it "roundtrips with long text" do
      let result = AudioTranscribe.TranscriptionResult
            { text = Text.repeat 1000 "word "
            , duration = Nothing
            , confidence = Nothing
            , language = Nothing
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.TranscriptionResult
      decoded `shouldSatisfy` Result.isOk

  describe "Config JSON" do
    it "roundtrips defaultConfig" do
      let config = AudioTranscribe.defaultConfig
      let encoded = Json.encodeText config
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.Config
      decoded `shouldBe` Result.Ok config

    it "roundtrips config with all fields set" do
      let config = Config
            { language = Just "en"
            , maxDurationSeconds = Just 300
            , systemPrompt = Just "Custom"
            , timeoutSeconds = 60
            }
      let encoded = Json.encodeText config
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.Config
      decoded `shouldBe` Result.Ok config

    it "roundtrips config with language only" do
      let config = AudioTranscribe.defaultConfig {language = Just "fr"}
      let encoded = Json.encodeText config
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.Config
      decoded `shouldBe` Result.Ok config

    it "roundtrips config with maxDurationSeconds only" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 600}
      let encoded = Json.encodeText config
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.Config
      decoded `shouldBe` Result.Ok config

    it "roundtrips config with zero maxDurationSeconds" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just 0}
      let encoded = Json.encodeText config
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.Config
      decoded `shouldBe` Result.Ok config

    it "roundtrips config with negative maxDurationSeconds" do
      let config = AudioTranscribe.defaultConfig {maxDurationSeconds = Just (-10)}
      let encoded = Json.encodeText config
      let decoded = Json.decodeText encoded :: Result Text AudioTranscribe.Config
      decoded `shouldBe` Result.Ok config


-- Test helpers

makeTestFileRef :: FileRef
makeTestFileRef = FileRef "00000000-0000-0000-0000-000000000001"
