module Integration.Ai.TranscribePdf.InternalSpec (spec) where

import Basics
import Integration qualified
import Integration.Ai.TranscribePdf (Config (..), ExtractionMode (..))
import Integration.Ai.TranscribePdf qualified as AiTranscribe
import Integration.Ai.TranscribePdf.Internal
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
    it "produces FullText prompt by default" do
      let prompt = buildSystemPrompt AiTranscribe.defaultConfig
      Text.contains "Extract ALL text" prompt `shouldBe` True

    it "produces Summary prompt for Summary mode" do
      let config = AiTranscribe.defaultConfig {extractionMode = Summary}
      let prompt = buildSystemPrompt config
      Text.contains "summary" prompt `shouldBe` True

    it "produces Structured prompt for Structured mode" do
      let config = AiTranscribe.defaultConfig {extractionMode = Structured}
      let prompt = buildSystemPrompt config
      Text.contains "JSON" prompt `shouldBe` True

    it "appends language hint when provided" do
      let config = AiTranscribe.defaultConfig {language = Just "es"}
      let prompt = buildSystemPrompt config
      Text.contains "es" prompt `shouldBe` True

    it "appends maxPages hint when provided" do
      let config = AiTranscribe.defaultConfig {maxPages = Just 5}
      let prompt = buildSystemPrompt config
      Text.contains "5" prompt `shouldBe` True

    it "uses custom systemPrompt when provided" do
      let config = AiTranscribe.defaultConfig {systemPrompt = Just "Custom prompt"}
      let prompt = buildSystemPrompt config
      prompt `shouldBe` "Custom prompt"

  describe "buildExtractionPrompt" do
    it "produces FullText extraction prompt" do
      let prompt = buildExtractionPrompt AiTranscribe.defaultConfig
      Text.contains "extract all text" prompt `shouldBe` True

    it "produces Summary extraction prompt" do
      let config = AiTranscribe.defaultConfig {extractionMode = Summary}
      let prompt = buildExtractionPrompt config
      Text.contains "summarize" prompt `shouldBe` True

    it "produces Structured extraction prompt" do
      let config = AiTranscribe.defaultConfig {extractionMode = Structured}
      let prompt = buildExtractionPrompt config
      Text.contains "JSON" prompt `shouldBe` True

  describe "fileAccessToIntegrationError" do
    it "converts FileNotFound to ValidationError" do
      let ref = makeTestFileRef
      let err = fileAccessToIntegrationError (FileNotFound ref)
      err `shouldBe` Integration.ValidationError "File not found"

    it "converts NotOwner to AuthenticationError" do
      let ref = makeTestFileRef
      let err = fileAccessToIntegrationError (NotOwner ref)
      err `shouldBe` Integration.AuthenticationError "Not authorized to access this file"

    it "converts FileExpired to ValidationError" do
      let ref = makeTestFileRef
      let err = fileAccessToIntegrationError (FileExpired ref)
      err `shouldBe` Integration.ValidationError "File has expired"

    it "converts FileIsDeleted to ValidationError" do
      let ref = makeTestFileRef
      let err = fileAccessToIntegrationError (FileIsDeleted ref)
      err `shouldBe` Integration.ValidationError "File has been deleted"

    it "converts BlobMissing to UnexpectedError" do
      let ref = makeTestFileRef
      let err = fileAccessToIntegrationError (BlobMissing ref)
      err `shouldBe` Integration.UnexpectedError "File blob is missing from storage"

    it "converts StorageError to UnexpectedError" do
      let err = fileAccessToIntegrationError (StorageError "disk full")
      err `shouldBe` Integration.UnexpectedError "Storage error: disk full"

  describe "defaultConfig" do
    it "has FullText extraction mode" do
      AiTranscribe.defaultConfig.extractionMode `shouldBe` FullText

    it "has 120 second timeout" do
      AiTranscribe.defaultConfig.timeoutSeconds `shouldBe` 120

    it "has no language hint" do
      AiTranscribe.defaultConfig.language `shouldBe` Nothing

    it "has no maxPages limit" do
      AiTranscribe.defaultConfig.maxPages `shouldBe` Nothing

    it "has no custom system prompt" do
      AiTranscribe.defaultConfig.systemPrompt `shouldBe` Nothing

  describe "TranscriptionResult JSON" do
    it "roundtrips through JSON" do
      let result = AiTranscribe.TranscriptionResult
            { text = "Hello world"
            , pageCount = Just 3
            , confidence = Nothing
            }
      let encoded = Json.encodeText result
      let decoded = Json.decodeText encoded :: Result Text AiTranscribe.TranscriptionResult
      decoded `shouldBe` Result.Ok result

  describe "ExtractionMode JSON" do
    it "roundtrips FullText" do
      let encoded = Json.encodeText FullText
      let decoded = Json.decodeText encoded :: Result Text ExtractionMode
      decoded `shouldBe` Result.Ok FullText

    it "roundtrips Summary" do
      let encoded = Json.encodeText Summary
      let decoded = Json.decodeText encoded :: Result Text ExtractionMode
      decoded `shouldBe` Result.Ok Summary

    it "roundtrips Structured" do
      let encoded = Json.encodeText Structured
      let decoded = Json.decodeText encoded :: Result Text ExtractionMode
      decoded `shouldBe` Result.Ok Structured


-- Test helpers

makeTestFileRef :: FileRef
makeTestFileRef = FileRef "00000000-0000-0000-0000-000000000001"
