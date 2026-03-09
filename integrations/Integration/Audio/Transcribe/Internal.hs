{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for AI audio transcription integration.
--
-- This module contains Nick's code - the 'ToAction' instance,
-- file retrieval, base64 encoding, prompt building, and OpenRouter
-- orchestration.
--
-- __This module is not exported to Jess.__
module Integration.Audio.Transcribe.Internal
  ( -- * For Testing Only
    buildSystemPrompt
  , buildTranscriptionPrompt
  , fileAccessToIntegrationError
  , integrationErrorToText
  ) where

import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import Integration qualified
import Integration.Audio.Transcribe (Config (..), Request (..), TranscriptionResult (..))
import Integration.OpenRouter.Internal ()
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Request qualified as OpenRouter
import Integration.OpenRouter.Response qualified as OpenRouter
import Json qualified
import Maybe (Maybe (..))
import Service.Command.Core (NameOf)
import Service.FileUpload.Core (FileAccessError (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | ToAction instance that executes the AI audio transcription.
--
-- This is the main entry point - when Jess writes:
--
-- @
-- Integration.outbound AudioTranscribe.Request { ... }
-- @
--
-- This instance converts the config into an executable action.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction config = Integration.action \ctx -> do
    executeTranscription ctx config


-- | Execute an AI audio transcription request.
--
-- NOT exported. Called only by the ToAction instance.
--
-- Steps:
-- 1. Extract FileAccessContext from ActionContext (throw if file uploads not enabled)
-- 2. Retrieve file bytes via fileAccess.retrieveFile
-- 3. Encode bytes to base64 text
-- 4. Build system prompt from Config
-- 5. Build user message with audio attachment via Message.userWithAttachment
-- 6. Create OpenRouter.Request delegating to the OpenRouter pipeline
-- 7. Run the OpenRouter action via Integration.runAction
executeTranscription ::
  forall command.
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ActionContext ->
  Request command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeTranscription ctx config = do
  -- Step 1: Get file access context
  fileAccess <- case ctx.fileAccess of
    Nothing ->
      Task.throw (Integration.ValidationError "File uploads not enabled. Cannot access file.")
    Just fa ->
      Task.yield fa

  -- Step 2: Retrieve file bytes
  fileBytes <- fileAccess.retrieveFile config.fileRef
    |> Task.mapError fileAccessToIntegrationError

  -- Step 3: Encode to base64
  let base64Text = fileBytes |> Bytes.toBase64 |> bytesToText

  -- Step 4: Build the system prompt
  let sysPrompt = buildSystemPrompt config.config

  -- Step 5: Build the user message with file attachment
  let userMsg = Message.userWithAttachment
        (buildTranscriptionPrompt config.config)
        base64Text
        config.mimeType

  -- Step 6: Create OpenRouter request
  let openRouterRequest = OpenRouter.Request
        { messages = [Message.system sysPrompt, userMsg]
        , model = config.model
        , config = OpenRouter.defaultConfig
            { OpenRouter.timeoutSeconds = config.config.timeoutSeconds
            }
        , onSuccess = \response ->
            case extractResponseText response of
              Just resultText -> do
                let result = TranscriptionResult
                      { text = resultText
                      , duration = Nothing
                      , confidence = Nothing
                      , language = Nothing
                      }
                config.onSuccess result
              Nothing ->
                config.onError "AI model returned empty or unparseable response"
        , onError = config.onError
        }

  -- Step 7: Delegate to OpenRouter integration
  let openRouterAction = Integration.toAction openRouterRequest
  Integration.runAction ctx openRouterAction


-- | Extract the text content from an OpenRouter response.
--
-- Handles both 'TextContent' (plain text) and 'MultiContent' (multimodal)
-- responses by extracting text parts from the content array.
--
-- NOT exported. Internal helper for executeTranscription.
extractResponseText :: OpenRouter.Response -> Maybe Text
extractResponseText response = do
  let choices = response.choices
  case choices |> Array.first of
    Nothing -> Nothing
    Just choice -> case choice.message.content of
      Message.TextContent text -> Just text
      Message.MultiContent parts -> do
        let textParts = parts
              |> Array.map (\part -> case part of
                  Message.TextPart text -> text
                  Message.ImageUrlPart _ -> "")
              |> Text.joinWith " "
        case textParts of
          "" -> Nothing
          result -> Just result


-- | Build the system prompt based on config.
--
-- Exported for testing.
--
-- Behavior:
-- 1. If config.systemPrompt is Just custom, return custom (no modification)
-- 2. Otherwise, build from base prompt + optional language hint + optional duration hint
-- 3. CRITICAL: maxDurationSeconds values <= 0 MUST be treated as Nothing (ignored)
buildSystemPrompt :: Config -> Text
buildSystemPrompt config = case config.systemPrompt of
  Just custom -> custom
  Nothing -> do
    let basePrompt :: Text
        basePrompt = "You are an audio transcription assistant. Transcribe the spoken content from the provided audio file faithfully and completely. Preserve speaker turns, pauses, and emphasis where possible. Output only the transcribed text."
    let languageHint :: Text
        languageHint = case config.language of
          Nothing -> ""
          Just lang -> [fmt| The audio is in #{lang}. Respond in the same language.|]
    let durationHint :: Text
        durationHint = case config.maxDurationSeconds of
          Nothing -> ""
          Just n ->
            if n > 0
              then do
                let nText = Text.fromInt n
                [fmt| Only transcribe the first #{nText} seconds of audio.|]
              else ""
    basePrompt |> Text.append languageHint |> Text.append durationHint


-- | Build the transcription prompt (user message text).
--
-- Exported for testing.
--
-- This is the text sent alongside the audio attachment.
-- Unlike OCR which varies by ExtractionMode, audio always uses the same prompt.
buildTranscriptionPrompt :: Config -> Text
buildTranscriptionPrompt _config = "Please transcribe the spoken content from this audio file."


-- | Convert FileAccessError to IntegrationError.
--
-- Exported for testing.
--
-- CRITICAL: All FileRef and internal message values MUST be discarded with _.
-- Error messages use generic "Audio file access failed: ..." prefix.
-- This prevents leaking internal identifiers to the onError callback.
fileAccessToIntegrationError :: FileAccessError -> Integration.IntegrationError
fileAccessToIntegrationError err = case err of
  FileNotFound _ ->
    Integration.ValidationError "Audio file access failed: file not found"
  StateLookupFailed _ _ ->
    Integration.UnexpectedError "Audio file access failed: state lookup error"
  NotOwner _ ->
    Integration.AuthenticationError "Audio file access failed: access denied"
  FileExpired _ ->
    Integration.ValidationError "Audio file access failed: file expired"
  FileIsDeleted _ ->
    Integration.ValidationError "Audio file access failed: file deleted"
  BlobMissing _ ->
    Integration.UnexpectedError "Audio file access failed: storage unavailable"
  StorageError _ ->
    Integration.UnexpectedError "Audio file access failed: storage error"


-- | Convert IntegrationError to Text for the onError callback.
--
-- Exported for testing.
integrationErrorToText :: Integration.IntegrationError -> Text
integrationErrorToText err = case err of
  Integration.ValidationError msg -> msg
  Integration.AuthenticationError msg -> msg
  Integration.NetworkError msg -> msg
  Integration.RateLimited _ -> "Rate limited"
  Integration.PermanentFailure msg -> msg
  Integration.UnexpectedError msg -> msg


-- | Convert Bytes to Text (UTF-8 decoding of base64 output).
--
-- NOT exported. Internal helper.
bytesToText :: Bytes -> Text
bytesToText bytes = bytes |> Text.fromBytes
