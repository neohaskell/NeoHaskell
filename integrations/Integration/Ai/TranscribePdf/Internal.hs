{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for AI PDF transcription integration.
--
-- This module contains Nick's code - the 'ToAction' instance,
-- file retrieval, base64 encoding, prompt building, and OpenRouter
-- orchestration.
--
-- __This module is not exported to Jess.__
module Integration.Ai.TranscribePdf.Internal
  ( -- * For Testing Only
    buildSystemPrompt
  , buildExtractionPrompt
  , fileAccessToIntegrationError
  , integrationErrorToText
  ) where

import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import Integration qualified
import Integration.Ai.TranscribePdf (Config (..), ExtractionMode (..), Request (..), TranscriptionResult (..))
import Integration.OpenRouter.Internal ()
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Request qualified as OpenRouter
import Integration.OpenRouter.Response qualified as OpenRouter
import Json qualified
import Maybe (Maybe (..))
import Service.Command.Core (NameOf)
import Service.FileUpload.Core (FileAccessError (..), FileRef (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | ToAction instance that executes the AI transcription.
--
-- This is the main entry point - when Jess writes:
--
-- @
-- Integration.outbound AiTranscribe.Request { ... }
-- @
--
-- This instance converts the config into an executable action.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction config = Integration.action \ctx -> do
    executeTranscription ctx config


-- | Execute an AI transcription request.
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
      Task.throw (Integration.ValidationError "File uploads not enabled. Cannot access PDF file.")
    Just fa ->
      Task.yield fa

  -- Step 2: Retrieve PDF bytes
  pdfBytes <- fileAccess.retrieveFile config.fileRef
    |> Task.mapError fileAccessToIntegrationError

  -- Step 3: Encode to base64
  let base64Text = pdfBytes |> Bytes.toBase64 |> bytesToText

  -- Step 4: Build the system prompt
  let sysPrompt = buildSystemPrompt config.config

  -- Step 5: Build the user message with PDF attachment
  let userMsg = Message.userWithAttachment
        (buildExtractionPrompt config.config)
        base64Text
        "application/pdf"

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
                      , pageCount = Nothing
                      , confidence = Nothing
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
extractResponseText :: OpenRouter.Response -> Maybe Text
extractResponseText response = do
  let choices = response.choices
  case choices |> Array.first of
    Nothing -> Nothing
    Just choice -> case choice.message.content of
      Message.TextContent text -> Just text
      Message.MultiContent _ -> Nothing


-- | Build the system prompt based on extraction mode and config.
buildSystemPrompt :: Config -> Text
buildSystemPrompt config = case config.systemPrompt of
  Just custom -> custom
  Nothing -> do
    let basePrompt :: Text
        basePrompt = case config.extractionMode of
          FullText ->
            "You are a document transcription assistant. Extract ALL text from the provided PDF document faithfully and completely. Preserve the original structure, headings, paragraphs, and formatting as much as possible. Do not summarize or omit any content."
          Summary ->
            "You are a document summarization assistant. Read the provided PDF document and produce a clear, concise summary of its contents. Capture the key points, main arguments, and important details."
          Structured ->
            "You are a structured data extraction assistant. Extract information from the provided PDF document and return it as a well-structured JSON object. Identify key fields, tables, and data points."
    let languageHint :: Text
        languageHint = case config.language of
          Nothing -> ""
          Just lang -> [fmt| The document is in #{lang}. Respond in the same language.|]
    let pageHint :: Text
        pageHint = case config.maxPages of
          Nothing -> ""
          Just n -> do
            let nText = Text.fromInt n
            [fmt| Only process the first #{nText} pages.|]
    basePrompt |> Text.append languageHint |> Text.append pageHint


-- | Build the extraction prompt (user message text).
buildExtractionPrompt :: Config -> Text
buildExtractionPrompt config = case config.extractionMode of
  FullText ->
    "Please extract all text from this PDF document."
  Summary ->
    "Please summarize the contents of this PDF document."
  Structured ->
    "Please extract structured data from this PDF document and return it as JSON."


-- | Convert FileAccessError to IntegrationError.
fileAccessToIntegrationError :: FileAccessError -> Integration.IntegrationError
fileAccessToIntegrationError err = case err of
  FileNotFound (FileRef ref) ->
    Integration.ValidationError [fmt|File not found: #{ref}|]
  StateLookupFailed _ msg ->
    Integration.UnexpectedError [fmt|Failed to lookup file state: #{msg}|]
  NotOwner (FileRef ref) ->
    Integration.AuthenticationError [fmt|Not authorized to access file: #{ref}|]
  FileExpired (FileRef ref) ->
    Integration.ValidationError [fmt|File has expired: #{ref}|]
  FileIsDeleted (FileRef ref) ->
    Integration.ValidationError [fmt|File has been deleted: #{ref}|]
  BlobMissing (FileRef ref) ->
    Integration.UnexpectedError [fmt|File blob is missing from storage: #{ref}|]
  StorageError msg ->
    Integration.UnexpectedError [fmt|Storage error: #{msg}|]


-- | Convert IntegrationError to Text for the onError callback.
integrationErrorToText :: Integration.IntegrationError -> Text
integrationErrorToText err = case err of
  Integration.ValidationError msg -> msg
  Integration.AuthenticationError msg -> msg
  Integration.NetworkError msg -> msg
  Integration.RateLimited _ -> "Rate limited"
  Integration.PermanentFailure msg -> msg
  Integration.UnexpectedError msg -> msg


-- | Convert Bytes to Text (UTF-8 decoding of base64 output).
bytesToText :: Bytes -> Text
bytesToText bytes = bytes |> Text.fromBytes |> Text.trim
