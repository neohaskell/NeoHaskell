-- | # AI-Powered Audio Transcription Integration
--
-- This module provides AI-powered audio transcription using multimodal
-- models (Gemini, GPT-4o) via the OpenRouter API.
--
-- == Two-Persona Model
--
-- Following ADR-0008 and ADR-0041, this integration separates concerns:
--
-- * __Jess (Integration User)__: Configures transcription with pure records.
--   No @Task@, no API calls, no base64 encoding visible.
--
-- * __Nick (Integration Developer)__: Implements 'ToAction' with file retrieval,
--   base64 encoding, and OpenRouter orchestration in
--   "Integration.Audio.Transcribe.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Audio.Transcribe qualified as AudioTranscribe
--
-- meetingIntegrations :: Meeting -> MeetingEvent -> Integration.Outbound
-- meetingIntegrations meeting event = case event of
--   RecordingUploaded info -> Integration.batch
--     [ Integration.outbound AudioTranscribe.Request
--         { fileRef = info.fileRef
--         , mimeType = "audio\/mpeg"
--         , model = "google\/gemini-2.0-flash"
--         , config = AudioTranscribe.defaultConfig
--         , onSuccess = \\result -> MeetingNotesReady
--             { meetingId = meeting.id
--             , transcript = result.text
--             }
--         , onError = \\err -> TranscriptionFailed
--             { meetingId = meeting.id
--             , error = err
--             }
--         }
--     ]
--   _ -> Integration.none
-- @
--
-- == Supported Audio Formats
--
-- * @audio\/wav@ — WAV (uncompressed)
-- * @audio\/mpeg@ — MP3 (compressed, most common)
-- * @audio\/mp4@ — M4A\/AAC (compressed, Apple devices)
-- * @audio\/ogg@ — OGG\/OGA (open-source, web)
--
-- == Dispatcher Timeout Warning
--
-- The default 'timeoutSeconds' is 180 (3 minutes). The integration dispatcher's
-- @eventProcessingTimeoutMs@ defaults to 30000 (30 seconds). __You MUST configure
-- the dispatcher timeout to at least 180000 (or 'Nothing' to disable)__, otherwise
-- the dispatcher will kill the worker before transcription completes:
--
-- @
-- Application.new
--   |> Application.withDispatcherConfig Dispatcher.defaultConfig
--       { Dispatcher.eventProcessingTimeoutMs = Just 180000
--       }
-- @
--
-- == Data Privacy Notice
--
-- Audio file content is sent to third-party AI providers via OpenRouter.
-- Audio data may contain sensitive spoken content (PII, medical, legal).
-- Ensure appropriate data processing agreements are in place.
-- See ADR-0041 for security and privacy considerations.
--
-- == Recommended File Sizes
--
-- Keep audio files under 25 MB and prefer compressed formats (MP3, M4A, OGG)
-- over uncompressed (WAV) for large recordings. For files exceeding model input
-- limits, see issue #457 (chunked transcription).
--
-- == Known Limitations (v1)
--
-- * No streaming support
-- * No confidence scoring (always Nothing)
-- * No chunked transcription for large files (see #457)
-- * Duration and language detection depend on model capabilities
module Integration.Audio.Transcribe
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)

    -- * Result Types
  , TranscriptionResult (..)

    -- * Config Helpers
  , defaultConfig
  ) where

import Basics
import Json qualified
import Maybe (Maybe (..))
import Service.FileUpload.Core (FileRef)
import Text (Text)


-- | Configuration for audio transcription.
--
-- Use 'defaultConfig' for sensible defaults, then override specific fields:
--
-- @
-- AudioTranscribe.defaultConfig
--   { language = Just "en"
--   , maxDurationSeconds = Just 300
--   }
-- @
data Config = Config
  { language :: Maybe Text
  -- ^ Language hint for the AI model (e.g., "en", "es", "de").
  --   Improves transcription accuracy for known languages.
  --   Nothing = auto-detect.
  , maxDurationSeconds :: Maybe Int
  -- ^ Maximum audio duration to process in seconds.
  --   Nothing = no limit (process entire file).
  --   Non-positive values are ignored (treated as Nothing).
  --   Useful for capping costs on long recordings.
  , systemPrompt :: Maybe Text
  -- ^ Override the default system prompt.
  --   Nothing = use the built-in transcription prompt.
  , timeoutSeconds :: Int
  -- ^ Timeout for the API call (default: 180).
  --   Audio transcription is typically slower than OCR,
  --   so the default is higher than OCR's 120 seconds.
  }
  deriving (Show, Eq, Generic)


instance Json.ToJSON Config
instance Json.FromJSON Config


-- | Result of audio transcription.
--
-- @
-- onSuccess = \\result -> RecordTranscription
--   { text = result.text
--   , duration = result.duration
--   , language = result.language
--   }
-- @
data TranscriptionResult = TranscriptionResult
  { text :: Text
  -- ^ Transcribed text from the audio
  , duration :: Maybe Float
  -- ^ Duration of the audio in seconds (if reported by the model).
  --   Float is Double-precision in NeoHaskell.
  , confidence :: Maybe Float
  -- ^ Confidence score (always Nothing for v1).
  --   Reserved for future model support.
  , language :: Maybe Text
  -- ^ Detected language code (e.g., "en", "es").
  --   Populated when the model reports the detected language.
  }
  deriving (Show, Eq, Generic)


instance Json.ToJSON TranscriptionResult
instance Json.FromJSON TranscriptionResult


-- | The main request configuration record that Jess instantiates.
--
-- The @command@ type parameter is the domain command emitted by
-- 'onSuccess' or 'onError' callbacks.
--
-- == Fields
--
-- * 'fileRef': Reference to the uploaded audio file
-- * 'mimeType': MIME type of the audio file (e.g., @"audio\/wav"@, @"audio\/mpeg"@, @"audio\/mp4"@)
-- * 'model': AI model to use (e.g., @"google\/gemini-2.0-flash"@)
-- * 'config': Transcription configuration (use 'defaultConfig')
-- * 'onSuccess': Callback that receives result and returns a domain command
-- * 'onError': Callback for error handling
--
-- == Example
--
-- @
-- Integration.outbound AudioTranscribe.Request
--   { fileRef = e.uploadedFile
--   , mimeType = "audio\/mpeg"
--   , model = "google\/gemini-2.0-flash"
--   , config = AudioTranscribe.defaultConfig
--       { language = Just "en"
--       }
--   , onSuccess = \\result -> RecordTranscription
--       { text = result.text
--       , duration = result.duration
--       }
--   , onError = \\err -> TranscriptionFailed
--       { error = err
--       }
--   }
-- @
data Request command = Request
  { fileRef :: FileRef
  -- ^ Reference to the uploaded audio file
  , mimeType :: Text
  -- ^ MIME type of the audio file (e.g., "audio/wav", "audio/mpeg", "audio/mp4")
  , model :: Text
  -- ^ AI model to use (e.g., "google/gemini-2.0-flash", "openai/gpt-4o-audio-preview")
  , config :: Config
  -- ^ Transcription configuration
  , onSuccess :: TranscriptionResult -> command
  -- ^ Callback for successful transcription
  , onError :: Text -> command
  -- ^ Callback for transcription errors
  }
  deriving (Generic)


-- | Default configuration for audio transcription.
--
-- * Language: None (auto-detect)
-- * Max duration: None (process entire file)
-- * System prompt: None (use default)
-- * Timeout: 180 seconds
{-# INLINE defaultConfig #-}
defaultConfig :: Config
defaultConfig = Config
  { language = Nothing
  , maxDurationSeconds = Nothing
  , systemPrompt = Nothing
  , timeoutSeconds = 180
  }
