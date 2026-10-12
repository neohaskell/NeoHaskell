-- | # Video Audio Extraction Integration
--
-- This module extracts the audio track of an uploaded video file as a mono
-- 16 kHz WAV, using the local @ffmpeg@ command-line tool. It is the missing
-- first step of a video-transcription pipeline: "Integration.Audio.Transcribe"
-- accepts audio, so a video has to be turned into audio first.
--
-- == Two-Persona Model
--
-- Following ADR-0008, ADR-0015 and ADR-0041, this integration separates
-- concerns:
--
-- * **Jess (Integration User)**: Configures extraction with pure records.
--   No @Task@, no subprocess handling, no temp files visible.
--
-- * **Nick (Integration Developer)**: Implements 'ToAction' with ffmpeg
--   execution, temp file management, and error handling in
--   "Integration.Video.ExtractAudio.Internal".
--
-- == Quick Start
--
-- @
-- import Bytes qualified
-- import Core
-- import Integration qualified
-- import Integration.Video.ExtractAudio qualified as VideoAudio
-- import Integration.Video.ExtractAudio.Internal ()
-- import Text qualified
--
-- lectureIntegrations :: Lecture -> LectureEvent -> Integration.Outbound
-- lectureIntegrations lecture event = case event of
--   VideoUploaded info -> Integration.batch
--     [ Integration.outbound VideoAudio.Request
--         { fileRef = info.fileRef
--         , config = VideoAudio.defaultConfig
--         , onSuccess = \\result -> RecordExtractedAudio
--             { lectureId = lecture.id
--             , audioBase64 = result.audio |> Bytes.toBase64 |> Text.fromBytes
--             , mimeType = result.mimeType
--             }
--         , onError = \\err -> AudioExtractionFailed
--             { lectureId = lecture.id
--             , error = err
--             }
--         }
--     ]
--   _ -> Integration.none
-- @
--
-- The @Internal ()@ import brings the 'Integration.ToAction' instance into
-- scope; without it @Integration.outbound@ does not accept a 'Request'.
--
-- == Carrying the audio in a command
--
-- Commands are serialized as JSON, and 'Bytes' has no JSON form. Encode
-- 'audio' as Base64 text (as above): every byte survives. Never use
-- @Text.fromBytes result.audio@ directly; WAV audio is not UTF-8 text.
--
-- Your command handler decodes the text back to the exact WAV bytes before
-- uploading it, with 'Bytes.fromBase64'. Malformed text is an 'Err', so a
-- corrupted command never turns into silently wrong audio:
--
-- @
-- import Bytes qualified
-- import Core
-- import Text qualified
--
-- decodeAudio :: Text -> Result Text Bytes
-- decodeAudio audioBase64 =
--   audioBase64
--     |> Text.toBytes
--     |> Bytes.fromBase64
-- @
--
-- Base64 makes the command about a third larger than the audio itself.
--
-- == Dispatcher Timeout Warning
--
-- The default 'timeoutSeconds' is 180 (3 minutes). The integration
-- dispatcher's @eventProcessingTimeoutMs@ defaults to 30000 (30 seconds).
-- __You MUST configure the dispatcher timeout to more than
-- @timeoutSeconds * 1000@ plus the time to fetch the video (or 'Nothing' to
-- disable it)__. Otherwise the dispatcher cancels the worker first: a slow
-- extraction never finishes, and an ffmpeg timeout never reaches 'onError'.
--
-- @
-- import Core
-- import Service.Application (Application)
-- import Service.Application qualified as Application
-- import Service.Integration.Dispatcher qualified as Dispatcher
--
-- app :: Application
-- app =
--   Application.new
--     |> Application.withDispatcherConfig \@() (\\_ -> Dispatcher.defaultConfig
--         { Dispatcher.eventProcessingTimeoutMs = Just 200000
--         })
-- @
--
-- 'Service.Application.withDispatcherConfig' takes a function of your
-- application config; @\@()@ with @\\_ ->@ means the setting does not
-- depend on it. @Core@ provides 'Bytes', 'Text', 'Result', 'Just' and @|>@.
--
-- If you raise 'timeoutSeconds', raise the dispatcher timeout with it.
--
-- == Transcribing a video
--
-- This integration only produces audio. To get a transcript, chain two
-- steps in your own domain (this module does not do the chaining for you):
--
-- 1. A video event triggers 'Request'. Its 'onSuccess' command carries
--    the extracted 'audio'.
-- 2. Your command handler uploads that audio as a new file and records an
--    event with its file reference.
-- 3. That event triggers "Integration.Audio.Transcribe" with
--    @mimeType = "audio/wav"@:
--
-- @
-- Integration.outbound AudioTranscribe.Request
--   { fileRef = e.audioFileRef
--   , mimeType = "audio/wav"
--   , model = "google/gemini-2.5-flash"
--   , config = AudioTranscribe.defaultConfig
--   , onSuccess = \\transcript -> RecordTranscript { text = transcript.text }
--   , onError = \\err -> TranscriptionFailed { error = err }
--   }
-- @
--
-- == Output
--
-- WAV, mono, 16 kHz by default ('mimeType' is @audio/wav@). The result is
-- capped at 'maxOutputBytes' (default 25 MB); ffmpeg stops writing at that
-- size, so a very long video yields only its first part.
--
-- == Requirements
--
-- This integration requires @ffmpeg@ on the @PATH@ (or set 'ffmpegPath'):
--
-- @
-- # Ubuntu/Debian
-- apt-get install ffmpeg
--
-- # macOS
-- brew install ffmpeg
--
-- # Nix
-- nix-shell -p ffmpeg
-- @
--
-- If @ffmpeg@ is missing, the integration fails with a validation error that
-- tells you to install it.
--
-- == Known Limitations
--
-- * A video without an audio track fails with an ffmpeg error.
-- * The whole video is held in memory and written to a temp file.
module Integration.Video.ExtractAudio
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)

    -- * Result Types
  , ExtractionResult (..)

    -- * Config Helpers
  , defaultConfig
  ) where

import Basics
import Bytes (Bytes)
import Maybe (Maybe (..))
import Service.FileUpload.Core (FileRef)
import Text (Text)


-- | Configuration for audio extraction.
--
-- Use 'defaultConfig' for sensible defaults, then override specific fields:
--
-- @
-- VideoAudio.defaultConfig
--   { timeoutSeconds = 600
--   , ffmpegPath = Just "/opt/ffmpeg/bin/ffmpeg"
--   }
-- @
data Config = Config
  { timeoutSeconds :: Int
  -- ^ Timeout for the ffmpeg run (default: 180). The dispatcher's
  --   @eventProcessingTimeoutMs@ must be longer; see the Dispatcher Timeout
  --   Warning above.
  , sampleRateHz :: Int
  -- ^ Output sample rate in Hz (default: 16000)
  , channels :: Int
  -- ^ Output channel count (default: 1, mono)
  , maxOutputBytes :: Int
  -- ^ Stop writing audio after this many bytes (default: 25000000)
  , ffmpegPath :: Maybe Text
  -- ^ Explicit ffmpeg executable. Nothing means look up @ffmpeg@ on the PATH.
  }
  deriving (Show, Eq, Generic)


-- | Default configuration for audio extraction.
--
-- * Timeout: 180 seconds
-- * Sample rate: 16000 Hz
-- * Channels: 1 (mono)
-- * Maximum output: 25 MB
-- * ffmpeg: found on the PATH
--
-- >>> defaultConfig.timeoutSeconds
-- 180
--
-- >>> defaultConfig.sampleRateHz
-- 16000
defaultConfig :: Config
defaultConfig = Config
  { timeoutSeconds = 180
  , sampleRateHz = 16000
  , channels = 1
  , maxOutputBytes = 25000000
  , ffmpegPath = Nothing
  }


-- | Result of audio extraction.
data ExtractionResult = ExtractionResult
  { audio :: Bytes
  -- ^ The extracted audio, as WAV
  , mimeType :: Text
  -- ^ Always @audio/wav@; pass it on to "Integration.Audio.Transcribe"
  }
  deriving (Show, Eq, Generic)


-- | The main request configuration record that Jess instantiates.
--
-- The @command@ type parameter is the domain command emitted by
-- 'onSuccess' or 'onError' callbacks.
--
-- == Fields
--
-- * 'fileRef': Reference to the uploaded video file
-- * 'config': Extraction configuration (use 'defaultConfig')
-- * 'onSuccess': Callback that receives the audio and returns a domain command
-- * 'onError': Callback for error handling
data Request command = Request
  { fileRef :: FileRef
  -- ^ Reference to the uploaded video file
  , config :: Config
  -- ^ Extraction configuration
  , onSuccess :: ExtractionResult -> command
  -- ^ Callback for successful extraction
  , onError :: Text -> command
  -- ^ Callback for extraction errors
  }
  deriving (Generic)
