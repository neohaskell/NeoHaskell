# ADR-0041: Audio Transcription Integration via Multimodal Models

## Status

Proposed

## Context

NeoHaskell has `Integration.Ocr.Ai` for AI-powered document and image text extraction using multimodal models via OpenRouter. However, there is no equivalent integration for audio files. Modern multimodal models (Google Gemini, OpenAI GPT-4o) can process audio content — transcribing speech, extracting dialogue, and identifying spoken language — but NeoHaskell provides no way for Jess to trigger audio transcription from domain events.

### Current State

1. **`Integration.Ocr.Ai`** — AI-powered document/image transcription via OpenRouter. Follows the Two-Persona Model (ADR-0008): Jess configures with pure records (`Request`, `Config`, `defaultConfig`), Nick implements `ToAction` in `Integration.Ocr.Ai.Internal`. This is the template for the new audio module.

2. **`Integration.OpenRouter`** — AI model access via OpenRouter API. Already supports multimodal content through `Message.userWithAttachment`, which builds data URLs from base64-encoded content with arbitrary MIME types. Audio MIME types (`audio/wav`, `audio/mpeg`, `audio/mp4`) work with the same attachment mechanism.

3. **`Integration.OpenRouter.Message`** — Provides `userWithAttachment :: Text -> Text -> Text -> Message` which accepts any MIME type. No changes needed to support audio.

4. **`ActionContext.fileAccess`** — Already exists in `Integration.hs`. `FileAccessContext.retrieveFile` returns `Task FileAccessError Bytes` for any uploaded file, regardless of format.

5. **`Bytes.toBase64`** — Base64 encoding utility in `core/core/Bytes.hs`. Works on any binary content.

### Use Cases

- **Voice memo transcription**: A project management app lets users upload voice memos. When a `VoiceMemoUploaded` event fires, the integration transcribes the audio and emits a `RecordTranscription` command with the text.

- **Podcast indexing**: A media platform ingests podcast episodes. After upload, the integration transcribes each episode so the content is searchable via NeoQL queries.

- **Meeting notes extraction**: A collaboration tool receives meeting recordings. The integration transcribes the audio and emits a `MeetingNotesReady` command with the transcript text, duration, and detected language.

- **Multilingual customer support**: A support platform receives audio messages in multiple languages. The integration uses the `language` hint to improve transcription accuracy and returns the detected language in the result.

### Design Goals

1. **Mirror the OCR pattern exactly**: Jess already knows `Integration.Ocr.Ai`. The audio integration should feel identical — same `Request`/`Config`/`defaultConfig`/`TranscriptionResult` shape, same `onSuccess`/`onError` callbacks, same `Integration.outbound` wiring. If Jess can use OCR, she can use audio transcription without learning anything new.

2. **Audio-specific configuration**: Audio transcription has different tuning knobs than document OCR. Instead of `extractionMode` and `maxPages`, audio needs `language` (hint for speech recognition), `maxDurationSeconds` (cap processing time for long recordings), and a `systemPrompt` override.

3. **Separation of concerns**: Audio transcription is a distinct domain from document/image OCR. Combining them into one module would create a grab-bag with confusing config options (what does `maxPages` mean for an MP3?). A dedicated `Integration.Audio.Transcribe` module keeps each integration focused.

4. **Reuse existing infrastructure**: No changes to `ActionContext`, `FileAccessContext`, `OpenRouter`, or `Message`. The new module piggybacks on the same OpenRouter → HTTP pipeline as OCR.

### GitHub Issue

- [#451: Audio Transcription Integration](https://github.com/neohaskell/NeoHaskell/issues/451)

## Decision

### 1. Module Placement: Separate from OCR

Two new modules in `nhintegrations`, following the Jess/Nick two-persona pattern established by `Integration.Ocr.Ai`:

| Module | Persona | Purpose |
|--------|---------|---------|
| `Integration.Audio.Transcribe` | Jess (User) | Config records, types, `defaultConfig` |
| `Integration.Audio.Transcribe.Internal` | Nick (Developer) | `ToAction` instance, orchestration logic |

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Extend `Integration.Ocr.Ai` with audio support | Rejected | OCR config has `extractionMode` (FullText/Summary/Structured) and `maxPages` which are meaningless for audio. Combining them creates a confusing grab-bag. Jess would see `maxPages` in autocomplete when transcribing a WAV file. |
| `Integration.Audio.Transcribe` (new module) | **Chosen** | Clean separation. Audio-specific config (`maxDurationSeconds`, no `extractionMode`). Mirrors the OCR module structure exactly so Jess recognizes the pattern. Future audio integrations (e.g., `Integration.Audio.Classify`) can live alongside it. |
| `Integration.Ai.TranscribeAudio` | Rejected | Inconsistent with the `Integration.Ocr.Ai` naming. The `Integration.Audio.*` namespace groups by media type, which is more intuitive for Jess browsing autocomplete. |

### 2. Supported Audio MIME Types

The module accepts any MIME type string (the field is `Text`, not an enum), but documentation and examples highlight the three primary formats:

| MIME Type | Format | Notes |
|-----------|--------|-------|
| `audio/wav` | WAV | Uncompressed, highest quality, largest files |
| `audio/mpeg` | MP3 | Compressed, most common format |
| `audio/mp4` | M4A/AAC | Compressed, common on Apple devices |
| `audio/ogg` | OGG/OGA | Open-source compressed format, common on Linux and web |
The MIME type is passed through to OpenRouter's `userWithAttachment` as-is. Model support for specific audio formats depends on the chosen model (e.g., Gemini supports all three; other models may vary).

### 3. Type Definitions

All types follow NeoHaskell conventions: descriptive type parameters, strict fields (via project-wide `Strict` extension), `Result` not `Either`, `Task` not `IO`.

#### Configuration

```haskell
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
```

#### Result Type

```haskell
-- | Result of audio transcription.
--
-- @
-- onSuccess = \result -> RecordTranscription
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
```

#### Request Record (Jess's API)

```haskell
-- | The main request configuration record that Jess instantiates.
--
-- The @command@ type parameter is the domain command emitted by
-- 'onSuccess' or 'onError' callbacks.
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
```

### 4. Public API

```haskell
-- Integration.Audio.Transcribe (Jess's module)
module Integration.Audio.Transcribe
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)

    -- * Result Types
  , TranscriptionResult (..)

    -- * Config Helpers
  , defaultConfig
  ) where
```

#### Default Configuration

```haskell
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
```

### 5. Internal Module (Nick's Implementation)

`Integration.Audio.Transcribe.Internal` implements the `ToAction` instance following the same pattern as `Integration.Ocr.Ai.Internal`:

```haskell
-- Integration.Audio.Transcribe.Internal (Nick's module)
module Integration.Audio.Transcribe.Internal
  ( -- * For Testing Only
    buildSystemPrompt
  , buildTranscriptionPrompt
  , fileAccessToIntegrationError
  , integrationErrorToText
  ) where
```

#### Execution Flow

```text
AudioTranscribe.Request
  → ToAction.toAction
    → Integration.action \ctx ->
      → ctx.fileAccess.retrieveFile fileRef    -- Get audio bytes
      → Bytes.toBase64 fileBytes               -- Encode to base64
      → Message.userWithAttachment prompt base64 config.mimeType  -- Build multimodal message
      → OpenRouter.Request { messages, model, config, onSuccess, onError }
        → OpenRouter.toAction                  -- Delegate to OpenRouter pipeline
      → Parse response → TranscriptionResult
      → Integration.emitCommand (config.onSuccess result)
```

#### System Prompt

```haskell
-- | Build the system prompt based on config.
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
          Just n -> do
            let nText = Text.fromInt n
            [fmt| Only transcribe the first #{nText} seconds of audio.|]
    basePrompt |> Text.append languageHint |> Text.append durationHint
```

#### Transcription Prompt

```haskell
-- | Build the transcription prompt (user message text).
buildTranscriptionPrompt :: Config -> Text
buildTranscriptionPrompt _config =
  "Please transcribe the spoken content from this audio file."
```

#### Error Mapping

The `fileAccessToIntegrationError` and `integrationErrorToText` functions follow the same pattern as `Integration.Ocr.Ai.Internal`, mapping `FileAccessError` variants to `IntegrationError` with generic, audio-prefixed messages. Internal details (FileRef UUIDs, state store messages) are discarded to prevent information leakage:

```haskell
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
```

### 6. Jess's Usage (Complete Example)

```haskell
import Integration qualified
import Integration.Audio.Transcribe qualified as AudioTranscribe

-- In the entity's integration function:
meetingIntegrations :: Meeting -> MeetingEvent -> Integration.Outbound
meetingIntegrations meeting event = case event of
  RecordingUploaded info -> Integration.batch
    [ Integration.outbound AudioTranscribe.Request
        { fileRef = info.fileRef
        , mimeType = "audio/mpeg"
        , model = "google/gemini-2.0-flash"
        , config = AudioTranscribe.defaultConfig
            { language = Just "en"
            , maxDurationSeconds = Just 600
            }
        , onSuccess = \result -> MeetingNotesReady
            { meetingId = meeting.id
            , transcript = result.text
            , detectedLanguage = result.language
            }
        , onError = \err -> TranscriptionFailed
            { meetingId = meeting.id
            , error = err
            }
        }
    ]
  _ -> Integration.none
```

### 7. Instance Decisions

| Instance | Decision | Rationale |
|----------|----------|-----------|
| `Generic` | **Yes** (derived) | Standard for all NeoHaskell types |
| `Show` | **Yes** (derived) on `Config`, `TranscriptionResult` | Debugging |
| `Eq` | **Yes** (derived) on `Config`, `TranscriptionResult` | Testing |
| `ToJSON`/`FromJSON` | **Derived** for `Config`, `TranscriptionResult` | Standard serialization, no custom format needed |
| `ToAction` | **Custom** in Internal | Orchestration logic (same pattern as OCR) |

### 8. Performance Considerations

- **Higher default timeout**: Audio files are typically larger than documents/images and take longer to process. The default timeout is 180 seconds (vs 120 for OCR).
- **Memory per request**: A 10MB MP3 produces ~13.3MB of base64, with peak memory ~50MB per request. Same profile as OCR. Strict fields prevent space leaks.
- **INLINE pragmas**: `defaultConfig` is marked `{-# INLINE #-}` as a small, frequently-called function.
- **Worker pool impact**: Each in-flight transcription holds a dispatcher worker thread for up to 180 seconds. High-volume usage should be rate-limited at the application level.
- **Dispatcher timeout**: The dispatcher's `eventProcessingTimeoutMs` defaults to 30 seconds. Audio transcription's default timeout is 180 seconds. **Applications using audio transcription MUST configure `eventProcessingTimeoutMs` to at least `180000` (or `Nothing` to disable)**, otherwise the dispatcher will kill the worker before the transcription completes. The module Haddock will include a prominent warning about this.
- **Recommended file sizes**: Keep audio files under 25MB and prefer compressed formats (MP3, M4A, OGG) over uncompressed (WAV) for large recordings.

### 9. Error Handling

`FileAccessError` variants map to `IntegrationError` using the same mapping as OCR, with audio-specific error message prefixes for clarity in logs:

| FileAccessError | IntegrationError | Rationale |
|----------------|-----------------|-----------|
| `FileNotFound` | `ValidationError` | Invalid file reference |
| `StateLookupFailed` | `UnexpectedError` | State store inconsistency |
| `NotOwner` | `AuthenticationError` | Access denied |
| `FileExpired` | `ValidationError` | File no longer available |
| `FileIsDeleted` | `ValidationError` | File was deleted |
| `BlobMissing` | `UnexpectedError` | Storage inconsistency |
| `StorageError` | `UnexpectedError` | Infrastructure failure |

No sensitive information (file paths, internal IDs beyond FileRef) is exposed in error messages.

### 10. Data Privacy Notice

The module Haddock documentation MUST include a Data Privacy Notice section, equivalent to the one in `Integration.Ocr.Ai`:

```
-- == Data Privacy Notice
--
-- Audio file content is sent to third-party AI providers via OpenRouter.
-- Audio data may contain sensitive spoken content (PII, medical, legal).
-- Ensure appropriate data processing agreements are in place.
-- See ADR-0041 for security and privacy considerations.
```

## Consequences

### Positive

1. **Jess already knows the pattern**: The API is structurally identical to `Integration.Ocr.Ai`. If Jess has used OCR transcription, she can use audio transcription by changing the import and adjusting config fields. Zero new concepts to learn.

2. **Reuses existing infrastructure**: No changes to `ActionContext`, `FileAccessContext`, `OpenRouter`, `Message`, or `Application.hs`. The new module piggybacks on the same OpenRouter → HTTP pipeline.

3. **Audio-specific defaults**: The 180-second timeout and `maxDurationSeconds` config reflect the reality that audio processing is slower and potentially more expensive than document OCR.

4. **Clean namespace**: `Integration.Audio.Transcribe` sits alongside `Integration.Ocr.Ai` without polluting it. Future audio integrations (classification, speaker diarization) can live under `Integration.Audio.*`.

5. **Testable pure functions**: `buildSystemPrompt` and `buildTranscriptionPrompt` are exported from Internal for unit testing, following the same pattern as OCR.

### Negative

1. **API costs per request**: Audio transcription via multimodal models is typically more expensive than text/image processing due to larger input sizes.

2. **Model support varies**: Not all OpenRouter models support audio input. Jess must choose a model that supports audio (e.g., Gemini, GPT-4o). An unsupported model will return an API error through the `onError` callback.

3. **No streaming or chunking support**: The entire audio file is base64-encoded and sent in a single request. Very large files (>25MB) may exceed model input limits. A future enhancement could split large audio files into chunks and transcribe them in parallel (see [#457](https://github.com/neohaskell/NeoHaskell/issues/457)).

4. **AI hallucination risk**: Models may fabricate words or phrases not present in the audio. This is inherent to AI-based transcription and cannot be eliminated at the integration level.

### Risks

1. **Memory pressure from large audio files**: Audio files can be significantly larger than documents. A 50MB WAV file produces ~67MB of base64. Mitigate with `maxDurationSeconds` config, document file size recommendations, and future chunked transcription support ([#457](https://github.com/neohaskell/NeoHaskell/issues/457)).

2. **Model input size limits**: OpenRouter models have varying input size limits. A very large audio file may be rejected by the model. The `onError` callback handles this gracefully.

3. **Transcription quality varies by model**: Different models have different strengths for audio transcription. Gemini models currently have the best audio support on OpenRouter.

### Mitigations

1. **Document recommended file sizes**: Integration documentation should recommend keeping audio files under 25MB and using compressed formats (MP3, M4A) over uncompressed (WAV) for large recordings.

2. **`maxDurationSeconds` config**: Allows Jess to cap processing time and cost for long recordings.

3. **`language` hint**: Improves transcription accuracy when the spoken language is known in advance, reducing hallucination risk.

4. **Future: chunked transcription**: For audio files exceeding model input limits, a future enhancement ([#457](https://github.com/neohaskell/NeoHaskell/issues/457)) will split large files into overlapping chunks, transcribe each chunk independently, and stitch the results together. This is out of scope for v1 but the module design does not preclude it.

## References

- [#451: Audio Transcription Integration](https://github.com/neohaskell/NeoHaskell/issues/451)
- [ADR-0023: AI-Powered OCR via Multimodal Models](0023-ai-pdf-transcription.md) — Template integration this ADR mirrors
- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — Two-persona model (Jess/Nick)
- [ADR-0015: HTTP Outbound Integration](0015-http-outbound-integration.md) — HTTP integration that OpenRouter piggybacks on
- [Integration.Ocr.Ai](../../integrations/Integration/Ocr/Ai.hs) — OCR module (structural template)
- [Integration.Ocr.Ai.Internal](../../integrations/Integration/Ocr/Ai/Internal.hs) — OCR implementation (structural template)
- [Integration.OpenRouter.Message](../../integrations/Integration/OpenRouter/Message.hs) — Multimodal Message type (`userWithAttachment`)
- [Integration.hs](../../core/service/Integration.hs) — ActionContext and FileAccessContext definitions
- [Bytes.hs](../../core/core/Bytes.hs) — Base64 encoding utility
