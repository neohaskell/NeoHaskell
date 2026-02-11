# ADR-0023: AI-Powered OCR via Multimodal Models

## Status

Proposed

## Context

NeoHaskell has `Integration.Pdf.ExtractText` for CLI-based PDF text extraction using poppler-utils (`pdftotext`, `pdfinfo`). While effective for well-structured digital PDFs, this approach fails for:

1. **Scanned documents and images**: CLI extraction returns empty text since there is no embedded text layer.
2. **Complex layouts**: Multi-column documents, forms, and mixed content produce garbled output.
3. **Handwritten notes**: No text to extract — requires visual understanding.
4. **Summarization and structured extraction**: CLI tools extract raw text but cannot interpret, summarize, or restructure content.

Multimodal AI models (Google Gemini, Anthropic Claude Vision, OpenAI GPT-4V) can process documents and images visually, extracting text with understanding of layout, context, and even handwriting. NeoHaskell already has an `Integration.OpenRouter` module that provides access to 300+ models including multimodal ones.

### Current State

- `Integration.Pdf.ExtractText` — CLI-based extraction (poppler-utils), works for digital PDFs
- `Integration.OpenRouter` — AI model access via OpenRouter API, currently text-only
- `Integration.OpenRouter.Message` — Conversation messages with `content :: Text` (no multimodal support)
- `ActionContext.fileAccess :: Maybe FileAccessContext` — Already exists in `core/service/Integration.hs`
- `FileAccessContext.retrieveFile :: FileRef -> Task FileAccessError Bytes` — Already exists
- `Bytes.toBase64` in `core/core/Bytes.hs` — Base64 encoding utility

### GitHub Issues

- [#343: Add AI-powered PDF transcription integration (multimodal)](https://github.com/neohaskell/NeoHaskell/issues/343) — Original feature request
- [#388: Rename Ai.TranscribePdf to Ocr.Ai and generalize for images](https://github.com/neohaskell/NeoHaskell/issues/388) — Module rename and generalization

## Decision

### 1. Module Placement

Two new modules in `nhintegrations`, following the Jess/Nick two-persona pattern:

| Module | Persona | Purpose |
|--------|---------|---------|
| `Integration.Ocr.Ai` | Jess (User) | Config records, types, `defaultConfig` |
| `Integration.Ocr.Ai.Internal` | Nick (Developer) | `ToAction` instance, orchestration logic |

Placed under `Integration.Ocr.` namespace to distinguish from the CLI-based `Integration.Pdf.` namespace.

**Note**: Originally named `Integration.Ai.TranscribePdf` in [#343](https://github.com/neohaskell/NeoHaskell/issues/343), renamed to `Integration.Ocr.Ai` in [#388](https://github.com/neohaskell/NeoHaskell/issues/388) to reflect generalization beyond PDFs to include images.

### 2. Type Definitions

All types use NeoHaskell style: descriptive type parameters, `Result` not `Either`, `Task` not `IO`.

#### Extraction Modes

```haskell
-- | Modes for AI-powered text extraction from documents and images.
data ExtractionMode
  = FullText
  -- ^ Extract all text faithfully, preserving structure
  | Summary
  -- ^ Summarize document/image content
  | Structured
  -- ^ Extract as structured JSON matching a provided schema
  deriving (Show, Eq, Generic)
```

#### Configuration

```haskell
-- | Configuration for AI transcription.
data Config = Config
  { extractionMode :: ExtractionMode
  -- ^ How to extract text (default: FullText)
  , language :: Maybe Text
  -- ^ Language hint for the AI model (e.g., "en", "es")
  , maxPages :: Maybe Int
  -- ^ Limit number of pages to process (Nothing = all)
  , systemPrompt :: Maybe Text
  -- ^ Override the default system prompt
  , timeoutSeconds :: Int
  -- ^ Timeout for the API call (default: 120)
  }
  deriving (Show, Eq, Generic)
```

#### Request Record (Jess's API)

```haskell
-- | Main request record that Jess instantiates.
-- Follows the same pattern as Pdf.ExtractText.Request.
data Request command = Request
  { fileRef :: FileRef
  -- ^ Reference to the uploaded document/image file
  , mimeType :: Text
  -- ^ MIME type of the file (e.g., "image/jpeg", "application/pdf")
  , model :: Text
  -- ^ AI model to use (e.g., "google/gemini-pro-1.5")
  , config :: Config
  -- ^ Extraction configuration
  , onSuccess :: TranscriptionResult -> command
  -- ^ Callback for successful transcription
  , onError :: Text -> command
  -- ^ Callback for transcription errors
  }
  deriving (Generic)
```

#### Result Type

```haskell
-- | Result of AI-powered transcription.
data TranscriptionResult = TranscriptionResult
  { text :: Text
  -- ^ Extracted/transcribed text
  , pageCount :: Maybe Int
  -- ^ Number of pages processed (if available)
  , confidence :: Maybe Float
  -- ^ Confidence score (always Nothing for v1, Float is Double-precision in NeoHaskell)
  }
  deriving (Show, Eq, Generic)
```

#### Message Extension (in OpenRouter.Message)

To support multimodal content, the `Message` type's `content` field changes from `Text` to a `Content` sum type:

```haskell
-- | Message content — either plain text or multimodal content array.
data Content
  = TextContent Text
  -- ^ Plain text content (backward compatible)
  | MultiContent (Array ContentPart)
  -- ^ Array of content parts (text + images/attachments)
  deriving (Show, Eq, Generic)

-- | A single part within multimodal content.
data ContentPart
  = TextPart Text
  -- ^ Text content part
  | ImageUrlPart ImageUrl
  -- ^ Image/attachment content part
  deriving (Show, Eq, Generic)

-- | Image URL reference for multimodal content.
data ImageUrl = ImageUrl
  { url :: Text
  -- ^ Data URL (e.g., "data:application/pdf;base64,...")
  }
  deriving (Show, Eq, Generic)
```

### 3. Architecture: Piggyback on OpenRouter

The integration follows the piggyback pattern used by `Pdf.ExtractText.Internal`:

```text
OcrAi.Request
  → ToAction.toAction
    → Integration.action \ctx ->
      → ctx.fileAccess.retrieveFile fileRef    -- Get document/image bytes
      → Bytes.toBase64 fileBytes               -- Encode to base64
      → Message.userWithAttachment prompt base64 config.mimeType  -- Build multimodal message
      → OpenRouter.chatCompletion messages model onSuccess onError
        → OpenRouter.toHttpRequest             -- Transform to HTTP
        → Integration.outbound httpRequest     -- Execute via HTTP integration
      → Parse response → TranscriptionResult
      → Integration.emitCommand (config.onSuccess result)
```

Key design points:

- **Uses `Integration.action`**: Because file retrieval requires IO context (like `Pdf.ExtractText.Internal`), NOT the pure transform pattern
- **No direct HTTP calls**: Piggybacks on the existing OpenRouter → Http pipeline
- **No changes to core**: `ActionContext.fileAccess` and `FileAccessContext` already exist and are sufficient

### 4. Message Backward Compatibility

**Critical**: The Message JSON serialization MUST remain backward-compatible.

| Content Type | JSON Output |
|-------------|-------------|
| `TextContent "hello"` | `"content": "hello"` (string — same as today) |
| `MultiContent [TextPart "prompt", ImageUrlPart ...]` | `"content": [{"type":"text","text":"prompt"}, {"type":"image_url","image_url":{"url":"data:..."}}]` (array) |

- `ToJSON`: `TextContent` serializes as a plain JSON string, `MultiContent` as an array
- `FromJSON`: Accepts BOTH formats — string parses to `TextContent`, array parses to `MultiContent`
- Existing `Message.user "hello"` produces identical JSON to today's implementation

### 5. Smart Constructors and Defaults

```haskell
-- | Default configuration for AI transcription.
-- Mode: FullText, Timeout: 120 seconds
defaultConfig :: Config

-- | Create a multimodal user message with an attachment.
userWithAttachment :: Text -> Text -> Text -> Message
-- userWithAttachment prompt base64Content mimeType
```

### 6. Instance Decisions

| Instance | Decision | Rationale |
|----------|----------|-----------|
| `Generic` | **Yes** (derived) | Standard for all NeoHaskell types |
| `Show` | **Yes** (derived) | Debugging |
| `Eq` | **Yes** (derived) | Testing |
| `ToJSON`/`FromJSON` | **Custom** for Content, ContentPart, ImageUrl | OpenRouter API format compliance |
| `ToJSON`/`FromJSON` | **Derived** for ExtractionMode, Config, TranscriptionResult | Standard serialization |
| `ToAction` | **Custom** in Internal | Orchestration logic |

### 7. Performance Considerations

- **Strict fields**: All new types (`Config`, `TranscriptionResult`, `Content`, `ContentPart`, `ImageUrl`) should use strict fields to prevent space leaks from large base64 strings.
- **`toEncoding` over `toJSON`**: Custom `ToJSON` instances for `Content`, `ContentPart`, and `ImageUrl` should implement `toEncoding` to write directly to a Builder, avoiding intermediate `Value` construction.
- **INLINE pragmas**: `defaultConfig` and `userWithAttachment` should be marked `{-# INLINE #-}` as small, frequently-called functions.
- **Memory per request**: A 10MB document/image produces ~13.3MB of base64, with peak memory ~50MB per request (original bytes + base64 + Text + JSON buffer). This is acceptable for an async integration but should be documented.
- **Worker pool impact**: Each in-flight transcription holds a dispatcher worker thread for up to 120 seconds. High-volume usage should be rate-limited at the application level.

### 8. Error Handling

`FileAccessError` variants map to `IntegrationError`:

| FileAccessError | IntegrationError | Rationale |
|----------------|-----------------|-----------|
| `FileNotFound` | `ValidationError` | Invalid file reference |
| `NotOwner` | `AuthenticationError` | Access denied |
| `FileExpired` | `ValidationError` | File no longer available |
| `FileIsDeleted` | `ValidationError` | File was deleted |
| `BlobMissing` | `UnexpectedError` | Storage inconsistency |
| `StorageError` | `UnexpectedError` | Infrastructure failure |

No sensitive information (file paths, internal IDs) is exposed in error messages.

## Consequences

### Positive

1. **Handles impossible-for-CLI cases**: Scanned documents, images, handwritten notes, and complex layouts that poppler-utils cannot process.

2. **Reuses existing infrastructure**: No changes to `ActionContext`, `FileAccessContext`, `Application.hs`, or the OpenRouter pipeline.

3. **Backward-compatible Message extension**: Existing text-only OpenRouter usage is completely unaffected.

4. **NeoHaskell style compliant**: Follows the Jess/Nick two-persona model, qualified module design, pipe-friendly API.

5. **Multiple extraction modes**: FullText, Summary, and Structured cover diverse use cases from simple OCR to data extraction.

6. **Generalized beyond PDFs**: Supports images (JPEG, PNG, etc.) in addition to PDFs, enabling broader OCR use cases.

### Negative

1. **API costs per request**: Unlike free CLI extraction, each AI transcription incurs API costs (varies by model and document size).

2. **Network latency**: 120-second default timeout vs <5 seconds for CLI extraction. Long-running requests may impact the integration dispatcher worker pool.

3. **AI hallucination risk**: Models may fabricate text that doesn't exist in the document/image. This is inherent to AI-based extraction and must be documented for users.

4. **External data exposure**: File content (as base64) is sent to third-party AI providers via OpenRouter. Documents and images may contain PII or sensitive data.

### Risks

1. **Memory pressure from base64 encoding**: Base64 encoding increases data size by ~33%. A 10MB document/image becomes ~13.3MB of base64 text. Mitigate with strict fields and document size limits.

2. **Prompt injection via file content**: Document/image content is NOT interpolated into the system prompt (only sent as an attachment), reducing but not eliminating injection risk.

3. **Rate limiting**: OpenRouter may rate-limit large or frequent requests. Users should be aware of provider-specific limits.

4. **Model availability**: Multimodal models may not always be available on OpenRouter. Error handling must gracefully report API failures.

## References

- [GitHub Issue #343](https://github.com/neohaskell/NeoHaskell/issues/343) — Original feature request
- [GitHub Issue #388](https://github.com/neohaskell/NeoHaskell/issues/388) — Module rename and generalization
- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — Two-persona model (Jess/Nick)
- [ADR-0015: HTTP Outbound Integration](0015-http-outbound-integration.md) — HTTP integration that OpenRouter piggybacks on
- [Integration.Pdf.ExtractText](../../integrations/Integration/Pdf/ExtractText.hs) — Companion CLI-based extraction module
- [Integration.Ocr.Ai](../../integrations/Integration/Ocr/Ai.hs) — AI-powered OCR module (user-facing)
- [Integration.Ocr.Ai.Internal](../../integrations/Integration/Ocr/Ai/Internal.hs) — AI-powered OCR implementation
- [Integration.OpenRouter](../../integrations/Integration/OpenRouter.hs) — AI model access via OpenRouter
- [Integration.OpenRouter.Message](../../integrations/Integration/OpenRouter/Message.hs) — Multimodal Message type
- [Integration.hs](../../core/service/Integration.hs) — ActionContext and FileAccessContext definitions
- [Bytes.hs](../../core/core/Bytes.hs) — Base64 encoding utility
