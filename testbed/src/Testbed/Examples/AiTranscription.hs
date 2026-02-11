-- | Example AI-powered document/image transcription integration for testbed.
--
-- This module demonstrates how Jess (Integration User) would use the
-- AI transcription integration to extract text from uploaded files
-- using multimodal AI models (Gemini, Claude Vision, GPT-4o) via OpenRouter.
--
-- == Usage Pattern
--
-- Jess configures AI transcription using pure records:
--
-- @
-- Integration.outbound OcrAi.Request
--   { fileRef = e.file
--   , mimeType = "application\/pdf"
--   , model = "google\/gemini-pro-1.5"
--   , config = OcrAi.defaultConfig
--   , onSuccess = \\result -> RecordTranscription { text = result.text }
--   , onError = \\err -> TranscriptionFailed { error = err }
--   }
-- @
--
-- No @Task@, no base64 encoding, no API calls visible to Jess.
--
-- == Comparison with CLI Extraction
--
-- For digital PDFs where CLI tools suffice, see "Testbed.Examples.PdfExtraction".
-- AI transcription is best for:
--
-- * Scanned documents requiring OCR
-- * Complex layouts (multi-column, mixed content)
-- * Handwritten notes
-- * Document summarization
-- * Structured data extraction (forms, invoices)
--
-- == Note
--
-- This module demonstrates the API design. The actual transcription
-- sends PDF content to third-party AI providers via OpenRouter.
-- See ADR-0023 for security and privacy considerations.
module Testbed.Examples.AiTranscription
  ( -- * Example Integration Builders
    fullTextTranscription
  , documentSummary
  , structuredExtraction
    -- * Example Command Types (for demonstration)
  , TranscriptionOutcome (..)
  ) where

import Core
import Integration qualified
import Integration.Ocr.Ai qualified as OcrAi
import Integration.Ocr.Ai.Internal ()  -- ToAction instance
import Json qualified
import Service.FileUpload.Core (FileRef)


-- ============================================================================
-- Example Commands
-- These are simplified commands for demonstration purposes.
-- In a real application, these would be defined in domain modules with
-- proper EntityOf instances and command handlers.
-- ============================================================================

-- | Result of AI transcription integration.
--
-- Since onSuccess and onError must return the same command type,
-- we use a sum type to represent all possible outcomes.
data TranscriptionOutcome
  = TranscriptionCompleted
      { documentId :: Text
      , transcribedText :: Text
      , pageCount :: Maybe Int
      }
  | TranscriptionFailed
      { documentId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON TranscriptionOutcome
instance Json.FromJSON TranscriptionOutcome


-- Type-level name instance required by Integration.ToAction
type instance NameOf TranscriptionOutcome = "TranscriptionOutcome"


-- ============================================================================
-- Example Integrations (Jess's code)
-- ============================================================================

-- | Internal helper: build standard transcription callbacks.
--
-- All three example functions share the same success\/error handling pattern.
-- This helper extracts the common callback logic.
mkCallbacks ::
  Text ->
  (OcrAi.TranscriptionResult -> TranscriptionOutcome, Text -> TranscriptionOutcome)
mkCallbacks documentId =
  ( \result ->
      TranscriptionCompleted
        { documentId = documentId
        , transcribedText = result.text
        , pageCount = result.pageCount
        }
  , \err ->
      TranscriptionFailed
        { documentId = documentId
        , errorMessage = err
        }
  )


-- | Example: Full text extraction from a scanned PDF.
--
-- This shows the standard usage pattern:
--
-- * Extract all text faithfully (FullText mode, the default)
-- * Use Gemini Pro 1.5 for multimodal understanding
-- * Handle success and error cases
--
-- @
-- documentIntegrations doc event = case event of
--   PdfUploaded e -> Integration.batch
--     [ fullTextTranscription doc.id e.fileRef ]
--   _ -> Integration.none
-- @
fullTextTranscription ::
  -- | Document ID
  Text ->
  -- | File reference from upload
  FileRef ->
  Integration.Action
fullTextTranscription documentId fileRef = do
  let (onSuccess, onError) = mkCallbacks documentId
  Integration.outbound OcrAi.Request
    { fileRef = fileRef
    , mimeType = "application/pdf"
    , model = "google/gemini-pro-1.5"
    , config = OcrAi.defaultConfig
    , onSuccess = onSuccess
    , onError = onError
    }


-- | Example: Summarize a long document.
--
-- This shows:
--
-- * Summary extraction mode
-- * Language hint for non-English documents
-- * Claude Vision for high-quality summarization
--
-- @
-- reportIntegrations report event = case event of
--   ReportUploaded e -> Integration.batch
--     [ documentSummary report.id "es" e.fileRef ]
--   _ -> Integration.none
-- @
documentSummary ::
  -- | Document ID
  Text ->
  -- | Language hint (e.g., "es" for Spanish)
  Text ->
  -- | File reference from upload
  FileRef ->
  Integration.Action
documentSummary documentId language fileRef = do
  let (onSuccess, onError) = mkCallbacks documentId
  Integration.outbound OcrAi.Request
    { fileRef = fileRef
    , mimeType = "application/pdf"
    , model = "anthropic/claude-3.5-sonnet"
    , config = OcrAi.defaultConfig
        { OcrAi.extractionMode = OcrAi.Summary
        , OcrAi.language = Just language
        , OcrAi.maxPages = Just 50  -- Limit for very long documents
        }
    , onSuccess = onSuccess
    , onError = onError
    }


-- | Example: Extract structured data from an invoice.
--
-- This shows:
--
-- * Structured extraction mode (returns JSON)
-- * Custom system prompt for domain-specific extraction
-- * GPT-4V for structured understanding
--
-- @
-- invoiceIntegrations invoice event = case event of
--   InvoiceUploaded e -> Integration.batch
--     [ structuredExtraction invoice.id e.fileRef ]
--   _ -> Integration.none
-- @
structuredExtraction ::
  -- | Document ID
  Text ->
  -- | File reference from upload
  FileRef ->
  Integration.Action
structuredExtraction documentId fileRef = do
  let (onSuccess, onError) = mkCallbacks documentId
  Integration.outbound OcrAi.Request
    { fileRef = fileRef
    , mimeType = "application/pdf"
    , model = "openai/gpt-4o"
    , config = OcrAi.defaultConfig
        { OcrAi.extractionMode = OcrAi.Structured
        , OcrAi.systemPrompt = Just
            "Extract invoice data as JSON with fields: invoice_number, date, vendor, line_items (array of {description, quantity, unit_price, total}), subtotal, tax, total."
        , OcrAi.timeoutSeconds = 180  -- Longer for complex extraction
        }
    , onSuccess = onSuccess
    , onError = onError
    }
