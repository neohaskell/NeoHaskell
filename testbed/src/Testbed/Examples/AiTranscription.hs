-- | Example AI-powered PDF transcription integration for testbed.
--
-- This module demonstrates how Jess (Integration User) would use the
-- AI transcription integration to extract text from uploaded PDF files
-- using multimodal AI models (Gemini, Claude Vision, GPT-4V) via OpenRouter.
--
-- == Usage Pattern
--
-- Jess configures AI transcription using pure records:
--
-- @
-- Integration.outbound AiTranscribe.Request
--   { fileRef = e.file
--   , model = "google\/gemini-pro-1.5"
--   , config = AiTranscribe.defaultConfig
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
import Integration.Ai.TranscribePdf qualified as AiTranscribe
import Integration.Ai.TranscribePdf.Internal ()  -- ToAction instance
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
fullTextTranscription documentId fileRef =
  Integration.outbound AiTranscribe.Request
    { fileRef = fileRef
    , model = "google/gemini-pro-1.5"
    , config = AiTranscribe.defaultConfig
    , onSuccess = \result ->
        TranscriptionCompleted
          { documentId = documentId
          , transcribedText = result.text
          , pageCount = result.pageCount
          }
    , onError = \err ->
        TranscriptionFailed
          { documentId = documentId
          , errorMessage = err
          }
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
documentSummary documentId language fileRef =
  Integration.outbound AiTranscribe.Request
    { fileRef = fileRef
    , model = "anthropic/claude-3.5-sonnet"
    , config = AiTranscribe.defaultConfig
        { AiTranscribe.extractionMode = AiTranscribe.Summary
        , AiTranscribe.language = Just language
        , AiTranscribe.maxPages = Just 50  -- Limit for very long documents
        }
    , onSuccess = \result ->
        TranscriptionCompleted
          { documentId = documentId
          , transcribedText = result.text
          , pageCount = result.pageCount
          }
    , onError = \err ->
        TranscriptionFailed
          { documentId = documentId
          , errorMessage = err
          }
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
structuredExtraction documentId fileRef =
  Integration.outbound AiTranscribe.Request
    { fileRef = fileRef
    , model = "openai/gpt-4o"
    , config = AiTranscribe.defaultConfig
        { AiTranscribe.extractionMode = AiTranscribe.Structured
        , AiTranscribe.systemPrompt = Just
            "Extract invoice data as JSON with fields: invoice_number, date, vendor, line_items (array of {description, quantity, unit_price, total}), subtotal, tax, total."
        , AiTranscribe.timeoutSeconds = 180  -- Longer for complex extraction
        }
    , onSuccess = \result ->
        TranscriptionCompleted
          { documentId = documentId
          , transcribedText = result.text
          , pageCount = result.pageCount
          }
    , onError = \err ->
        TranscriptionFailed
          { documentId = documentId
          , errorMessage = err
          }
    }
