-- | Example PDF text extraction integration for testbed.
--
-- This module demonstrates how Jess (Integration User) would use the
-- PDF extraction integration to extract text from uploaded PDF files.
--
-- == Usage Pattern
--
-- Jess configures PDF extraction using pure records:
--
-- @
-- Integration.outbound PdfExtract.Request
--   { fileRef = e.file
--   , config = PdfExtract.defaultConfig
--       { layout = PdfExtract.PreserveLayout
--       }
--   , onSuccess = \\result -> RecordTranscription { text = result.text }
--   , onError = \\err -> TranscriptionFailed { error = err }
--   }
-- @
--
-- No @Task@, no subprocess handling, no temp files visible to Jess.
--
-- == Note
--
-- This module demonstrates the API design. The actual PDF extraction
-- uses CLI tools (pdftotext, pdfinfo) from poppler-utils under the hood.
-- For scanned documents or complex layouts requiring AI, see the
-- AI-powered transcription integration (future #343).
module Testbed.Examples.PdfExtraction
  ( -- * Example Integration Builders
    documentUploadIntegration
  , batchDocumentExtraction
  , tableExtractionIntegration
    -- * Example Command Types (for demonstration)
  , ExtractionOutcome (..)
  , BatchExtractionOutcome (..)
  ) where

import Core
import Integration qualified
import Integration.Pdf.ExtractText qualified as PdfExtract
import Integration.Pdf.ExtractText.Internal ()  -- ToAction instance
import Json qualified
import Service.FileUpload.Core (FileRef)


-- ============================================================================
-- Example Commands
-- These are simplified commands for demonstration purposes.
-- In a real application, these would be defined in domain modules with
-- proper EntityOf instances and command handlers.
-- ============================================================================

-- | Result of PDF extraction integration.
--
-- Since onSuccess and onError must return the same command type,
-- we use a sum type to represent all possible outcomes.
data ExtractionOutcome
  = TextExtracted
      { documentId :: Text
      , extractedText :: Text
      , pageCount :: Int
      , title :: Maybe Text
      }
  | ExtractionFailed
      { documentId :: Text
      , errorMessage :: Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON ExtractionOutcome
instance Json.FromJSON ExtractionOutcome


-- | Result of batch extraction.
data BatchExtractionOutcome
  = BatchCompleted
      { batchId :: Text
      , successCount :: Int
      }
  | BatchPartialFailure
      { batchId :: Text
      , successCount :: Int
      , failedDocumentIds :: Array Text
      }
  deriving (Generic, Show, Typeable)

instance Json.ToJSON BatchExtractionOutcome
instance Json.FromJSON BatchExtractionOutcome


-- Type-level name instances required by Integration.ToAction
type instance NameOf ExtractionOutcome = "ExtractionOutcome"
type instance NameOf BatchExtractionOutcome = "BatchExtractionOutcome"


-- ============================================================================
-- Example Integrations (Jess's code)
-- ============================================================================

-- | Example: Extract text from an uploaded document.
--
-- This shows the standard usage pattern:
--
-- * Extract text with layout preservation (default)
-- * Include metadata extraction
-- * Handle success and error cases
--
-- @
-- documentIntegrations doc event = case event of
--   DocumentUploaded e -> Integration.batch
--     [ documentUploadIntegration doc.id e.fileRef ]
--   _ -> Integration.none
-- @
documentUploadIntegration ::
  -- | Document ID
  Text ->
  -- | File reference from upload
  FileRef ->
  Integration.Action
documentUploadIntegration documentId fileRef =
  Integration.outbound PdfExtract.Request
    { fileRef = fileRef
    , config = PdfExtract.defaultConfig
        { PdfExtract.layout = PdfExtract.PreserveLayout
        , PdfExtract.pageRange = Nothing  -- All pages
        , PdfExtract.encoding = PdfExtract.UTF8
        }
    , onSuccess = \result ->
        TextExtracted
          { documentId = documentId
          , extractedText = result.text
          , pageCount = result.pageCount
          , title = result.metadata
              |> fmap (.title)
              |> join
          }
    , onError = \err ->
        ExtractionFailed
          { documentId = documentId
          , errorMessage = err
          }
    }


-- | Example: Extract specific pages for preview.
--
-- This shows:
--
-- * Page range selection (first 5 pages only)
-- * Raw text mode (no layout preservation)
-- * Shorter timeout for quick previews
batchDocumentExtraction ::
  -- | Batch ID
  Text ->
  -- | Document ID
  Text ->
  -- | File reference
  FileRef ->
  Integration.Action
batchDocumentExtraction batchId documentId fileRef =
  Integration.outbound PdfExtract.Request
    { fileRef = fileRef
    , config = PdfExtract.defaultConfig
        { PdfExtract.layout = PdfExtract.RawText
        , PdfExtract.pageRange = Just (1, 5)  -- First 5 pages only
        , PdfExtract.timeoutSeconds = 15       -- Quick timeout for batch
        }
    , onSuccess = \_result ->
        BatchCompleted
          { batchId = batchId
          , successCount = 1
          }
    , onError = \_err ->
        BatchPartialFailure
          { batchId = batchId
          , successCount = 0
          , failedDocumentIds = [documentId]
          }
    }


-- | Example: Extract tabular data from a PDF.
--
-- This shows:
--
-- * Table layout mode (optimized for tables)
-- * Full document extraction
-- * Longer timeout for complex processing
tableExtractionIntegration ::
  -- | Document ID
  Text ->
  -- | File reference
  FileRef ->
  Integration.Action
tableExtractionIntegration documentId fileRef =
  Integration.outbound PdfExtract.Request
    { fileRef = fileRef
    , config = PdfExtract.defaultConfig
        { PdfExtract.layout = PdfExtract.Table
        , PdfExtract.pageRange = Nothing
        , PdfExtract.timeoutSeconds = 60  -- Longer for table extraction
        }
    , onSuccess = \result ->
        TextExtracted
          { documentId = documentId
          , extractedText = result.text
          , pageCount = result.pageCount
          , title = Nothing  -- Tables don't typically have titles
          }
    , onError = \err ->
        ExtractionFailed
          { documentId = documentId
          , errorMessage = err
          }
    }
