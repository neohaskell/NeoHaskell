-- | # PDF Text Extraction Integration
--
-- This module provides PDF text extraction integration for NeoHaskell,
-- allowing Jess to extract text from uploaded PDF files using local
-- CLI tools (pdftotext, pdfinfo from poppler-utils).
--
-- == Two-Persona Model
--
-- Following ADR-0008 and ADR-0015, this integration separates concerns:
--
-- * **Jess (Integration User)**: Configures extraction with pure records.
--   No @Task@, no subprocess handling, no temp files visible.
--
-- * **Nick (Integration Developer)**: Implements 'ToAction' with CLI execution,
--   temp file management, and error handling in "Integration.Pdf.ExtractText.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Pdf.ExtractText qualified as PdfExtract
--
-- documentIntegrations :: Document -> DocumentEvent -> Integration.Outbound
-- documentIntegrations doc event = case event of
--   PdfUploaded info -> Integration.batch
--     [ Integration.outbound PdfExtract.Request
--         { fileRef = info.fileRef
--         , config = PdfExtract.defaultConfig
--         , onSuccess = \\result -> RecordExtraction
--             { documentId = doc.id
--             , text = result.text
--             , pageCount = result.pageCount
--             }
--         , onError = \\err -> ExtractionFailed
--             { documentId = doc.id
--             , error = err
--             }
--         }
--     ]
--   _ -> Integration.none
-- @
--
-- == Layout Modes
--
-- * 'PreserveLayout': Maintains original positioning (-layout flag)
-- * 'RawText': Just the text, no formatting
-- * 'Table': Optimized for tables (-table flag)
--
-- == Requirements
--
-- This integration requires @pdftotext@ and @pdfinfo@ from poppler-utils:
--
-- @
-- # Ubuntu/Debian
-- apt-get install poppler-utils
--
-- # macOS
-- brew install poppler
--
-- # Nix
-- nix-shell -p poppler_utils
-- @
--
-- == Known Limitations
--
-- * No OCR support (for scanned documents, use AI-powered extraction)
-- * No image extraction
-- * Encrypted PDFs will fail with an error
module Integration.Pdf.ExtractText
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)
  , Layout (..)
  , Encoding (..)

    -- * Result Types
  , ExtractionResult (..)
  , PdfMetadata (..)

    -- * Config Helpers
  , defaultConfig
  ) where

import Basics
import Json qualified
import Maybe (Maybe (..))
import Service.FileUpload.Core (FileRef)
import Text (Text)


-- | Layout modes for text extraction.
--
-- * 'PreserveLayout': Maintains original positioning. Best for documents
--   where spatial layout matters (forms, multi-column text).
--
-- * 'RawText': Extracts just the text content. Best for simple documents
--   where layout doesn't matter.
--
-- * 'Table': Optimized for extracting tabular data. Best for documents
--   with tables.
data Layout
  = PreserveLayout
  -- ^ Maintain original positioning (-layout flag)
  | RawText
  -- ^ Just the text, no formatting
  | Table
  -- ^ Optimize for tables (-table flag)
  deriving (Show, Eq, Generic)

instance Json.ToJSON Layout
instance Json.FromJSON Layout


-- | Output encoding for extracted text.
data Encoding
  = UTF8
  | Latin1
  | ASCII
  deriving (Show, Eq, Generic)

instance Json.ToJSON Encoding
instance Json.FromJSON Encoding


-- | Configuration for PDF text extraction.
--
-- Use 'defaultConfig' for sensible defaults, then override specific fields:
--
-- @
-- PdfExtract.defaultConfig
--   { layout = PdfExtract.Table
--   , pageRange = Just (1, 10)
--   }
-- @
data Config = Config
  { layout :: Layout
  -- ^ How to handle text layout (default: PreserveLayout)
  , pageRange :: Maybe (Int, Int)
  -- ^ (firstPage, lastPage) to extract. Nothing means all pages.
  , encoding :: Encoding
  -- ^ Output text encoding (default: UTF8)
  , timeoutSeconds :: Int
  -- ^ Timeout for extraction (default: 30)
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON Config
instance Json.FromJSON Config


-- | Default configuration for PDF extraction.
--
-- * Layout: PreserveLayout
-- * Page range: All pages
-- * Encoding: UTF8
-- * Timeout: 30 seconds
defaultConfig :: Config
defaultConfig = Config
  { layout = PreserveLayout
  , pageRange = Nothing
  , encoding = UTF8
  , timeoutSeconds = 30
  }


-- | Result of PDF text extraction.
data ExtractionResult = ExtractionResult
  { text :: Text
  -- ^ Extracted text content
  , pageCount :: Int
  -- ^ Number of pages in the PDF
  , metadata :: Maybe PdfMetadata
  -- ^ Optional metadata extracted via pdfinfo
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ExtractionResult
instance Json.FromJSON ExtractionResult


-- | PDF metadata extracted via pdfinfo.
data PdfMetadata = PdfMetadata
  { title :: Maybe Text
  , author :: Maybe Text
  , creationDate :: Maybe Text
  , pageSize :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON PdfMetadata
instance Json.FromJSON PdfMetadata


-- | The main request configuration record that Jess instantiates.
--
-- The @command@ type parameter is the domain command emitted by
-- 'onSuccess' or 'onError' callbacks.
--
-- == Fields
--
-- * 'fileRef': Reference to the uploaded PDF file
-- * 'config': Extraction configuration (use 'defaultConfig')
-- * 'onSuccess': Callback that receives result and returns a domain command
-- * 'onError': Callback for error handling
--
-- == Example
--
-- @
-- Integration.outbound PdfExtract.Request
--   { fileRef = e.uploadedFile
--   , config = PdfExtract.defaultConfig
--       { layout = PdfExtract.Table
--       }
--   , onSuccess = \\result -> RecordText
--       { documentId = doc.id
--       , text = result.text
--       }
--   , onError = \\err -> ExtractionFailed
--       { documentId = doc.id
--       , error = err
--       }
--   }
-- @
data Request command = Request
  { fileRef :: FileRef
  -- ^ Reference to the uploaded PDF file
  , config :: Config
  -- ^ Extraction configuration
  , onSuccess :: ExtractionResult -> command
  -- ^ Callback for successful extraction
  , onError :: Text -> command
  -- ^ Callback for extraction errors
  }
  deriving (Generic)
