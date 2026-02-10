-- | # AI-Powered PDF Transcription Integration
--
-- This module provides AI-powered PDF text extraction using multimodal
-- models (Gemini, Claude Vision, GPT-4V) via the OpenRouter API.
--
-- == Two-Persona Model
--
-- Following ADR-0008 and ADR-0023, this integration separates concerns:
--
-- * __Jess (Integration User)__: Configures extraction with pure records.
--   No @Task@, no API calls, no base64 encoding visible.
--
-- * __Nick (Integration Developer)__: Implements 'ToAction' with file retrieval,
--   base64 encoding, and OpenRouter orchestration in
--   "Integration.Ai.TranscribePdf.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Ai.TranscribePdf qualified as AiTranscribe
--
-- documentIntegrations :: Document -> DocumentEvent -> Integration.Outbound
-- documentIntegrations doc event = case event of
--   PdfUploaded info -> Integration.batch
--     [ Integration.outbound AiTranscribe.Request
--         { fileRef = info.fileRef
--         , model = "google\/gemini-pro-1.5"
--         , config = AiTranscribe.defaultConfig
--         , onSuccess = \\result -> RecordTranscription
--             { documentId = doc.id
--             , text = result.text
--             }
--         , onError = \\err -> TranscriptionFailed
--             { documentId = doc.id
--             , error = err
--             }
--         }
--     ]
--   _ -> Integration.none
-- @
--
-- == Extraction Modes
--
-- * 'FullText': Extract all text faithfully, preserving structure
-- * 'Summary': Summarize document content
-- * 'Structured': Extract as structured JSON
--
-- == Companion Module
--
-- For CLI-based extraction (faster, free, but limited to digital PDFs),
-- see "Integration.Pdf.ExtractText".
--
-- == Data Privacy Notice
--
-- File content is sent to third-party AI providers via OpenRouter.
-- Ensure appropriate data processing agreements are in place.
-- See ADR-0023 for security and privacy considerations.
--
-- == Known Limitations (v1)
--
-- * No streaming support
-- * No confidence scoring (always Nothing)
-- * No page-level extraction (whole document only)
module Integration.Ai.TranscribePdf
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)
  , ExtractionMode (..)

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


-- | Extraction modes for AI-powered transcription.
--
-- * 'FullText': Extract all text faithfully, preserving structure.
--   Best for documents where you need the complete text.
--
-- * 'Summary': Summarize document content.
--   Best for getting a quick overview of long documents.
--
-- * 'Structured': Extract as structured JSON.
--   Best for forms, invoices, and documents with known structure.
data ExtractionMode
  = FullText
  -- ^ Extract all text faithfully, preserving structure
  | Summary
  -- ^ Summarize document content
  | Structured
  -- ^ Extract as structured JSON
  deriving (Show, Eq, Generic)

instance Json.ToJSON ExtractionMode
instance Json.FromJSON ExtractionMode


-- | Configuration for AI transcription.
--
-- Use 'defaultConfig' for sensible defaults, then override specific fields:
--
-- @
-- AiTranscribe.defaultConfig
--   { extractionMode = AiTranscribe.Summary
--   , language = Just "es"
--   }
-- @
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

instance Json.ToJSON Config
instance Json.FromJSON Config


-- | Default configuration for AI transcription.
--
-- * Extraction mode: FullText
-- * Language: None (auto-detect)
-- * Max pages: None (all pages)
-- * System prompt: None (use default)
-- * Timeout: 120 seconds
{-# INLINE defaultConfig #-}
defaultConfig :: Config
defaultConfig = Config
  { extractionMode = FullText
  , language = Nothing
  , maxPages = Nothing
  , systemPrompt = Nothing
  , timeoutSeconds = 120
  }


-- | Result of AI-powered transcription.
--
-- @
-- onSuccess = \\result -> RecordTranscription
--   { text = result.text
--   , pageCount = result.pageCount
--   }
-- @
data TranscriptionResult = TranscriptionResult
  { text :: Text
  -- ^ Extracted\/transcribed text
  , pageCount :: Maybe Int
  -- ^ Number of pages processed (if available from model response)
  , confidence :: Maybe Float
  -- ^ Confidence score (always Nothing for v1)
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
-- * 'fileRef': Reference to the uploaded PDF file
-- * 'model': AI model to use (e.g., @\"google\/gemini-pro-1.5\"@)
-- * 'config': Extraction configuration (use 'defaultConfig')
-- * 'onSuccess': Callback that receives result and returns a domain command
-- * 'onError': Callback for error handling
--
-- == Example
--
-- @
-- Integration.outbound AiTranscribe.Request
--   { fileRef = e.uploadedFile
--   , model = "google\/gemini-pro-1.5"
--   , config = AiTranscribe.defaultConfig
--       { extractionMode = AiTranscribe.Summary
--       }
--   , onSuccess = \\result -> RecordText
--       { documentId = doc.id
--       , text = result.text
--       }
--   , onError = \\err -> TranscriptionFailed
--       { documentId = doc.id
--       , error = err
--       }
--   }
-- @
data Request command = Request
  { fileRef :: FileRef
  -- ^ Reference to the uploaded PDF file
  , model :: Text
  -- ^ AI model to use (e.g., "google\/gemini-pro-1.5")
  , config :: Config
  -- ^ Extraction configuration
  , onSuccess :: TranscriptionResult -> command
  -- ^ Callback for successful transcription
  , onError :: Text -> command
  -- ^ Callback for transcription errors
  }
  deriving (Generic)
