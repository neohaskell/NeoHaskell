-- | # PDF Integration
--
-- This module provides PDF processing integrations for NeoHaskell.
--
-- == Available Integrations
--
-- * "Integration.Pdf.ExtractText" - Local CLI-based text extraction
--   (fast, free, requires poppler-utils). Re-exported from this module.
-- * "Integration.Ocr.Ai" - AI-powered multimodal transcription
--   (handles scanned docs, OCR, summarization — requires OpenRouter API key).
--   Must be imported separately: @import Integration.Ocr.Ai qualified as OcrAi@
--
-- == Quick Start
--
-- @
-- import Integration.Pdf.ExtractText qualified as PdfExtract
--
-- -- Configure extraction
-- Integration.outbound PdfExtract.Request
--   { fileRef = uploadedFile
--   , config = PdfExtract.defaultConfig
--   , onSuccess = \\result -> RecordText { text = result.text }
--   , onError = \\err -> ExtractionFailed { error = err }
--   }
-- @
--
-- == Requirements
--
-- === Local Text Extraction
--
-- The @Integration.Pdf.ExtractText@ integration requires poppler-utils:
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
-- === AI Transcription
--
-- The @Integration.Ocr.Ai@ integration requires an OpenRouter API key.
-- See <https://openrouter.ai/keys> for API key setup.
module Integration.Pdf
  ( -- * Text Extraction
    module Integration.Pdf.ExtractText
  ) where

import Integration.Pdf.ExtractText
