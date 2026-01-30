-- | # PDF Integration
--
-- This module provides PDF processing integrations for NeoHaskell.
--
-- == Available Integrations
--
-- * "Integration.Pdf.ExtractText" - Local CLI-based text extraction
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
-- The text extraction integration requires poppler-utils:
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
module Integration.Pdf
  ( -- * Text Extraction
    module Integration.Pdf.ExtractText
  ) where

import Integration.Pdf.ExtractText
