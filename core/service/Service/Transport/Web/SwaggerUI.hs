module Service.Transport.Web.SwaggerUI (
  scalarHtml,
) where

import Basics
import Text (Text)
import Text qualified


-- | Generate HTML for Scalar API documentation page.
--
-- Loads Scalar from CDN with Subresource Integrity (SRI) verification
-- and configures it to fetch the OpenAPI spec from /openapi.json.
--
-- Security features:
-- - HTML escapes the title to prevent XSS
-- - Uses SRI hash to verify CDN script integrity
-- - Pins specific version to prevent supply chain attacks
--
-- Example:
--
-- @
-- html <- scalarHtml "My API"
-- -- Returns complete HTML page with Scalar documentation
-- @
scalarHtml :: Text -> Text
scalarHtml title = do
  let safeTitle = Text.escapeHtml title
  [fmt|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>#{safeTitle} - API Documentation</title>
  <style>
    body {{
      margin: 0;
      padding: 0;
    }}
  </style>
</head>
<body>
  <script
    id="api-reference"
    data-url="/openapi.json">
  </script>
  <script 
    src="https://cdn.jsdelivr.net/npm/@scalar/api-reference@1.25.0/dist/browser/standalone.min.js"
    integrity="sha384-VPR3QD7bdyIR9EHf6dRrFXz7NO0yJV7/UCVTLQ6e7a0wgzot5WaxvciVrrYOKEpU"
    crossorigin="anonymous">
  </script>
</body>
</html>|]
