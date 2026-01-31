module Service.Transport.Web.SwaggerUI (
  scalarHtml,
) where

import Basics
import Text (Text)


-- | Generate HTML for Scalar API documentation page.
--
-- Loads Scalar from CDN and configures it to fetch the OpenAPI spec
-- from /openapi.json. Scalar provides a modern, beautiful API reference.
--
-- Example:
--
-- @
-- html <- scalarHtml "My API"
-- -- Returns complete HTML page with Scalar documentation
-- @
scalarHtml :: Text -> Text
scalarHtml _title = do
  [fmt|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{_title} - API Documentation</title>
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
  <script src="https://cdn.jsdelivr.net/npm/@scalar/api-reference"></script>
</body>
</html>|]
