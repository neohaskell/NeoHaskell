module Service.Transport.Web.SwaggerUI (
  swaggerUiHtml,
) where

import Basics
import Text (Text)


-- | Generate HTML for Swagger UI documentation page.
--
-- Loads Swagger UI assets from CDN and configures it to fetch the OpenAPI spec
-- from /openapi.json.
--
-- Example:
--
-- @
-- html <- swaggerUiHtml "My API"
-- -- Returns complete HTML page with Swagger UI
-- @
swaggerUiHtml :: Text -> Text
swaggerUiHtml apiTitle = do
  [fmt|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{apiTitle} - API Documentation</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css">
  <style>
    body {{
      margin: 0;
      padding: 0;
    }}
  </style>
</head>
<body>
  <div id="swagger-ui"></div>
  <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
  <script>
    window.onload = function() {{
      SwaggerUIBundle({{
        url: "/openapi.json",
        dom_id: '#swagger-ui',
        deepLinking: true,
        presets: [
          SwaggerUIBundle.presets.apis,
          SwaggerUIBundle.SwaggerUIStandalonePreset
        ],
        layout: "BaseLayout"
      }});
    }};
  </script>
</body>
</html>|]
