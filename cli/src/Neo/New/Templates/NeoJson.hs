module Neo.New.Templates.NeoJson where

import Core


template :: Text -> Text
template projectName = do
  [fmt|{{
  "name": "#{projectName}",
  "version": "0.0.1",
  "description": "Your project's description",
  "author": "Your Name",
  "license": "ISC",
  "dependencies": {{
  }}
}}|]
