-- | Naming convention utilities for configuration fields.
--
-- This module provides functions to convert field names from camelCase
-- to SCREAMING_SNAKE_CASE for environment variable names.
module Config.Naming (
  -- * Naming Conversions
  toScreamingSnake,
  toEnvVarName,
) where

import Core
import Text qualified


-- | Convert camelCase to SCREAMING_SNAKE_CASE.
--
-- This is the standard convention for environment variable names.
--
-- ==== __Examples__
--
-- >>> toScreamingSnake "port"
-- "PORT"
--
-- >>> toScreamingSnake "databaseUrl"
-- "DATABASE_URL"
--
-- >>> toScreamingSnake "openRouterKey"
-- "OPEN_ROUTER_KEY"
--
-- >>> toScreamingSnake "enableFeatureX"
-- "ENABLE_FEATURE_X"
--
-- >>> toScreamingSnake "httpURL"
-- "HTTP_URL"
--
-- >>> toScreamingSnake "oauth2"
-- "OAUTH2"
--
-- >>> toScreamingSnake "x"
-- "X"
toScreamingSnake :: Text -> Text
toScreamingSnake fieldName =
  fieldName
    |> Text.toSnakeCase
    |> Text.toUpper


-- | Generate the default environment variable name for a field.
--
-- This is an alias for 'toScreamingSnake', used to make the intent
-- clearer when generating env var names from field names.
--
-- ==== __Examples__
--
-- >>> toEnvVarName "databaseUrl"
-- "DATABASE_URL"
toEnvVarName :: Text -> Text
toEnvVarName = toScreamingSnake
