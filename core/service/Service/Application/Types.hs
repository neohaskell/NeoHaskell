module Service.Application.Types (
  ApiInfo (..),
  defaultApiInfo,
) where

import Basics
import Text (Text)


-- | API metadata for OpenAPI spec generation.
data ApiInfo = ApiInfo
  { apiTitle :: Text
  , apiVersion :: Text
  , apiDescription :: Text
  }
  deriving (Show, Eq)


-- | Default API info when none is provided.
defaultApiInfo :: ApiInfo
defaultApiInfo = ApiInfo
  { apiTitle = "API"
  , apiVersion = "1.0.0"
  , apiDescription = ""
  }
