module Neo.Core.ProjectConfiguration (
    ProjectConfiguration (..),
    fromText,
) where

import Core
import Json qualified


data ProjectConfiguration = ProjectConfiguration
    { name :: Text,
      version :: Version,
      description :: Text,
      author :: Text,
      license :: Text,
      dependencies :: Map Text Text
    }
    deriving (Show, Eq, Ord, Generic)


-- | We allow the `ProjectConfiguration` type to be converted from JSON.
instance Json.FromJSON ProjectConfiguration


-- | We allow the `ProjectConfiguration` type to be converted to JSON.
instance Json.ToJSON ProjectConfiguration


-- | The `ProjectConfiguration.fromText` function allows you to convert a JSON
-- `Text` value to a `ProjectConfiguration` value.
--
-- >>> fromText "{\"name\":\"neo\",\"version\":\"0.5.0\",\"description\":\"NeoHaskell's console helper\",\"author\":\"NeoHaskell\",\"license\":\"MIT\"}"
-- Ok (ProjectConfiguration {name = "neo", version = [version|0.5.0|], description = "NeoHaskell's console helper", author = "NeoHaskell", license = "MIT"})
--
-- >>> fromText "some invalid json"
-- Err "Error in $: not enough input"
fromText :: Text -> Result Text ProjectConfiguration
fromText someText = Json.decodeText someText
