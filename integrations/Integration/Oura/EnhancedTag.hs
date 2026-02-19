module Integration.Oura.EnhancedTag
  ( EnhancedTag (..)
  )
where

import Array (Array)
import Integration.Oura.Types (EnhancedTagData)
import Maybe (Maybe)
import Text (Text)


data EnhancedTag command = EnhancedTag
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array EnhancedTagData -> command
  , onError :: Maybe (Text -> command)
  }
