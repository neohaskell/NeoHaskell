module Integration.Oura.Activity
  ( DailyActivity (..)
  )
where

import Array (Array)
import Integration.Oura.Types (ActivityData)
import Maybe (Maybe)
import Text (Text)


data DailyActivity command = DailyActivity
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array ActivityData -> command
  , onError :: Maybe (Text -> command)
  }
