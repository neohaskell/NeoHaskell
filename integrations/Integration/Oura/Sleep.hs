module Integration.Oura.Sleep
  ( DailySleep (..)
  )
where

import Array (Array)
import Integration.Oura.Types (SleepData)
import Maybe (Maybe)
import Text (Text)


data DailySleep command = DailySleep
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array SleepData -> command
  , onError :: Maybe (Text -> command)
  }
