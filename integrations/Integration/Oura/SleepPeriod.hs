module Integration.Oura.SleepPeriod
  ( SleepPeriod (..)
  )
where

import Array (Array)
import Integration.Oura.Types (SleepPeriodData)
import Maybe (Maybe)
import Text (Text)


data SleepPeriod command = SleepPeriod
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array SleepPeriodData -> command
  , onError :: Maybe (Text -> command)
  }
