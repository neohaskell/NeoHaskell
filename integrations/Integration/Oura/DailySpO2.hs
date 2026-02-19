module Integration.Oura.DailySpO2
  ( DailySpO2 (..)
  )
where

import Array (Array)
import Integration.Oura.Types (DailySpO2Data)
import Maybe (Maybe)
import Text (Text)


data DailySpO2 command = DailySpO2
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array DailySpO2Data -> command
  , onError :: Maybe (Text -> command)
  }
