module Integration.Oura.DailyStress
  ( DailyStress (..)
  )
where

import Array (Array)
import Integration.Oura.Types (DailyStressData)
import Maybe (Maybe)
import Text (Text)


data DailyStress command = DailyStress
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array DailyStressData -> command
  , onError :: Maybe (Text -> command)
  }
