module Integration.Oura.DailyResilience
  ( DailyResilience (..)
  )
where

import Array (Array)
import Integration.Oura.Types (DailyResilienceData)
import Maybe (Maybe)
import Text (Text)


data DailyResilience command = DailyResilience
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array DailyResilienceData -> command
  , onError :: Maybe (Text -> command)
  }
