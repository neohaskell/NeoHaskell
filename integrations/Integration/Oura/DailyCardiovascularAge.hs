module Integration.Oura.DailyCardiovascularAge
  ( DailyCardiovascularAge (..)
  )
where

import Array (Array)
import Integration.Oura.Types (DailyCardiovascularAgeData)
import Maybe (Maybe)
import Text (Text)


data DailyCardiovascularAge command = DailyCardiovascularAge
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array DailyCardiovascularAgeData -> command
  , onError :: Maybe (Text -> command)
  }
