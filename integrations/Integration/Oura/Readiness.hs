module Integration.Oura.Readiness
  ( DailyReadiness (..)
  )
where

import Array (Array)
import Integration.Oura.Types (ReadinessData)
import Maybe (Maybe)
import Text (Text)


data DailyReadiness command = DailyReadiness
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array ReadinessData -> command
  , onError :: Maybe (Text -> command)
  }
