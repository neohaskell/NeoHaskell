module Integration.Oura.SleepTime
  ( SleepTime (..)
  )
where

import Array (Array)
import Integration.Oura.Types (SleepTimeData)
import Maybe (Maybe)
import Text (Text)


data SleepTime command = SleepTime
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array SleepTimeData -> command
  , onError :: Maybe (Text -> command)
  }
