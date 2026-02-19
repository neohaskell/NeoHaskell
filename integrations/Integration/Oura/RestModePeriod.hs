module Integration.Oura.RestModePeriod
  ( RestModePeriod (..)
  )
where

import Array (Array)
import Integration.Oura.Types (RestModePeriodData)
import Maybe (Maybe)
import Text (Text)


data RestModePeriod command = RestModePeriod
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array RestModePeriodData -> command
  , onError :: Maybe (Text -> command)
  }
