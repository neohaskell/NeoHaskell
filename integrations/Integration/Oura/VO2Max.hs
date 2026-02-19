module Integration.Oura.VO2Max
  ( VO2Max (..)
  )
where

import Array (Array)
import Integration.Oura.Types (VO2MaxData)
import Maybe (Maybe)
import Text (Text)


data VO2Max command = VO2Max
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array VO2MaxData -> command
  , onError :: Maybe (Text -> command)
  }
