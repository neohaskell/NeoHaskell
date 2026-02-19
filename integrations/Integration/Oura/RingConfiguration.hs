module Integration.Oura.RingConfiguration
  ( RingConfiguration (..)
  )
where

import Array (Array)
import Integration.Oura.Types (RingConfigurationData)
import Maybe (Maybe)
import Text (Text)


data RingConfiguration command = RingConfiguration
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array RingConfigurationData -> command
  , onError :: Maybe (Text -> command)
  }
