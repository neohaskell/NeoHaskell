module Integration.Oura.Workout
  ( Workout (..)
  )
where

import Array (Array)
import Integration.Oura.Types (WorkoutData)
import Maybe (Maybe)
import Text (Text)


data Workout command = Workout
  { userId :: Text
  , startDate :: Text
  , endDate :: Text
  , onSuccess :: Array WorkoutData -> command
  , onError :: Maybe (Text -> command)
  }
