module Integration.Oura.PersonalInfo
  ( PersonalInfo (..)
  )
where

import Integration.Oura.Types (PersonalInfoData)
import Maybe (Maybe)
import Text (Text)


-- Note: PersonalInfo is the only non-paginated endpoint
-- It returns a single object, not an array
-- It has no date range parameters
data PersonalInfo command = PersonalInfo
  { userId :: Text
  , onSuccess :: PersonalInfoData -> command
  , onError :: Maybe (Text -> command)
  }
