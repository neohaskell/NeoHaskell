-- | Response types for Brevo transactional email integration.
--
-- Provides the 'Response' record decoded from Brevo's HTTP 201 response.
module Integration.Brevo.Response
  ( Response (..)
  ) where

import Basics
import Json qualified
import Text (Text)


-- | Brevo's success response for a single email send.
data Response = Response
  { messageId :: Text
  } deriving (Eq, Show, Generic)


instance Json.ToJSON Response
instance Json.FromJSON Response
