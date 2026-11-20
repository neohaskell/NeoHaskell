{-| Provide a zero-entity query so neoctl can run as an Event Model app.
The query keeps the service within normal model startup without adding domain data.
-}
module Neoctl.Placeholder (
  Placeholder (..),
  canAccess,
  canView,
) where

import Core
import Service.AccessControl (AccessError, UserClaims, publicAccess, publicView)


data Placeholder = Placeholder
  { purpose :: Text
  }


-- | Allow callers to access neoctl's placeholder query.
--
-- >>> canAccess Nothing
-- Nothing
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess userClaims = publicAccess userClaims


canView :: Maybe UserClaims -> Placeholder -> Maybe AccessError
canView userClaims placeholder = publicView userClaims placeholder


deriveQuery ''Placeholder []
