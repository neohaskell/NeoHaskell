{-| Project the protected entity into an authorized testbed query.
The query retains the wrapper so response rendering reads current key state.
-}
module Testbed.Erasable.Query (
  ProtectedView (..),
  canAccess,
  canView,
) where

import Core
import Erasable (Erasable)
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.Erasable.Core


data ProtectedView = ProtectedView
  { protectedId :: Uuid,
    firstSecret :: Maybe (Erasable SecretPayload),
    secondSecret :: Maybe (Erasable SecretPayload),
    mappedSecret :: Maybe (Erasable SecretPayload)
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = AccessControl.publicAccess claims


canView :: Maybe UserClaims -> ProtectedView -> Maybe AccessError
canView claims view = AccessControl.publicView claims view


deriveQuery ''ProtectedView [''ProtectedEntity]


instance QueryOf ProtectedEntity ProtectedView where
  queryId entity = entity.protectedId
  combine entity _ = Update ProtectedView
    { protectedId = entity.protectedId,
      firstSecret = entity.firstSecret,
      secondSecret = entity.secondSecret,
      mappedSecret = entity.mappedSecret
    }
