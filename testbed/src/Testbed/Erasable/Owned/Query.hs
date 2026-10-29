{-| Show an owner the protected values, first reference, and request state.
Only the owner's authenticated query response exposes the reference for an erasure request.
-}
module Testbed.Erasable.Owned.Query (
  OwnedProtectedView (..),
  canAccess,
  canView,
) where

import Core
import Erasable (Erasable)
import Erasable qualified
import Maybe qualified
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.Erasable.Core (SecretPayload)
import Testbed.Erasable.Owned.Core


data OwnedProtectedView = OwnedProtectedView
  { caseId :: Uuid,
    ownerId :: Text,
    caseStatus :: Text,
    firstReference :: Maybe Text,
    firstSecret :: Maybe (Erasable SecretPayload),
    secondSecret :: Maybe (Erasable SecretPayload),
    staleSecret :: Maybe (Erasable SecretPayload),
    erasureStatus :: Text,
    wasAlreadyErased :: Maybe Bool,
    failureReason :: Maybe Text
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = AccessControl.authenticatedAccess claims


canView :: Maybe UserClaims -> OwnedProtectedView -> Maybe AccessError
canView claims view = AccessControl.ownerOnly (\owned -> owned.ownerId) claims view


deriveQuery ''OwnedProtectedView [''OwnedProtectedEntity]


instance QueryOf OwnedProtectedEntity OwnedProtectedView where
  queryId entity = entity.caseId
  combine entity _ = do
    let reference = entity.firstSecret |> Maybe.map Erasable.reference
    let (status, already, failure) = case entity.erasureState of
          NotRequested -> ("not-requested", Nothing, Nothing)
          Pending -> ("pending", Nothing, Nothing)
          Failed reason -> ("failed", Nothing, Just reason)
          Completed wasAlreadyErased -> ("completed", Just wasAlreadyErased, Nothing)
    Update OwnedProtectedView
      { caseId = entity.caseId,
        ownerId = entity.ownerId,
        caseStatus = entity.caseStatus,
        firstReference = reference,
        firstSecret = entity.firstSecret,
        secondSecret = entity.secondSecret,
        staleSecret = entity.staleSecret,
        erasureStatus = status,
        wasAlreadyErased = already,
        failureReason = failure
      }
