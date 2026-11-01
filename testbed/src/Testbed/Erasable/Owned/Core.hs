-- | Keep an application's protected value and erasure request state in one entity.
-- Commands append these events; the query and outbound handler read the reconstructed entity.
module Testbed.Erasable.Owned.Core (
  OwnedProtectedEntity (..),
  OwnedProtectedEvent (..),
  ErasureRequestedPayload (..),
  ErasureState (..),
) where

import Core
import Erasable (Erasable)
import Json qualified
import Service.Command.Core (Event (..))
import Testbed.Erasable.Core (SecretPayload)
import Uuid qualified


data ErasureState = NotRequested | Pending | Failed Text | Completed Bool
  deriving (Eq, Generic, Show)


instance Json.ToJSON ErasureState


instance Json.FromJSON ErasureState


data OwnedProtectedEntity = OwnedProtectedEntity
  { caseId :: Uuid,
    ownerId :: Text,
    caseStatus :: Text,
    firstSecret :: Maybe (Erasable SecretPayload),
    secondSecret :: Maybe (Erasable SecretPayload),
    staleSecret :: Maybe (Erasable SecretPayload),
    erasureState :: ErasureState
  }
  deriving (Generic)


instance Json.ToJSON OwnedProtectedEntity


instance Json.FromJSON OwnedProtectedEntity


type instance NameOf OwnedProtectedEntity = "OwnedProtectedEntity"


instance Default OwnedProtectedEntity where
  def =
    OwnedProtectedEntity
      { caseId = Uuid.nil,
        ownerId = "",
        caseStatus = "",
        firstSecret = Nothing,
        secondSecret = Nothing,
        staleSecret = Nothing,
        erasureState = NotRequested
      }


data OwnedProtectedEvent
  = OwnedProtectedCreated
      { entityId :: Uuid,
        owner :: Text,
        caseStatus :: Text,
        first :: Erasable SecretPayload,
        second :: Erasable SecretPayload
      }
  | ErasureRequested
      { entityId :: Uuid,
        reference :: Text,
        rejectResult :: Bool,
        simulateUnknownReference :: Bool,
        staleFirst :: Erasable SecretPayload
      }
  | ErasureCompleted
      { entityId :: Uuid,
        reference :: Text,
        wasAlreadyErased :: Bool,
        staleFirst :: Erasable SecretPayload
      }
  | ErasureFailed
      { entityId :: Uuid,
        reference :: Text,
        failureReason :: Text
      }
  deriving (Generic, Show)


data ErasureRequestedPayload = ErasureRequestedPayload
  { requestedEntityId :: Uuid,
    requestedReference :: Text,
    requestedRejectResult :: Bool,
    requestedSimulateUnknownReference :: Bool,
    requestedStaleFirst :: Erasable SecretPayload
  }


type instance EntityOf ErasureRequestedPayload = OwnedProtectedEntity


instance EventVariantOf OwnedProtectedEvent ErasureRequestedPayload where
  fromVariant payload =
    ErasureRequested
      { entityId = payload.requestedEntityId,
        reference = payload.requestedReference,
        rejectResult = payload.requestedRejectResult,
        simulateUnknownReference = payload.requestedSimulateUnknownReference,
        staleFirst = payload.requestedStaleFirst
      }
  toVariant eventValue = case eventValue of
    ErasureRequested {entityId, reference, rejectResult, simulateUnknownReference, staleFirst} ->
      Just
        ErasureRequestedPayload
          { requestedEntityId = entityId,
            requestedReference = reference,
            requestedRejectResult = rejectResult,
            requestedSimulateUnknownReference = simulateUnknownReference,
            requestedStaleFirst = staleFirst
          }
    OwnedProtectedCreated {} -> Nothing
    ErasureCompleted {} -> Nothing
    ErasureFailed {} -> Nothing


instance Json.ToJSON OwnedProtectedEvent


instance Json.FromJSON OwnedProtectedEvent


type instance EventOf OwnedProtectedEntity = OwnedProtectedEvent


type instance EntityOf OwnedProtectedEvent = OwnedProtectedEntity


instance Event OwnedProtectedEvent where
  getEventEntityIdImpl event = event.entityId


instance Entity OwnedProtectedEntity where
  initialStateImpl = def
  updateImpl event entity = case event of
    OwnedProtectedCreated {entityId, owner, caseStatus, first, second} ->
      entity
        { caseId = entityId,
          ownerId = owner,
          caseStatus,
          firstSecret = Just first,
          secondSecret = Just second,
          staleSecret = Nothing,
          erasureState = NotRequested
        }
    ErasureRequested {} -> entity {erasureState = Pending}
    ErasureCompleted {wasAlreadyErased, staleFirst} -> entity {erasureState = Completed wasAlreadyErased, staleSecret = Just staleFirst}
    ErasureFailed {failureReason} -> entity {erasureState = Failed failureReason}
