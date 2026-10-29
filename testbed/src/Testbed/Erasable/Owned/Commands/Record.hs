{-| Record the result of one outbound erasure attempt in the owning entity.
Only the internal integration dispatcher registers this command.
-}
module Testbed.Erasable.Owned.Commands.Record (
  RecordErasureResult (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Erasable (Erasable)
import Erasable qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Internal (InternalTransport)
import Testbed.Erasable.Core (SecretPayload)
import Testbed.Erasable.Owned.Core


data RecordErasureResult
  = RecordErasureSuccess
      { caseId :: Uuid,
        reference :: Text,
        wasAlreadyErased :: Bool,
        rejectResult :: Bool,
        staleFirst :: Erasable SecretPayload
      }
  | RecordErasureFailure
      { caseId :: Uuid,
        reference :: Text,
        reason :: Text
      }
  deriving (Generic)


getEntityId :: RecordErasureResult -> Maybe Uuid
getEntityId command = Just command.caseId


decide :: RecordErasureResult -> Maybe OwnedProtectedEntity -> RequestContext -> Decision OwnedProtectedEvent
decide command maybeEntity _ = case maybeEntity of
  Nothing -> Decider.reject "Erasure request was not found"
  Just entity -> case entity.firstSecret of
    Nothing -> Decider.reject "Protected reference was not found"
    Just first ->
      if command.reference != Erasable.reference first
        then Decider.reject "Protected reference does not belong to this entity"
        else case entity.erasureState of
          Pending -> recordPending command
          Completed _ -> Decider.reject "Erasure result was already recorded"
          Failed _ -> Decider.reject "Erasure request is not pending"
          NotRequested -> Decider.reject "Erasure was not requested"


recordPending :: RecordErasureResult -> Decision OwnedProtectedEvent
recordPending command = case command of
  RecordErasureSuccess {caseId, reference, wasAlreadyErased, rejectResult, staleFirst} ->
    if rejectResult
      then Decider.reject "Testbed rejected the result after key commit"
      else Decider.acceptExisting [ErasureCompleted {entityId = caseId, reference, wasAlreadyErased, staleFirst}]
  RecordErasureFailure {caseId, reference, reason} ->
    Decider.acceptExisting [ErasureFailed {entityId = caseId, reference, failureReason = reason}]


type instance EntityOf RecordErasureResult = OwnedProtectedEntity
type instance TransportsOf RecordErasureResult = '[InternalTransport]


command ''RecordErasureResult
