{-| Run one scoped erasure action for each application request event.
The action returns a result command; the request stays pending until that command commits.
-}
module Testbed.Erasable.Owned.Outbound (
  OwnedErasureOutbound (..),
  handleEvent,
) where

import Core
import AsyncTask qualified
import Environment qualified
import File qualified
import Integration qualified
import Integration.Erasable qualified as Erasure
import Path qualified
import Service.Event.EntityName (EntityName (..))
import Service.OutboundIntegration.TH (outboundIntegrationFullEvent)
import Task qualified
import Testbed.Erasable.Owned.Commands.Record (RecordErasureResult (..))
import Testbed.Erasable.Owned.Core


data OwnedErasureOutbound = OwnedErasureOutbound
  deriving (Generic)


type instance EntityOf OwnedErasureOutbound = OwnedProtectedEntity


handleEvent :: OwnedProtectedEntity -> OwnedProtectedEvent -> Integration.Outbound
handleEvent _ event = case event of
  ErasureRequested {entityId, reference, rejectResult, simulateUnknownReference, pauseAfterCommit} ->
    Integration.batch [requestAction entityId reference rejectResult simulateUnknownReference pauseAfterCommit]
  _ -> Integration.none


requestAction :: Uuid -> Text -> Bool -> Bool -> Bool -> Integration.Action
requestAction entityId reference rejectResult simulateUnknownReference pauseAfterCommit =
  if pauseAfterCommit
    then pauseAfterCommittedErase
      (erasureTarget entityId reference simulateUnknownReference)
      reference
      (resultCommand entityId reference rejectResult)
      (failureCommand entityId reference)
    else Integration.outbound Erasure.Erase
      { target = erasureTarget entityId reference simulateUnknownReference,
        onResult = resultCommand entityId reference rejectResult,
        onFailure = failureCommand entityId reference
      }


erasureTarget :: Uuid -> Text -> Bool -> Erasure.ErasureTarget
erasureTarget entityId reference simulateUnknownReference =
  Erasure.ErasureTarget
    { Erasure.reference = if simulateUnknownReference then [fmt|unknown-#{reference}|] else reference,
      Erasure.entityName = EntityName "OwnedProtectedEntity",
      Erasure.streamId = toStreamId entityId
    }


resultCommand :: Uuid -> Text -> Bool -> Erasure.ErasureResult -> RecordErasureResult
resultCommand entityId reference rejectResult result = case result of
  Erasure.ErasedNow -> RecordErasureSuccess {caseId = entityId, reference, wasAlreadyErased = False, rejectResult}
  Erasure.AlreadyErased -> RecordErasureSuccess {caseId = entityId, reference, wasAlreadyErased = True, rejectResult}
  Erasure.UnknownReference -> RecordErasureFailure {caseId = entityId, reference, reason = "unknown reference"}


failureCommand :: Uuid -> Text -> Integration.IntegrationError -> RecordErasureResult
failureCommand entityId reference _ =
  RecordErasureFailure {caseId = entityId, reference, reason = "operation failed"}


pauseAfterCommittedErase :: Erasure.ErasureTarget -> Text -> (Erasure.ErasureResult -> RecordErasureResult) -> (Integration.IntegrationError -> RecordErasureResult) -> Integration.Action
pauseAfterCommittedErase target reference onResult onFailure =
  Integration.action \context -> case context.eraseErasableKey of
    Nothing -> Integration.emitCommand (onFailure (Integration.UnexpectedError "Erasable key store unavailable"))
    Just erase -> do
      outcome <- erase target |> Task.asResult
      case outcome of
        Err failure -> Integration.emitCommand (onFailure failure)
        Ok Erasure.ErasedNow -> do
          marker <- Environment.getVariable "CRYPTOERASURE_COMMIT_MARKER" |> Task.mapError Integration.UnexpectedError
          path <- case Path.fromText marker of
            Nothing -> Task.throw (Integration.UnexpectedError "Invalid erasure marker path")
            Just value -> Task.yield value
          File.writeText path reference |> Task.mapError (\_ -> Integration.UnexpectedError "Erasure marker could not be written")
          AsyncTask.sleep 60000
          Task.throw (Integration.UnexpectedError "Expected testbed process to stop before result callback")
        Ok result -> Integration.emitCommand (onResult result)


outboundIntegrationFullEvent ''OwnedErasureOutbound
