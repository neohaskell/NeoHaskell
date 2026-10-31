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
import Erasable (Erasable)
import File qualified
import Integration qualified
import Integration.Erasable qualified as Erasure
import Path qualified
import Service.Event.EntityName (EntityName (..))
import Service.OutboundIntegration.TH (outboundIntegrationFullEvent)
import Task qualified
import Testbed.Erasable.Core (SecretPayload)
import Testbed.Erasable.Owned.Commands.Record (RecordErasureResult (..))
import Testbed.Erasable.Owned.Core
import Uuid qualified


data OwnedErasureOutbound = OwnedErasureOutbound
  deriving (Generic)


type instance EntityOf OwnedErasureOutbound = OwnedProtectedEntity


handleEvent :: OwnedProtectedEntity -> OwnedProtectedEvent -> Integration.Outbound
handleEvent _ event = case event of
  ErasureRequested {entityId, reference, rejectResult, simulateUnknownReference, staleFirst} ->
    Integration.batch [requestAction entityId reference rejectResult simulateUnknownReference staleFirst]
  _ -> Integration.none


requestAction :: Uuid -> Text -> Bool -> Bool -> Erasable SecretPayload -> Integration.Action
requestAction entityId reference rejectResult simulateUnknownReference staleFirst =
  Integration.action \context -> do
    pause <- shouldPause "CRYPTOERASURE_PAUSE_SELECTOR" entityId
    pauseBeforeAction <- shouldPause "CRYPTOERASURE_PRE_ACTION_SELECTOR" entityId
    throwResult <- shouldPause "CRYPTOERASURE_CALLBACK_EXCEPTION_SELECTOR" entityId
    omitResult <- shouldPause "CRYPTOERASURE_MISSING_CALLBACK_SELECTOR" entityId
    failAction <- shouldPause "CRYPTOERASURE_ACTION_FAILURE_SELECTOR" entityId
    let target = erasureTarget entityId reference simulateUnknownReference
    let onResult = resultCommand entityId reference rejectResult throwResult staleFirst
    let onFailure = failureCommand entityId reference
    if failAction
      then Task.throw (Integration.UnexpectedError "Testbed erasure action failure")
      else if omitResult
        then Integration.runAction context (omitResultCallback target)
        else if pauseBeforeAction
          then Integration.runAction context (pauseBeforeErase reference)
          else if pause
            then Integration.runAction context (pauseAfterCommittedErase target reference onResult onFailure)
            else Integration.runAction context (Integration.outbound Erasure.Erase {target, onResult, onFailure})


shouldPause :: Text -> Uuid -> Task Integration.IntegrationError Bool
shouldPause selectorVariable entityId = do
  selector <- Environment.getVariable selectorVariable |> Task.asResult
  case selector of
    Err _ -> Task.yield False
    Ok selectorText -> case Path.fromText selectorText of
      Nothing -> Task.yield False
      Just selectorPath -> do
        selected <- File.readText selectorPath |> Task.asResult
        case selected of
          Err _ -> Task.yield False
          Ok selectedId -> Task.yield (selectedId == Uuid.toText entityId)


erasureTarget :: Uuid -> Text -> Bool -> Erasure.ErasureTarget
erasureTarget entityId reference simulateUnknownReference =
  Erasure.ErasureTarget
    { Erasure.reference = if simulateUnknownReference then [fmt|unknown-#{reference}|] else reference,
      Erasure.entityName = EntityName "OwnedProtectedEntity",
      Erasure.streamId = toStreamId entityId
    }


resultCommand :: Uuid -> Text -> Bool -> Bool -> Erasable SecretPayload -> Erasure.ErasureResult -> RecordErasureResult
resultCommand entityId reference rejectResult throwResult staleFirst result = case result of
  Erasure.ErasedNow -> RecordErasureSuccess {caseId = entityId, reference, wasAlreadyErased = False, rejectResult, throwResult, staleFirst}
  Erasure.AlreadyErased -> RecordErasureSuccess {caseId = entityId, reference, wasAlreadyErased = True, rejectResult, throwResult, staleFirst}
  Erasure.UnknownReference -> RecordErasureFailure {caseId = entityId, reference, reason = "unknown reference"}


failureCommand :: Uuid -> Text -> Integration.IntegrationError -> RecordErasureResult
failureCommand entityId reference _ =
  RecordErasureFailure {caseId = entityId, reference, reason = "operation failed"}


omitResultCallback :: Erasure.ErasureTarget -> Integration.Action
omitResultCallback target =
  Integration.action \context -> case context.eraseErasableKey of
    Nothing -> Task.throw (Integration.UnexpectedError "Erasable key store unavailable")
    Just erase -> erase target |> Task.map (\_ -> Nothing)


pauseBeforeErase :: Text -> Integration.Action
pauseBeforeErase reference =
  Integration.action \_ -> do
    marker <- Environment.getVariable "CRYPTOERASURE_PRE_ACTION_MARKER" |> Task.mapError Integration.UnexpectedError
    path <- case Path.fromText marker of
      Nothing -> Task.throw (Integration.UnexpectedError "Invalid pre-action marker path")
      Just value -> Task.yield value
    File.writeText path reference |> Task.mapError (\_ -> Integration.UnexpectedError "Pre-action marker could not be written")
    AsyncTask.sleep 60000
    Task.throw (Integration.UnexpectedError "Expected testbed process to stop before key action")


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
