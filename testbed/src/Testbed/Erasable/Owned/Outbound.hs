{-| Run one scoped erasure action for each application request event.
The action returns a result command; the request stays pending until that command commits.
-}
module Testbed.Erasable.Owned.Outbound (
  OwnedErasureOutbound (..),
  handleEvent,
) where

import Core
import Integration qualified
import Integration.Erasable qualified as Erasure
import Service.Event.EntityName (EntityName (..))
import Service.OutboundIntegration.TH (outboundIntegrationFullEvent)
import Testbed.Erasable.Owned.Commands.Record (RecordErasureResult (..))
import Testbed.Erasable.Owned.Core


data OwnedErasureOutbound = OwnedErasureOutbound
  deriving (Generic)


type instance EntityOf OwnedErasureOutbound = OwnedProtectedEntity


handleEvent :: OwnedProtectedEntity -> OwnedProtectedEvent -> Integration.Outbound
handleEvent _ event = case event of
  ErasureRequested {entityId, reference, rejectResult, simulateUnknownReference} ->
    Integration.batch
      [ Integration.outbound Erasure.Erase
          { target = Erasure.ErasureTarget
              { Erasure.reference = if simulateUnknownReference then [fmt|unknown-#{reference}|] else reference,
                Erasure.entityName = EntityName "OwnedProtectedEntity",
                Erasure.streamId = toStreamId entityId
              },
            onResult = \result -> case result of
              Erasure.ErasedNow -> RecordErasureSuccess {caseId = entityId, reference, wasAlreadyErased = False, rejectResult}
              Erasure.AlreadyErased -> RecordErasureSuccess {caseId = entityId, reference, wasAlreadyErased = True, rejectResult}
              Erasure.UnknownReference -> RecordErasureFailure {caseId = entityId, reference, reason = "unknown reference"},
            onFailure = \_ -> RecordErasureFailure {caseId = entityId, reference, reason = "operation failed"}
          }
      ]
  _ -> Integration.none


outboundIntegrationFullEvent ''OwnedErasureOutbound
