{-| Copy an erased protected value in the owned testbed record.
Use this command after erasure to check that mapping keeps its reference without reading plaintext.
-}
module Testbed.Erasable.Owned.Commands.MapErased (
  MapErasedSecret (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Erasable qualified
import Service.Auth (RequestContext (..), UserClaims (..))
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Erasable.Core (SecretPayload)
import Testbed.Erasable.Owned.Core


data MapErasedSecret = MapErasedSecret {caseId :: Uuid}
  deriving (Generic)


getEntityId :: MapErasedSecret -> Maybe Uuid
getEntityId command = Just command.caseId


decide :: MapErasedSecret -> Maybe OwnedProtectedEntity -> RequestContext -> Decision OwnedProtectedEvent
decide _ maybeEntity context = case (maybeEntity, context.user) of
  (Nothing, _) -> Decider.reject "Protected record was not found"
  (_, Nothing) -> Decider.reject "Authentication required"
  (Just entity, Just user) ->
    if entity.ownerId != user.sub
      then Decider.reject "Protected record is not owned by this user"
      else case (entity.erasureState, entity.firstSecret) of
        (Completed _, Just first) -> do
          mapped <- Erasable.map unavailable first
          Decider.acceptExisting [ErasedCopyRecorded {entityId = entity.caseId, mappedErased = mapped}]
        _ -> Decider.reject "Protected value has not been erased"


unavailable :: SecretPayload -> SecretPayload
unavailable _ = panic "Erased map evaluated its function"


type instance EntityOf MapErasedSecret = OwnedProtectedEntity
type instance TransportsOf MapErasedSecret = '[WebTransport]


command ''MapErasedSecret
