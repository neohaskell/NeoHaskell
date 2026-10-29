{-| Create a protected testbed record owned by the authenticated user.
The command uses two fresh references so erasing the first leaves the second available.
-}
module Testbed.Erasable.Owned.Commands.Create (
  CreateOwnedProtected (..),
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
import Testbed.Erasable.Core (SecretPayload (..))
import Testbed.Erasable.Owned.Core


data CreateOwnedProtected = CreateOwnedProtected


getEntityId :: CreateOwnedProtected -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateOwnedProtected -> Maybe OwnedProtectedEntity -> RequestContext -> Decision OwnedProtectedEvent
decide _ maybeEntity context = case (maybeEntity, context.user) of
  (Just _, _) -> Decider.reject "Protected record already exists"
  (_, Nothing) -> Decider.reject "Authentication required"
  (Nothing, Just user) -> do
    caseId <- Decider.generateUuid
    first <- Erasable.new SecretPayload {name = "Álvaro ñ 😀", aliases = [Nothing, Just "café"]}
    second <- Erasable.new SecretPayload {name = "Álvaro ñ 😀", aliases = [Nothing, Just "東京"]}
    Decider.acceptNew
      [OwnedProtectedCreated {entityId = caseId, owner = user.sub, caseStatus = "open", first, second}]


type instance EntityOf CreateOwnedProtected = OwnedProtectedEntity
type instance TransportsOf CreateOwnedProtected = '[WebTransport]


command ''CreateOwnedProtected
