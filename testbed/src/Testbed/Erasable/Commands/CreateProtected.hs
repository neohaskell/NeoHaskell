{-| Create one protected testbed entity through the ordinary command executor.
The fixed value keeps the testbed request free of personal input while exercising encryption.
-}
module Testbed.Erasable.Commands.CreateProtected (
  CreateProtected (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Erasable qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Erasable.Core


data CreateProtected = CreateProtected


getEntityId :: CreateProtected -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateProtected -> Maybe ProtectedEntity -> RequestContext -> Decision ProtectedEvent
decide _ maybeEntity _ = case maybeEntity of
  Just _ -> Decider.reject "Protected entity already exists"
  Nothing -> do
    protectedId <- Decider.generateUuid
    let payload = SecretPayload
          { name = "Álvaro ñ 😀",
            aliases = [Nothing, Just "café", Just "東京"]
          }
    first <- Erasable.new payload
    second <- Erasable.new payload
    mapped <- Erasable.map identity first
    Decider.acceptNew
      [ ProtectedCreated
          { createdId = protectedId,
            createdFirst = first,
            createdSecond = second,
            createdMapped = mapped
          }
      ]


type instance EntityOf CreateProtected = ProtectedEntity
type instance TransportsOf CreateProtected = '[WebTransport]


command ''CreateProtected
