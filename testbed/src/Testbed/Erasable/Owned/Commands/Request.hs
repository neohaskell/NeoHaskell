{-| Record an owner-authorized erasure request or an explicit retry.
The outbound handler acts only after this command appends 'ErasureRequested'.
-}
module Testbed.Erasable.Owned.Commands.Request (
  RequestErasure (..),
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
import Testbed.Erasable.Owned.Core


data RequestErasure
  = RequestFirstErasure
      { caseId :: Uuid,
        reference :: Text,
        rejectResult :: Bool,
        simulateUnknownReference :: Bool
      }
  | RetryFirstErasure
      { caseId :: Uuid
      }
  deriving (Generic)


getEntityId :: RequestErasure -> Maybe Uuid
getEntityId command = Just command.caseId


decide :: RequestErasure -> Maybe OwnedProtectedEntity -> RequestContext -> Decision OwnedProtectedEvent
decide command maybeEntity context = case (maybeEntity, context.user) of
  (Nothing, _) -> Decider.reject "Protected record was not found"
  (_, Nothing) -> Decider.reject "Authentication required"
  (Just entity, Just user) ->
    if entity.ownerId != user.sub
      then Decider.reject "Protected record is not owned by this user"
      else case entity.firstSecret of
        Nothing -> Decider.reject "Protected reference was not found"
        Just first -> decideForReference command entity (Erasable.reference first)


decideForReference :: RequestErasure -> OwnedProtectedEntity -> Text -> Decision OwnedProtectedEvent
decideForReference command entity ownedReference = case command of
  RequestFirstErasure {reference, rejectResult, simulateUnknownReference} ->
    if reference != ownedReference
      then Decider.reject "Protected reference was not found"
      else case entity.erasureState of
        NotRequested -> request entity ownedReference rejectResult simulateUnknownReference
        Failed _ -> request entity ownedReference rejectResult simulateUnknownReference
        Pending -> Decider.reject "Erasure is pending; use the retry command"
        Completed _ -> Decider.reject "Erasure already completed"
  RetryFirstErasure {} -> case entity.erasureState of
    NotRequested -> Decider.reject "No erasure request to retry"
    Pending -> request entity ownedReference False False
    Failed _ -> request entity ownedReference False False
    Completed _ -> request entity ownedReference False False


request :: OwnedProtectedEntity -> Text -> Bool -> Bool -> Decision OwnedProtectedEvent
request entity ownedReference rejectResult simulateUnknownReference =
  Decider.acceptExisting
    [ ErasureRequested
        { entityId = entity.caseId,
          reference = ownedReference,
          rejectResult,
          simulateUnknownReference
        }
    ]


type instance EntityOf RequestErasure = OwnedProtectedEntity
type instance TransportsOf RequestErasure = '[WebTransport]


command ''RequestErasure
