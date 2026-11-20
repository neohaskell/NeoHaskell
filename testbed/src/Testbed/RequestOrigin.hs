-- | Demonstrate reading Web request-origin metadata without storing its values.
-- Include 'requestOriginFlow' in the application's Event Model.
module Testbed.RequestOrigin (requestOriginFlow) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext (..), RequestOrigin (..))
import Service.Command.Core (Event (..), TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.EventModel qualified as EventModel
import Service.Transport.Web (WebTransport)
import Uuid qualified


data RequestOriginProbeEntity = RequestOriginProbeEntity
  { requestOriginProbeId :: Uuid
  , clientAddressWasPresent :: Bool
  , userAgentWasPresent :: Bool
  }
  deriving (Generic)


instance Json.FromJSON RequestOriginProbeEntity


instance Json.ToJSON RequestOriginProbeEntity


initialState :: RequestOriginProbeEntity
initialState =
  RequestOriginProbeEntity
    { requestOriginProbeId = Uuid.nil
    , clientAddressWasPresent = False
    , userAgentWasPresent = False
    }


type instance NameOf RequestOriginProbeEntity = "RequestOriginProbeEntity"


instance Entity RequestOriginProbeEntity where
  initialStateImpl = initialState
  updateImpl = update


data RequestOriginProbeEvent = RequestOriginObserved
  { entityId :: Uuid
  , clientAddressPresent :: Bool
  , userAgentPresent :: Bool
  }
  deriving (Generic, Show)


type instance EventOf RequestOriginProbeEntity = RequestOriginProbeEvent


type instance EntityOf RequestOriginProbeEvent = RequestOriginProbeEntity


instance Event RequestOriginProbeEvent where
  getEventEntityIdImpl event = event.entityId


instance Json.FromJSON RequestOriginProbeEvent


instance Json.ToJSON RequestOriginProbeEvent


update :: RequestOriginProbeEvent -> RequestOriginProbeEntity -> RequestOriginProbeEntity
update event _entity = case event of
  RequestOriginObserved {entityId, clientAddressPresent, userAgentPresent} ->
    RequestOriginProbeEntity
      { requestOriginProbeId = entityId
      , clientAddressWasPresent = clientAddressPresent
      , userAgentWasPresent = userAgentPresent
      }


data CheckRequestOrigin = CheckRequestOrigin


getEntityId :: CheckRequestOrigin -> Maybe Uuid
getEntityId _ = Nothing


decide :: CheckRequestOrigin -> Maybe RequestOriginProbeEntity -> RequestContext -> Decision RequestOriginProbeEvent
decide _ maybeEntity context = case maybeEntity of
  Just _ -> Decider.reject "Request origin was already checked"
  Nothing -> case context.origin of
    Nothing -> Decider.reject "Web request origin was not attached"
    Just requestOrigin -> case (requestOrigin.clientAddress, requestOrigin.userAgent) of
      (Just "198.51.100.42", Just "neohaskell-request-origin-example") -> do
        probeId <- Decider.generateUuid
        Decider.acceptNew
          [ RequestOriginObserved
              { entityId = probeId
              , clientAddressPresent = True
              , userAgentPresent = True
              }
          ]
      _ -> Decider.reject "Web request origin did not match the synthetic example values"


type instance EntityOf CheckRequestOrigin = RequestOriginProbeEntity


type instance TransportsOf CheckRequestOrigin = '[WebTransport]


command ''CheckRequestOrigin


requestOriginFlow :: EventModel.FeatureAction
requestOriginFlow = EventModel.feature "Request origin" do
  EventModel.chapter "Read Web request metadata" do
    EventModel.slice "Check the Web request origin" do
      EventModel.entity @RequestOriginProbeEntity do
        EventModel.command @CheckRequestOrigin do
          EventModel.produces @RequestOriginProbeEvent
