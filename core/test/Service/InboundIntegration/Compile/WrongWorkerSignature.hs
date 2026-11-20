-- | Reject a marker whose worker erases its command outputs too early.
-- The standalone compiler check requires the typed-worker diagnostic.
module Service.InboundIntegration.Compile.WrongWorkerSignature where

import Core
import Integration qualified
import Json qualified
import Task qualified


data Handler = Handler


data CommandValue = CommandValue deriving (Generic)


instance Json.ToJSON CommandValue


type instance NameOf CommandValue = "CommandValue"


workerImpl :: () -> Integration.Inbound
workerImpl _ =
  Integration.inbound @CommandValue Integration.InboundConfig
    { Integration.run = \_ -> Task.yield unit
    }


inboundIntegration ''Handler
