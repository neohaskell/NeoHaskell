-- | Reject an emitted command omitted from the typed worker output list.
-- The standalone compiler check requires the undeclared-command diagnostic.
module Service.InboundIntegration.Compile.OmittedCommand where

import Core
import Integration qualified
import Json qualified


data TargetEntity = TargetEntity


data Listed = Listed deriving (Generic)


data Omitted = Omitted deriving (Generic)


instance Json.ToJSON Listed


instance Json.ToJSON Omitted


type instance NameOf Listed = "Listed"


type instance NameOf Omitted = "Omitted"


type instance EntityOf Listed = TargetEntity


type instance EntityOf Omitted = TargetEntity


data Handler = Handler


workerImpl :: () -> Integration.TypedInbound '[ '(Listed, TargetEntity)]
workerImpl _ =
  Integration.typedInbound Integration.TypedInboundConfig
    { Integration.run = \emit -> emit Omitted
    }


inboundIntegration ''Handler
