-- | Reject a declared command target that differs from its EntityOf relation.
-- The standalone compiler check requires the target mismatch diagnostic.
module Service.InboundIntegration.Compile.WrongTarget where

import Core
import Integration qualified
import Json qualified


data ExpectedEntity = ExpectedEntity


data WrongEntity = WrongEntity


data WrongTargetCommand = WrongTargetCommand deriving (Generic)


instance Json.ToJSON WrongTargetCommand


type instance NameOf WrongTargetCommand = "WrongTargetCommand"


type instance EntityOf WrongTargetCommand = ExpectedEntity


data Handler = Handler


workerImpl :: () -> Integration.TypedInbound '[ '(WrongTargetCommand, WrongEntity)]
workerImpl _ =
  Integration.typedInbound Integration.TypedInboundConfig
    { Integration.run = \emit -> emit WrongTargetCommand
    }


inboundIntegration ''Handler
