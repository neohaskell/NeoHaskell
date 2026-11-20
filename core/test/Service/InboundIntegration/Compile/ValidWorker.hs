-- | Compile a typed worker that emits two commands for distinct entities.
-- The standalone compiler check treats successful compilation as acceptance.
module Service.InboundIntegration.Compile.ValidWorker where

import Core
import Integration qualified
import Json qualified


data FirstEntity = FirstEntity


data SecondEntity = SecondEntity


data FirstCommand = FirstCommand deriving (Generic)


data SecondCommand = SecondCommand deriving (Generic)


instance Json.ToJSON FirstCommand


instance Json.ToJSON SecondCommand


type instance NameOf FirstCommand = "FirstCommand"


type instance NameOf SecondCommand = "SecondCommand"


type instance EntityOf FirstCommand = FirstEntity


type instance EntityOf SecondCommand = SecondEntity


data Handler = Handler


workerImpl :: () -> Integration.TypedInbound '[ '(FirstCommand, FirstEntity), '(SecondCommand, SecondEntity)]
workerImpl _ =
  Integration.typedInbound Integration.TypedInboundConfig
    { Integration.run = \emit -> do
        emit FirstCommand
        emit SecondCommand
    }


inboundIntegration ''Handler
