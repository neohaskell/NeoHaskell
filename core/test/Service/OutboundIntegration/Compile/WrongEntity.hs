-- | Reject a handler whose entity argument conflicts with its type equation.
-- The standalone compiler check verifies the expected-entity diagnostic.
module Service.OutboundIntegration.Compile.WrongEntity where

import Core
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)


data Target = Target


data Other = Other


data TargetEvent = TargetEvent


data Added = Added


type instance EventOf Target = TargetEvent


data Handler = Handler


type instance EntityOf Handler = Target


handleEvent :: Other -> Added -> Integration.Outbound
handleEvent _ _ = Integration.none


outboundIntegration ''Handler
