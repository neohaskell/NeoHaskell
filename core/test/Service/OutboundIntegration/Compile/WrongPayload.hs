-- | Reject a handler payload with no mapping from the stored event sum.
-- The standalone compiler check matches the marker's repair diagnostic.
module Service.OutboundIntegration.Compile.WrongPayload where

import Core
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)


data Target = Target


data TargetEvent = TargetEvent


data Unmapped = Unmapped


type instance EventOf Target = TargetEvent


data Handler = Handler


type instance EntityOf Handler = Target


handleEvent :: Target -> Unmapped -> Integration.Outbound
handleEvent _ _ = Integration.none


outboundIntegration ''Handler
