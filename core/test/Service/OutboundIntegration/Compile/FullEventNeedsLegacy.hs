-- | Reject a full event sum passed through the payload-specific marker.
-- The standalone compiler check verifies the explicit migration hint.
module Service.OutboundIntegration.Compile.FullEventNeedsLegacy where

import Core
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)


data Target = Target


data TargetEvent = TargetEvent


type instance EventOf Target = TargetEvent


data Handler = Handler


type instance EntityOf Handler = Target


handleEvent :: Target -> TargetEvent -> Integration.Outbound
handleEvent _ _ = Integration.none


outboundIntegration ''Handler
