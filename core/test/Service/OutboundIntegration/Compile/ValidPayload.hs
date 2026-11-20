-- | Compile a payload handler with an explicit parent event mapping.
-- The standalone compiler check uses this module as the accepted control.
module Service.OutboundIntegration.Compile.ValidPayload where

import Core
import EventVariantOf (EventVariantOf (..))
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)


data Target = Target


data TargetEvent = TargetAdded Added | TargetRemoved


data Added = Added


type instance EventOf Target = TargetEvent


instance EventVariantOf TargetEvent Added where
  fromVariant payload = TargetAdded payload
  toVariant eventValue = case eventValue of
    TargetAdded payload -> Just payload
    TargetRemoved -> Nothing


data Handler = Handler


type instance EntityOf Handler = Target


handleEvent :: Target -> Added -> Integration.Outbound
handleEvent _ _ = Integration.none


outboundIntegration ''Handler
