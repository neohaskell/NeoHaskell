-- | Record a second HTTP observation for requested external effects.
-- The request shares the effect adapter without emitting a result command.
module Testbed.ExternalEffect.AuditOutbound (ExternalEffectAuditOutbound (..), handleEvent) where

import Core
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)
import Testbed.ExternalEffect.Core (ExternalEffectEntity, ExternalEffectRequestedPayload)
import Testbed.ExternalEffect.Outbound (deliverEffect)


data ExternalEffectAuditOutbound = ExternalEffectAuditOutbound


type instance EntityOf ExternalEffectAuditOutbound = ExternalEffectEntity


handleEvent :: ExternalEffectEntity -> ExternalEffectRequestedPayload -> Integration.Outbound
handleEvent _ payload = deliverEffect "audit" False payload


outboundIntegration ''ExternalEffectAuditOutbound
