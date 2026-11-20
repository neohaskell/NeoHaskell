-- | Declare external effect commands, projection, and outbound work.
-- Compose 'externalEffectFlow' in the application model.
module Testbed.ExternalEffect.Feature (externalEffectFlow) where

import Service.EventModel
import Testbed.ExternalEffect.AuditOutbound (ExternalEffectAuditOutbound)
import Testbed.ExternalEffect.Commands.RecordExternalEffect (RecordExternalEffect)
import Testbed.ExternalEffect.Commands.RequestExternalEffect (RequestExternalEffect)
import Testbed.ExternalEffect.Core (ExternalEffectEntity, ExternalEffectEvent, ExternalEffectRequestedPayload)
import Testbed.ExternalEffect.Outbound (ExternalEffectOutbound)
import Testbed.ExternalEffect.Query (ExternalEffectSummary)


externalEffectFlow :: FeatureAction
externalEffectFlow = feature "External effect flow" do
  chapter "Deliver effects" do
    slice "Request an external effect" do
      entity @ExternalEffectEntity do
        command @RequestExternalEffect do
          produces @ExternalEffectRequestedPayload

    slice "Record an external effect" do
      entity @ExternalEffectEntity do
        command @RecordExternalEffect do
          produces @ExternalEffectEvent

    slice "Show external effect status" do
      query @ExternalEffectSummary do
        readsFrom @ExternalEffectEntity do
          onEvent @ExternalEffectEvent

    slice "Deliver a requested external effect" do
      outboundIntegration @ExternalEffectOutbound do
        onEvent @ExternalEffectRequestedPayload
        triggersCommand @RecordExternalEffect do
          onTargetEntity @ExternalEffectEntity

    slice "Audit a requested external effect" do
      outboundIntegration @ExternalEffectAuditOutbound do
        onEvent @ExternalEffectRequestedPayload
