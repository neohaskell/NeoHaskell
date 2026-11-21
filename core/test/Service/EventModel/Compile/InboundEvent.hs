-- | Reject an event trigger inside an inbound translation.
module Service.EventModel.Compile.InboundEvent where

import Service.EventModel
import Testbed.Cart.Core (ItemAddedPayload)
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Integrations" do
    slice "Inbound event" do
      inboundIntegration @TypedInboundExample do
        onEvent @ItemAddedPayload
