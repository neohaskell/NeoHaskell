-- | Reject an outbound trigger that differs from the handler's payload.
module Service.EventModel.Compile.WrongOutboundPayload where

import Service.EventModel
import Testbed.Cart.Core (CartEvent)
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Integrations" do
    slice "Wrong payload" do
      outboundIntegration @ReserveStockOnItemAdded do
        onEvent @CartEvent
