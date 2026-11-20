-- | Reject an outbound command aimed at the wrong entity type.
module Service.EventModel.Compile.WrongCommandTarget where

import Service.EventModel
import Testbed.Cart.Core (CartEntity, ItemAddedPayload)
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Stock.Commands.ReserveStock (ReserveStock)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Integrations" do
    slice "Wrong target" do
      outboundIntegration @ReserveStockOnItemAdded do
        onEvent @ItemAddedPayload
        triggersCommand @ReserveStock do
          onTargetEntity @CartEntity
