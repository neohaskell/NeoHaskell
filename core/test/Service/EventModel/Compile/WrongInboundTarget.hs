-- | Reject an inbound command aimed at an entity other than its declared owner.
-- Compile this fixture through the Event Model rejection runner.
module Service.EventModel.Compile.WrongInboundTarget where

import Service.EventModel
import Testbed.Cart.Commands.CreateTypedInboundCart (CreateTypedInboundCart)
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Core (StockEntity)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Integrations" do
    slice "Wrong inbound target" do
      inboundIntegration @TypedInboundExample do
        triggersCommand @CreateTypedInboundCart do
          onTargetEntity @StockEntity
