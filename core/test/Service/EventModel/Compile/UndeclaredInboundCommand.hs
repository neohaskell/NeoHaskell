-- | Reject an inbound command absent from the typed worker output set.
module Service.EventModel.Compile.UndeclaredInboundCommand where

import Service.EventModel
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Commands.ReserveStock (ReserveStock)
import Testbed.Stock.Core (StockEntity)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Integrations" do
    slice "Undeclared output" do
      inboundIntegration @TypedInboundExample do
        triggersCommand @ReserveStock do
          onTargetEntity @StockEntity
