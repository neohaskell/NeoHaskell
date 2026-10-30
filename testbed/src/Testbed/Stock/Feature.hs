-- | Describe stock topology beside the stock implementation.
-- Export 'stockFlow' for the application Event Model.
module Testbed.Stock.Feature (stockFlow) where

import Service.EventModel
import Testbed.Stock.Commands.InitializeStock (InitializeStock)
import Testbed.Stock.Core (StockEntity, StockInitializedPayload)
import Testbed.Stock.Queries.StockLevel (StockLevel)


stockFlow :: FeatureAction
stockFlow = feature "Stock flow" do
  chapter "Track availability" do
    slice "Initialize stock" do
      entity @StockEntity do
        command @InitializeStock do
          produces @StockInitializedPayload

    slice "Show stock level" do
      query @StockLevel do
        readsFrom @StockEntity do
          onEvent @StockInitializedPayload
