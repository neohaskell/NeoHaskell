-- | Describe stock topology beside the stock implementation.
-- Export 'stockFlow' for the application Event Model.
module Testbed.Stock.Feature (stockFlow) where

import Service.EventModel
import Testbed.Cart.Core (CartCreatedPayload, CartEntity)
import Testbed.Cart.Queries.CartStockActivity (CartStockActivity)
import Testbed.Stock.Commands.InitializeStock (InitializeStock)
import Testbed.Stock.Commands.InitializeStockInternal (InitializeStockInternal)
import Testbed.Stock.Commands.ReserveStock (ReserveStock)
import Testbed.Stock.Core (StockEntity, StockInitializedPayload, StockReservedPayload)
import Testbed.Stock.Queries.StockLevel (StockLevel)


stockFlow :: FeatureAction
stockFlow = feature "Stock flow" do
  chapter "Track availability" do
    slice "Initialize stock" do
      entity @StockEntity do
        command @InitializeStock do
          produces @StockInitializedPayload

    slice "Initialize stock internally" do
      entity @StockEntity do
        command @InitializeStockInternal do
          produces @StockInitializedPayload

    slice "Reserve stock" do
      entity @StockEntity do
        command @ReserveStock do
          produces @StockReservedPayload

    slice "Show stock level" do
      query @StockLevel do
        readsFrom @StockEntity do
          onEvent @StockInitializedPayload
          onEvent @StockReservedPayload

    slice "Show declared cart and stock activity" do
      query @CartStockActivity do
        readsFrom @CartEntity do
          onEvent @CartCreatedPayload
        readsFrom @StockEntity do
          onEvent @StockInitializedPayload
