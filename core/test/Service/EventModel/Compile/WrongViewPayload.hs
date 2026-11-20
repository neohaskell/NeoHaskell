-- | Reject a query subscription to a payload owned by another entity.
-- Compile this fixture through the Event Model rejection runner.
module Service.EventModel.Compile.WrongViewPayload where

import Service.EventModel
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Stock.Core (StockInitializedPayload)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Views" do
    slice "Wrong view payload" do
      query @CartSummary do
        readsFrom @CartEntity do
          onEvent @StockInitializedPayload
