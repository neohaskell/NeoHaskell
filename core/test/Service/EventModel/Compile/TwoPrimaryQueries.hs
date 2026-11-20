-- | Reject two different primary queries in one Slice.
module Service.EventModel.Compile.TwoPrimaryQueries where

import Core
import Service.EventModel
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Stock.Queries.StockLevel (StockLevel)

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Two queries" do
      query @CartSummary do
        pass
      query @StockLevel do
        pass
