-- | Reject an entity absent from a query's typed source relation.
module Service.EventModel.Compile.WrongQuerySource where

import Core
import Service.EventModel
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Stock.Core (StockEntity)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Queries" do
    slice "Wrong source" do
      query @CartSummary do
        readsFrom @StockEntity do
          pass
