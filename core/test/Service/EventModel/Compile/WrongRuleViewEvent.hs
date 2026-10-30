-- | Reject a view rule whose incoming payload has an undeclared entity source.
module Service.EventModel.Compile.WrongRuleViewEvent where

import Core
import Service.EventModel
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Stock.Core (StockInitializedPayload (..))
import Uuid qualified

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Wrong event" do
      query @CartSummary do
        pass
      rule "Wrong event" do
        givenNothing
        when StockInitializedPayload {initializedEntityId = Uuid.nil, initializedProductId = Uuid.nil, initializedAvailable = 1}
        therefore do
          noOp @CartSummary
