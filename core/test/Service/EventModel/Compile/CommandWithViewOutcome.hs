-- | Reject a state-change rule with a query outcome.
module Service.EventModel.Compile.CommandWithViewOutcome where

import Core
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Queries.CartSummary (CartSummary (..))
import Uuid qualified

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Wrong outcome" do
      entity @CartEntity do
        command @AddItem do
          pass
      rule "Wrong outcome" do
        givenNothing
        when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
        therefore do
          updatedTo Uuid.nil CartSummary {cartSummaryId = Uuid.nil, ownerId = "", itemCount = 0, isEmpty = True, iban = Nothing}
