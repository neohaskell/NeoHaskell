-- | Reject a command rule that uses a different initiating command.
module Service.EventModel.Compile.WrongRuleCommand where

import Core
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Core (CartEntity)
import Testbed.Stock.Commands.ReserveStock (ReserveStock (..))
import Uuid qualified

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Wrong command" do
      entity @CartEntity do
        command @AddItem do
          pass
      rule "Wrong command" do
        givenNothing
        when ReserveStock {stockId = Uuid.nil, cartId = Uuid.nil, quantity = 1}
        therefore do
          rejected
