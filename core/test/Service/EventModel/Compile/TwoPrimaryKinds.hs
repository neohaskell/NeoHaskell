-- | Reject a command and query as two primary declarations in one Slice.
module Service.EventModel.Compile.TwoPrimaryKinds where

import Core
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Queries.CartSummary (CartSummary)

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Two roots" do
      entity @CartEntity do
        command @AddItem do
          pass
      query @CartSummary do
        pass
