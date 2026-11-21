-- | Reject zero-event success combined with a rejection.
module Service.EventModel.Compile.MixedZeroRejectionOutcomes where

import Core
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Core (CartEntity)
import Uuid qualified

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Mixed outcome" do
      entity @CartEntity do
        command @AddItem do
          pass
      rule "Mixed outcome" do
        givenNothing
        when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
        therefore do
          acceptedWithNoEvents
          rejected
