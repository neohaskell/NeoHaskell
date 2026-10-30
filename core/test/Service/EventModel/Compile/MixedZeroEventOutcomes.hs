-- | Reject zero-event success combined with an emitted event.
module Service.EventModel.Compile.MixedZeroEventOutcomes where

import Core hiding (event)
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Core (CartEntity, ItemAddedPayload (..))
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
          event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
          acceptedWithNoEvents
