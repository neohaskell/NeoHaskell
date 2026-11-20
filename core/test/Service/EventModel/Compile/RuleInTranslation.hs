-- | Reject a Given-When-Therefore rule inside a translation Slice.
module Service.EventModel.Compile.RuleInTranslation where

import Core
import Service.EventModel
import Testbed.Cart.Core (ItemAddedPayload (..))
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Uuid qualified

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Translation" do
      outboundIntegration @ReserveStockOnItemAdded do
        onEvent @ItemAddedPayload
      rule "Not a translation rule" do
        givenNothing
        when ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
        therefore do
          rejected
