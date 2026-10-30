-- | Reject a state-view rule with a command outcome.
module Service.EventModel.Compile.ViewWithCommandOutcome where

import Core
import Service.EventModel
import Testbed.Cart.Core (CartCreatedPayload (..), CartEntity)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Uuid qualified

invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Invalid" do
    slice "Wrong outcome" do
      query @CartSummary do
        readsFrom @CartEntity do
          onEvent @CartCreatedPayload
      rule "Wrong outcome" do
        givenNothing
        when CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
        therefore do
          rejected
