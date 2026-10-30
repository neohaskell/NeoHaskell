-- | Compile the distinct command and view outcome builder forms.
module Service.EventModel.Compile.ValidRuleForms where

import Core hiding (event)
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Core (CartCreatedPayload (..), CartEntity)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Uuid qualified


zeroEventForm :: RuleAction ('Change CartEntity AddItem) ()
zeroEventForm = do
  givenNothing
  when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
  therefore do
    acceptedWithNoEvents


rejectionForm :: RuleAction ('Change CartEntity AddItem) ()
rejectionForm = do
  givenNothing
  when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
  therefore do
    rejected


viewDeleteForm :: RuleAction ('View CartSummary) ()
viewDeleteForm = do
  givenNothing
  when CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
  therefore do
    deleted @CartSummary


viewNoOpForm :: RuleAction ('View CartSummary) ()
viewNoOpForm = do
  givenNothing
  when CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
  therefore do
    noOp @CartSummary
