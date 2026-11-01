-- | Describe the cart flow beside its commands, query, and integrations.
-- Export 'cartFlow' for the application Event Model.
module Testbed.Cart.Feature (cartFlow) where

import Basics (Bool (..), Int, modBy, (+))
import Maybe (Maybe (..))
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Commands.CreateCart (CreateCart)
import Testbed.Cart.Commands.CreateCartInternal (CreateCartInternal)
import Testbed.Cart.Commands.CreateTypedInboundCart (CreateTypedInboundCart)
import Testbed.Cart.Commands.RegisterCartByKey (RegisterCartByKey)
import Testbed.Cart.Commands.SaveIBAN (SaveIBAN)
import Testbed.Cart.Core (CartCreatedPayload (..), CartEntity, IBANSavedPayload, ItemAddedPayload (..))
import Testbed.Cart.Integrations (PeriodicCartCreator)
import Testbed.Cart.Integrations.EventCounter qualified as EventCounter
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary (..))
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Commands.InitializeStockInternal (InitializeStockInternal)
import Testbed.Stock.Commands.ReserveStock (ReserveStock)
import Testbed.Stock.Core (StockEntity)
import Uuid qualified


cartFlow :: FeatureAction
cartFlow = feature "Cart flow" do
  chapter "Manage carts" do
    slice "Create a cart" do
      entity @CartEntity do
        command @CreateCart do
          produces @CartCreatedPayload

    slice "Create a cart internally" do
      entity @CartEntity do
        command @CreateCartInternal do
          produces @CartCreatedPayload

    slice "Create a cart from typed inbound" do
      entity @CartEntity do
        command @CreateTypedInboundCart do
          produces @CartCreatedPayload

    slice "Register a cart by key" do
      entity @CartEntity do
        command @RegisterCartByKey do
          produces @CartCreatedPayload

    slice "Add an item" do
      entity @CartEntity do
        command @AddItem do
          produces @ItemAddedPayload

      rule "Adding an item emits its exact event" do
        quantitySeed <- requestValue @Int
        let quantity = modBy 10 quantitySeed + 1
        given do
          event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
        when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity}
        therefore do
          event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = quantity}

      rule "An unknown cart rejects the command" do
        givenNothing
        when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
        therefore do
          rejectedWith "Cart not found!"

      rule "A missing cart reason matches its pattern" do
        givenNothing
        when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
        therefore do
          rejectedMatches [regex|^Cart not found!$|]

    slice "Show cart summary" do
      query @CartSummary do
        readsFrom @CartEntity do
          onEvent @CartCreatedPayload
          onEvent @ItemAddedPayload
          onEvent @IBANSavedPayload

      rule "A new cart has an empty summary" do
        givenNothing
        when CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
        therefore do
          updatedTo CartSummary {cartSummaryId = Uuid.nil, ownerId = "owner", itemCount = 0, isEmpty = True, iban = Nothing}

      rule "An added item changes the summary" do
        given do
          event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
        when ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
        therefore do
          updatedTo CartSummary {cartSummaryId = Uuid.nil, ownerId = "owner", itemCount = 1, isEmpty = False, iban = Nothing}

    slice "Save a cart IBAN" do
      entity @CartEntity do
        command @SaveIBAN do
          produces @IBANSavedPayload

    slice "Reserve stock after an item" do
      outboundIntegration @ReserveStockOnItemAdded do
        onEvent @ItemAddedPayload
        triggersCommand @ReserveStock do
          onTargetEntity @StockEntity
        outboundLifecycle @() @CartEntity (\_ -> EventCounter.eventCounterIntegration)

    slice "Accept typed inbound commands" do
      inboundIntegration @TypedInboundExample do
        triggersCommand @CreateTypedInboundCart do
          onTargetEntity @CartEntity
        triggersCommand @InitializeStockInternal do
          onTargetEntity @StockEntity

    slice "Create a cart periodically" do
      inboundIntegration @PeriodicCartCreator do
        triggersCommand @CreateCartInternal do
          onTargetEntity @CartEntity
