-- | Describe the cart flow beside its commands, query, and integrations.
-- Export 'cartFlow' for the application Event Model.
module Testbed.Cart.Feature (cartFlow) where

import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Commands.CreateTypedInboundCart (CreateTypedInboundCart)
import Testbed.Cart.Core (CartCreatedPayload, CartEntity, ItemAddedPayload)
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Commands.InitializeStockInternal (InitializeStockInternal)
import Testbed.Stock.Commands.ReserveStock (ReserveStock)
import Testbed.Stock.Core (StockEntity)


cartFlow :: FeatureAction
cartFlow = feature "Cart flow" do
  chapter "Manage carts" do
    slice "Add an item" do
      entity @CartEntity do
        command @AddItem do
          produces @ItemAddedPayload

    slice "Show cart summary" do
      query @CartSummary do
        readsFrom @CartEntity do
          onEvent @CartCreatedPayload
          onEvent @ItemAddedPayload

    slice "Reserve stock after an item" do
      outboundIntegration @ReserveStockOnItemAdded do
        onEvent @ItemAddedPayload
        triggersCommand @ReserveStock do
          onTargetEntity @StockEntity

    slice "Accept typed inbound commands" do
      inboundIntegration @TypedInboundExample do
        triggersCommand @CreateTypedInboundCart do
          onTargetEntity @CartEntity
        triggersCommand @InitializeStockInternal do
          onTargetEntity @StockEntity
