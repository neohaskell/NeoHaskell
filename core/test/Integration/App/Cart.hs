module Integration.App.Cart (
  model,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)
import Integration.App.Cart.Core (CartEntity, CartEvent)


model :: Model '[CreateCart] Unit
model = do
  entity @CartEntity
  events @CartEvent
  -- Commands
  command @CreateCart