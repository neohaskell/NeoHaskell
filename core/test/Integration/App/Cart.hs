module Integration.App.Cart (
  model,
) where

import Core
import Integration.App.Cart.Commands.CreateCart (CreateCart)
import Service.Model.Core qualified as Model


model :: Model _ Unit
model = Model.do
  command @CreateCart