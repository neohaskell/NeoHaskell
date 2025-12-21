module Testbed.Cart.Service (
  service,
) where

import Core
import Service qualified
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Commands.CreateCart (CreateCart)
import Testbed.Cart.Core ()


service :: Service _ _ _ _
service =
  Service.new
    |> Service.command @AddItem
    |> Service.command @CreateCart