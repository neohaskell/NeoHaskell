-- | Reject two initiating commands in one state-change Slice.
module Service.EventModel.Compile.TwoCommands where

import Core
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Commands.CreateTypedInboundCart (CreateTypedInboundCart)
import Testbed.Cart.Core (CartEntity)


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Commands" do
    slice "Two commands" do
      entity @CartEntity do
        command @AddItem do
          pass
        command @CreateTypedInboundCart do
          pass
