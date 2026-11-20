-- | Reject a declared command without its executable Command instance.
module Service.EventModel.Compile.MissingCommandImplementation where

import Core
import Service.EventModel
import Testbed.Cart.Core (CartEntity)


data MissingCommand


type instance EntityOf MissingCommand = CartEntity


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Commands" do
    slice "Missing implementation" do
      entity @CartEntity do
        command @MissingCommand do
          pass
