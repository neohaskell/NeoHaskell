-- | Reject a payload without a separately generated Event ADT mapping.
module Service.EventModel.Compile.MissingEventVariant where

import Core
import Service.EventModel
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Core (CartEntity)


data MissingPayload


type instance EntityOf MissingPayload = CartEntity


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Commands" do
    slice "Missing event variant" do
      entity @CartEntity do
        command @AddItem do
          produces @MissingPayload
