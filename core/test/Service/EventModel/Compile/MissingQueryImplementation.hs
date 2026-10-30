-- | Reject a declared view without its executable Query instance.
module Service.EventModel.Compile.MissingQueryImplementation where

import Core
import Service.EventModel
import Testbed.Cart.Core (CartEntity)


data MissingView


type instance EntitiesOf MissingView = '[CartEntity]


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Queries" do
    slice "Missing implementation" do
      query @MissingView do
        readsFrom @CartEntity do
          pass
