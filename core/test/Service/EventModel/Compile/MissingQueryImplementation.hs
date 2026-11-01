-- | Reject a declared view without its executable Query instance.
module Service.EventModel.Compile.MissingQueryImplementation where

import Core
import Json qualified
import Service.EventModel
import Testbed.Cart.Core (CartEntity (..))


data MissingView = MissingView
  deriving (Generic)


type instance NameOf MissingView = "MissingView"


instance Json.ToJSON MissingView


instance Json.FromJSON MissingView


instance ToSchema MissingView


instance QueryOf CartEntity MissingView where
  queryId cart = case cart of
    CartEntity {cartId} -> cartId
  combine _ _ = NoOp


type instance EntitiesOf MissingView = '[CartEntity]


invalid :: FeatureAction
invalid = feature "Cart" do
  chapter "Queries" do
    slice "Missing implementation" do
      query @MissingView do
        readsFrom @CartEntity do
          pass
