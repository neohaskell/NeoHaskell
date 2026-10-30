-- | Reject a planned query source outside its declared entity list.
-- Compile this module with the Event Model rejection runner.
module Service.EventModel.Compile.WrongPlannedQuerySource where

import Core
import Service.EventModel
import Testbed.Cart.Core (CartEntity)
import Testbed.Stock.Core (StockEntity)


data PlannedView


type instance EntitiesOf PlannedView = '[CartEntity]


invalid :: FeatureAction
invalid = feature "Planned" do
  chapter "Read" do
    slice "Wrong planned source" do
      plannedQuery @PlannedView do
        plannedReadsFrom @StockEntity do
          pass
