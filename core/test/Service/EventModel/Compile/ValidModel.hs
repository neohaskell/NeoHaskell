-- | Compile the public source-local Feature composition contract.
-- The fixture references the testbed Feature actions without registering runtime workers.
module Service.EventModel.Compile.ValidModel where

import App qualified
import Core
import Service.Application (Application)
import Service.EventModel qualified as EventModel
import Testbed.Cart.Feature (cartFlow)
import Testbed.Stock.Feature (stockFlow)


valid :: Result Text EventModel.EventModel
valid = EventModel.eventModel do
  EventModel.appName "Compiler fixture"
  EventModel.features do
    cartFlow
    stockFlow


existingApplication :: Int -> Application
existingApplication port = App.app port
