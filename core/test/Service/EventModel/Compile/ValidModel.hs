-- | Compile the public source-local Feature composition contract.
-- The fixture references the testbed Feature actions without registering runtime workers.
module Service.EventModel.Compile.ValidModel where

import Core
import Service.Application (Application)
import Service.EventModel qualified as EventModel
import Testbed.Cart.Feature (cartFlow)
import Testbed.EventModel qualified as App
import Testbed.Stock.Feature (stockFlow)


valid :: Result Text EventModel.EventModel
valid = EventModel.eventModel do
  EventModel.appName "Compiler fixture"
  EventModel.features do
    cartFlow
    stockFlow


existingApplication :: Int -> Application
existingApplication port = case App.appEventModel port of
  Err problem -> panic problem
  Ok model -> EventModel.toApplication model
