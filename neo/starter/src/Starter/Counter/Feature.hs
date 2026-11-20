-- | Declare the executable counter flow beside its commands and query.
module Starter.Counter.Feature (counterFlow) where

import Service.EventModel
import Starter.Counter.Commands.CreateCounter (CreateCounter)
import Starter.Counter.Commands.DecrementCounter (DecrementCounter)
import Starter.Counter.Commands.IncrementCounter (IncrementCounter)
import Starter.Counter.Core (CounterEntity)
import Starter.Counter.Events.CounterCreated qualified as CounterCreated
import Starter.Counter.Events.CounterDecremented qualified as CounterDecremented
import Starter.Counter.Events.CounterIncremented qualified as CounterIncremented
import Starter.Counter.Queries.CounterView (CounterView)


counterFlow :: FeatureAction
counterFlow = feature "Counter flow" do
  chapter "Manage counters" do
    slice "Create a counter" do
      entity @CounterEntity do
        command @CreateCounter do
          produces @CounterCreated.Event

    slice "Increment a counter" do
      entity @CounterEntity do
        command @IncrementCounter do
          produces @CounterIncremented.Event

    slice "Decrement a counter" do
      entity @CounterEntity do
        command @DecrementCounter do
          produces @CounterDecremented.Event

    slice "Show counter values" do
      query @CounterView do
        readsFrom @CounterEntity do
          onEvent @CounterCreated.Event
          onEvent @CounterIncremented.Event
          onEvent @CounterDecremented.Event
