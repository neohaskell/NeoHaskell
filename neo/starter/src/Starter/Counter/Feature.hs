module Starter.Counter.Feature (counterFlow) where

import Service.EventModel
import Starter.Counter.Commands.CreateCounter (CreateCounter)
import Starter.Counter.Commands.DecrementCounter (DecrementCounter)
import Starter.Counter.Commands.IncrementCounter (IncrementCounter)
import Starter.Counter.Entity (CounterEntity)
import Starter.Counter.Events.CounterCreated (CounterCreated)
import Starter.Counter.Events.CounterDecremented (CounterDecremented)
import Starter.Counter.Events.CounterIncremented (CounterIncremented)
import Starter.Counter.Queries.CounterView (CounterView)


counterFlow :: FeatureAction
counterFlow = feature "Counter flow" do
  chapter "Manage counters" do
    slice "Create a counter" do
      entity @CounterEntity do
        command @CreateCounter do
          produces @CounterCreated

    slice "Increment a counter" do
      entity @CounterEntity do
        command @IncrementCounter do
          produces @CounterIncremented

    slice "Decrement a counter" do
      entity @CounterEntity do
        command @DecrementCounter do
          produces @CounterDecremented

    slice "Show counter values" do
      query @CounterView do
        readsFrom @CounterEntity do
          onEvent @CounterCreated
          onEvent @CounterIncremented
          onEvent @CounterDecremented
