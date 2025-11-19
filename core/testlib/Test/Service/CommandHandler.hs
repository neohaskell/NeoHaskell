module Test.Service.CommandHandler where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EventStore (EventStore)
import Test
import Test.Service.Command.Core (CartEntity, CartEvent, OrderEntity, OrderEvent)
import Test.Service.CommandHandler.Execute.Spec qualified as Execute


spec ::
  Task Text (EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Spec Unit
spec newCartStoreAndFetcher newOrderStoreAndFetcher = do
  describe "CommandHandler Specification Tests" do
    Execute.spec newCartStoreAndFetcher newOrderStoreAndFetcher
