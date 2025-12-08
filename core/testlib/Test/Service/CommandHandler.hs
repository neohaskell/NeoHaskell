module Test.Service.CommandHandler where

import Array qualified
import Core
import Service.EntityFetcher qualified as EntityFetcher
import Service.EventStore.Core (EventStore)
import Task qualified
import Test
import Test.Service.Command.Core (CartEntity (..), applyCartEvent)
import Test.Service.EventStore.Core (CartEvent (..))
import Test.Service.CommandHandler.Execute.Spec qualified as Execute


spec :: Task Text (EventStore CartEvent) -> Spec Unit
spec newStore = do
  describe "CommandHandler Specification Tests" do
    let newCartStoreAndFetcher = do
          store <- newStore
          let initialState =
                CartEntity
                  { cartId = def,
                    cartItems = Array.empty,
                    cartCheckedOut = False
                  }
          fetcher <- EntityFetcher.new store initialState applyCartEvent |> Task.mapError toText
          pure (store, fetcher)

    Execute.spec newCartStoreAndFetcher
