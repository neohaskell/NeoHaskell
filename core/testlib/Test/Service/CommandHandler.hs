module Test.Service.CommandHandler where

import Array qualified
import Core
import Service.EntityFetcher qualified as EntityFetcher
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.Command.Core (CartEntity (..), CartEvent (..), applyCartEvent)
import Test.Service.CommandHandler.Execute.Spec qualified as Execute


spec :: Spec Unit
spec = do
  describe "CommandHandler Specification Tests" do
    let newCartStoreAndFetcher = do
          store <- InMemory.new @CartEvent |> Task.mapError toText
          let initialState =
                CartEntity
                  { cartId = def,
                    cartItems = Array.empty,
                    cartCheckedOut = False
                  }
          fetcher <- EntityFetcher.new store initialState applyCartEvent |> Task.mapError (\err -> toText err |> toText)
          pure (store, fetcher)

    Execute.spec newCartStoreAndFetcher
