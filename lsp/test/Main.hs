module Main (main) where

import Prelude

import Control.Concurrent.STM (atomically, readTVar)
import Data.Map.Strict qualified as Map
import Test.Hspec

import NeoHaskell.LSP.State (ServerState (..), initState)


main :: IO ()
main = hspec do
  describe "NeoHaskell.LSP.State" do
    describe "initState" do
      it "creates state with empty documents" \_ -> do
        state <- initState
        docs <- atomically (readTVar state.documents)
        docs `shouldBe` Map.empty

      it "creates state with no project root" \_ -> do
        state <- initState
        root <- atomically (readTVar state.projectRoot)
        root `shouldBe` Nothing

      it "creates state with empty transpilation cache" \_ -> do
        state <- initState
        cache <- atomically (readTVar state.lastGoodTranspilation)
        cache `shouldBe` Map.empty
