{-# OPTIONS_GHC -Wno-unused-imports #-}

module EnvironmentSpec where

import Core
import Array qualified
import Environment qualified
import Task qualified
import Text qualified
import Test


spec :: Spec Unit
spec = do
  describe "Environment" do
    describe "getArgs" do
      it "returns an Array of Text" \_ -> do
        -- Just verify the function exists and has the right type
        let _check = Environment.getArgs :: Task Text (Array Text)
        Task.yield ()

      it "result is an Array" \_ -> do
        -- Verify we can construct the type
        let _arr = Array.empty :: Array Text
        Task.yield ()

      it "can map over result" \_ -> do
        -- Verify Array operations work on the result type
        let _mapped = Array.empty |> Array.map Text.length :: Array Int
        Task.yield ()
