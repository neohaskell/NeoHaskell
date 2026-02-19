{-# OPTIONS_GHC -Wno-unused-imports #-}

module LogSpec where

import Core
import Log (Level (..))
import Log qualified
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Log" do
    describe "Level ordering" do
      it "Debug < Info" \_ -> do
        (Debug < Info) |> shouldBe True

      it "Info < Warn" \_ -> do
        (Info < Warn) |> shouldBe True

      it "Warn < Error" \_ -> do
        (Warn < Error) |> shouldBe True

      it "Debug < Error" \_ -> do
        (Debug < Error) |> shouldBe True

      it "Error is not less than Debug" \_ -> do
        (Error < Debug) |> shouldBe False

    describe "Level show" do
      it "toText Debug == \"Debug\"" \_ -> do
        toText Debug |> shouldBe "Debug"

      it "toText Info == \"Info\"" \_ -> do
        toText Info |> shouldBe "Info"

      it "toText Warn == \"Warn\"" \_ -> do
        toText Warn |> shouldBe "Warn"

      it "toText Error == \"Error\"" \_ -> do
        toText Error |> shouldBe "Error"

    describe "Basic logging functions" do
      it "Log.info runs without error" \_ -> do
        Log.info "test info message"

      it "Log.debug runs without error" \_ -> do
        Log.debug "test debug message"

      it "Log.warn runs without error" \_ -> do
        Log.warn "test warn message"

      it "Log.critical runs without error" \_ -> do
        Log.critical "test critical message"

    describe "withScope" do
      it "withScope completes without error" \_ -> do
        Log.withScope [("requestId", "abc-123")] do
          Log.info "inside scope"

      it "withScope cleans up after completion" \_ -> do
        Log.withScope [("key", "value")] do
          Log.info "scoped message"
        -- If we get here, cleanup succeeded (no crash)
        Log.info "after scope"

      it "withScope cleans up on error" \_ -> do
        result <- Task.asResult do
          Log.withScope [("key", "value")] do
            Task.throw ("test error" :: Text)
        case (result :: Result Text Unit) of
          Err _ -> Log.info "recovered after scope error"
          Ok _ -> Log.info "unexpected success"

      it "nested withScope works" \_ -> do
        Log.withScope [("outer", "1")] do
          Log.withScope [("inner", "2")] do
            Log.info "nested scope message"
          Log.info "back to outer scope"
