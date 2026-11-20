-- | Checks lock release and acquisition timeout behavior.
-- Run in the core suite when changing Lock.with or Lock.withTimeout.
module LockSpec (spec) where

import AsyncTask qualified
import Core
import Lock qualified
import Task qualified
import Test
import Var qualified


spec :: Spec Unit
spec = do
  describe "Lock.with" do
    it "releases the lock after a runtime failure" \_ -> do
      lock <- Lock.new
      let failing =
            Task.evaluate (panic "lock-with-sentinel")
              :: Task.Task Text Unit

      outcome <- Lock.with lock failing |> Task.asResultSafe
      case outcome of
        Err _ -> pass
        Ok _ -> fail "Expected the runtime exception to escape"

      acquired <- Lock.withTimeout 1 lock (Task.yield (5 :: Int))
      acquired |> shouldBe (Ok 5)

  describe "Lock.withTimeout" do
    it "limits acquisition without timing out the action" \_ -> do
      lock <- Lock.new
      result <- Lock.withTimeout 1 lock do
        AsyncTask.sleep 1100
        Task.yield (6 :: Int)
      result |> shouldBe (Ok 6)

    it "times out while another owner holds the lock, then permits a new owner" \_ -> do
      lock <- Lock.new
      timedOutActionRan <- Var.new False
      Lock.acquire lock

      let timedOutAction = do
            Var.set True timedOutActionRan
            Task.yield (1 :: Int)
      blocked <- Lock.withTimeout 1 lock timedOutAction
      blocked |> shouldBe (Err "lock timeout")

      Lock.release lock
      acquired <- Lock.withTimeout 1 lock (Task.yield (2 :: Int))
      acquired |> shouldBe (Ok 2)
      ran <- Var.get timedOutActionRan
      ran |> shouldBe False

    it "releases the lock when its task fails" \_ -> do
      lock <- Lock.new
      let failing = Task.throw ("action failed" :: Text) :: Task.Task Text Int

      failed <- Lock.withTimeout 1 lock failing |> Task.asResult
      failed |> shouldBe (Err "action failed")

      acquired <- Lock.withTimeout 1 lock (Task.yield (3 :: Int))
      acquired |> shouldBe (Ok 3)

    it "releases the lock when its task throws at runtime" \_ -> do
      lock <- Lock.new
      let failing =
            Task.evaluate (panic "lock-action-sentinel")
              :: Task.Task Text Int

      outcome <- Lock.withTimeout 1 lock failing |> Task.asResultSafe
      case outcome of
        Err _ -> pass
        Ok _ -> fail "Expected the runtime exception to escape"

      acquired <- Lock.withTimeout 1 lock (Task.yield (4 :: Int))
      acquired |> shouldBe (Ok 4)
