module LockSpec (spec) where

import Core
import Lock qualified
import Task qualified
import Test
import Var qualified


spec :: Spec Unit
spec = do
  describe "Lock.withTimeout" do
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
