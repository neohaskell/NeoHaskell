-- | Verify AtomicVar transactions and waits through public concurrency APIs.
--
-- The core test runner calls 'spec' to exercise map composition and cancellation.
module AtomicVarSpec (spec) where

import AsyncTask qualified
import AtomicVar qualified
import ConcurrentMap qualified
import Core
import Task qualified
import Test
import Transaction qualified


spec :: Spec Unit
spec = do
  describe "AtomicVar" do
    it "updates a value inside a map removal transaction" \_ -> do
      status <- AtomicVar.containing True
      workers <- ConcurrentMap.new @Text @(AtomicVar.AtomicVar Bool)
      ConcurrentMap.set "worker" status workers
      removed <-
        workers
          |> ConcurrentMap.removeIfTransaction "worker" (AtomicVar.modifyWithResultSTM (\accepting -> (False, accepting)))
      case removed of
        Just _ -> Task.yield unit
        Nothing -> Test.fail "expected the accepting worker to be removed"
      accepting <- AtomicVar.peek status
      accepting |> shouldBe False
      current <- ConcurrentMap.get "worker" workers
      case current of
        Just _ -> Test.fail "expected the worker map entry to be removed"
        Nothing -> Task.yield unit

    it "replaces a map entry and updates the old value in one transaction" \_ -> do
      existing <- AtomicVar.containing (1 :: Int)
      candidate <- AtomicVar.containing (5 :: Int)
      workers <- ConcurrentMap.new @Text @(AtomicVar.AtomicVar Int)
      ConcurrentMap.set "worker" existing workers
      (_, discarded) <-
        workers
          |> ConcurrentMap.getOrInsertIfTransaction
            "worker"
            candidate
            (AtomicVar.modifyWithResultSTM (\value -> (value + 1, value == 1)))
      case discarded of
        Just old -> do
          oldValue <- AtomicVar.peek old
          oldValue |> shouldBe 2
        Nothing -> Test.fail "expected the old worker to be returned"
      current <- ConcurrentMap.get "worker" workers
      case current of
        Just stored -> do
          currentValue <- AtomicVar.peek stored
          currentValue |> shouldBe 5
        Nothing -> Test.fail "expected the replacement worker in the map"

    it "runs composed updates as one transaction" \_ -> do
      left <- AtomicVar.containing (2 :: Int)
      right <- AtomicVar.containing (7 :: Int)
      previous <- Transaction.run do
        leftBefore <- AtomicVar.modifyWithResultSTM (\value -> (value + 1, value)) left
        rightBefore <- AtomicVar.modifyWithResultSTM (\value -> (value + 1, value)) right
        pure (leftBefore, rightBefore)
      previous |> shouldBe (2, 7)
      leftValue <- AtomicVar.peek left
      rightValue <- AtomicVar.peek right
      (leftValue, rightValue) |> shouldBe (3, 8)

    it "waits for a satisfying value and wakes after an update" \_ -> do
      status <- AtomicVar.containing (0 :: Int)
      waiter <- AsyncTask.run (AtomicVar.waitUntil (\value -> value == 2) status :: Task Text Unit)
      AtomicVar.set 1 status
      waitForStateChange waiter 1000
      AtomicVar.set 2 status
      AsyncTask.waitFor waiter

    it "lets a blocked predicate wait be cancelled" \_ -> do
      status <- AtomicVar.containing False
      waiter <- AsyncTask.run (AtomicVar.waitUntil identity status :: Task Text Unit)
      waitForStateChange waiter 1000
      AsyncTask.cancel waiter
      outcome <- AsyncTask.waitOutcome waiter
      case outcome of
        AsyncTask.Interrupted -> Task.yield unit
        AsyncTask.Completed _ -> Test.fail "expected cancellation to interrupt the wait"
        AsyncTask.Failed _ -> Test.fail "expected cancellation to interrupt the wait"
        AsyncTask.Crashed _ -> Test.fail "expected cancellation to interrupt the wait"


waitForStateChange :: AsyncTask.AsyncTask Text Unit -> Int -> Task Text Unit
waitForStateChange waiter attempts = do
  waiting <- AsyncTask.isWaitingForStateChange waiter
  Task.unless waiting do
    Task.when (attempts == 0) (Test.fail "expected the task to wait for the state change")
    AsyncTask.sleep 1
    waitForStateChange waiter (attempts - 1)
