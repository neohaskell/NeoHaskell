module AtomicVarSpec (spec) where

import AsyncTask qualified
import AtomicVar qualified
import ConcurrentMap qualified
import Core
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "AtomicVar" do
    it "updates a value inside a map removal transaction" \_ -> do
      status <- AtomicVar.containing True
      workers <- ConcurrentMap.new @Text @(AtomicVar.AtomicVar Bool)
      ConcurrentMap.set "worker" status workers
      removed <-
        workers
          |> ConcurrentMap.removeIfM "worker" (AtomicVar.modifyWithResultSTM (\accepting -> (False, accepting)))
      case removed of
        Just _ -> Task.yield unit
        Nothing -> Test.fail "expected the accepting worker to be removed"
      accepting <- AtomicVar.peek status
      accepting |> shouldBe False
      current <- ConcurrentMap.get "worker" workers
      case current of
        Just _ -> Test.fail "expected the worker map entry to be removed"
        Nothing -> Task.yield unit

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
  if waiting
    then Task.yield unit
    else if attempts == 0
      then Test.fail "expected the task to wait for the state change"
      else do
        AsyncTask.sleep 1
        waitForStateChange waiter (attempts - 1)
