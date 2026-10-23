{-# OPTIONS_GHC -Wno-unused-imports #-}

module TaskSpec (spec) where

import AsyncTask qualified
import Channel (Channel)
import Channel qualified
import Control.DeepSeq qualified as DeepSeq
import Core
import Maybe qualified
import Prelude qualified as GhcPrelude
import Result (Result (..))
import Result qualified
import Task qualified
import Test
import Text qualified
import Var qualified


spec :: Spec Unit
spec = do
  describe "Task" do
    describe "evaluate" do
      it "raises a deferred exception while the Task runs" \_ -> do
        result <- Task.evaluate (GhcPrelude.error "deferred outer error" :: Unit) |> Task.asResultSafe @Text
        case result of
          Err message -> message |> shouldSatisfy (Text.contains "deferred outer error")
          Ok _ -> Test.fail "expected a deferred exception"

      it "leaves nested fields unevaluated without force" \_ -> do
        result <- Task.evaluate (Just (GhcPrelude.error "nested value" :: Int))
        case result of
          Just _ -> Task.yield unit
          Nothing -> Test.fail "expected the outer constructor"

      it "checks nested values when the caller requests full evaluation" \_ -> do
        let values = [1, GhcPrelude.error "deferred nested error"] :: [Int]
        result <- Task.evaluate (DeepSeq.force values) |> Task.asResultSafe @Text
        case result of
          Err message -> message |> shouldSatisfy (Text.contains "deferred nested error")
          Ok _ -> Test.fail "expected a nested deferred exception"

    describe "mask" do
      it "defers cancellation and restores the caller's state" \_ -> do
        callerState <- Task.cancellationState
        (protectedState, restoredState) <- Task.mask \restore -> do
          protectedState <- Task.cancellationState
          restoredState <- restore Task.cancellationState
          Task.yield (protectedState, restoredState)
        protectedState |> shouldBe Task.CancellationDeferred
        restoredState |> shouldBe callerState

    describe "withCancellationBlocked" do
      it "blocks cancellation in its body and restores the caller's state" \_ -> do
        callerState <- Task.cancellationState
        protectedState <- Task.withCancellationBlocked Task.cancellationState
        restoredState <- Task.cancellationState
        protectedState |> shouldBe Task.CancellationBlocked
        restoredState |> shouldBe callerState

      it "returns success and typed failure without changing their values" \_ -> do
        success <- Task.withCancellationBlocked (Task.yield (37 :: Int))
        success |> shouldBe 37
        failure <- Task.withCancellationBlocked (Task.throw ("original" :: Text) :: Task Text Unit) |> Task.asResult
        failure |> shouldBe (Err "original")

      it "preserves a synchronous runtime failure" \_ -> do
        outcome <-
          Task.withCancellationBlocked (Task.evaluate (GhcPrelude.error "original synchronous exception" :: Unit))
            |> Task.asResultSafe @Text
        case outcome of
          Err message -> message |> shouldSatisfy (Text.contains "original synchronous exception")
          Ok _ -> Test.fail "expected the original runtime failure"

      it "finishes a blocking operation before delivering cancellation" \_ -> do
        entered <- Channel.new
        release <- Channel.newBounded 1 :: Task Text (Channel Unit)
        finishedWait <- Channel.new
        let action :: Task Text Unit
            action = Task.withCancellationBlocked do
              Channel.write unit entered
              Channel.read release
              Channel.write unit finishedWait
        child <- AsyncTask.run action
        Channel.read entered
        let waitForBlock attempts = do
              waiting <- AsyncTask.isWaitingForStateChange child
              if waiting
                then Task.yield unit
                else
                  if attempts <= 0
                    then Test.fail "the protected task did not reach its wait"
                    else do
                      AsyncTask.sleep 0
                      waitForBlock (attempts - 1)
        waitForBlock (100000 :: Int)
        requester <- AsyncTask.run do
          AsyncTask.requestThreadTermination child
        let waitForRequest attempts = do
              waiting <- AsyncTask.isWaitingForExceptionDelivery requester
              if waiting
                then Task.yield unit
                else
                  if attempts <= 0
                    then Test.fail "the cancellation request did not reach its wait"
                    else do
                      AsyncTask.sleep 0
                      waitForRequest (attempts - 1)
        waitForRequest (100000 :: Int)
        Channel.write unit release
        requestOutcome <- AsyncTask.waitOutcome requester
        case requestOutcome of
          AsyncTask.Completed _ -> Task.yield unit
          _ -> Test.fail "cancellation request did not finish"
        outcome <- AsyncTask.waitOutcome child
        case outcome of
          AsyncTask.Interrupted -> Task.yield unit
          _ -> Test.fail "expected cancellation after the wait ended"
        completedWait <- Channel.tryRead finishedWait
        completedWait |> shouldBe (Just unit)

    describe "asResultSafe" do
      it "catches synchronous IO exceptions as Err" \_ -> do
        let action = Task.evaluate (GhcPrelude.error "sync error" :: Unit) :: Task Text Unit
        result <- Task.fromIO (Task.runResult (Task.asResultSafe @Text action))
        case result of
          Ok innerResult -> (innerResult :: Result Text Unit) |> shouldSatisfy Result.isErr
          Err _ -> Test.fail "outer Task should not fail"
        observed <- Var.new False
        failure <-
          action
            |> Task.onFailure (\reason -> case reason of
                Task.RuntimeFailure -> Var.set True observed
                _ -> Task.yield unit
              )
            |> Task.asResultSafe @Text
        case failure of
          Err message -> message |> shouldSatisfy (Text.contains "sync error")
          Ok _ -> Test.fail "expected the original exception"
        observedFailure <- Var.get observed
        observedFailure |> shouldBe True

      it "catches Task.throw errors as Err" \_ -> do
        let action = Task.throw ("test error" :: Text)
        result <- Task.fromIO (Task.runResult (Task.asResultSafe @Text action))
        case result of
          Ok innerResult -> (innerResult :: Result Text Unit) |> shouldSatisfy Result.isErr
          Err _ -> Test.fail "outer Task should not fail"

      it "propagates cancellation instead of returning a result" \_ -> do
        ready <- Channel.new
        never <- Channel.newBounded 1 :: Task Text (Channel Unit)
        let action = do
              Channel.write unit ready
              Channel.read never
        handle <- AsyncTask.run (Task.asResultSafe @Text action :: Task Text (Result Text Unit))
        Channel.read ready
        AsyncTask.cancel handle
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Interrupted -> Task.yield unit
          _ -> Test.fail "expected cancellation to propagate"

      it "propagates thread termination instead of returning a result" \_ -> do
        ready <- Channel.new
        never <- Channel.newBounded 1 :: Task Text (Channel Unit)
        let action = do
              Channel.write unit ready
              Channel.read never
        handle <- AsyncTask.run (Task.asResultSafe @Text action :: Task Text (Result Text Unit))
        Channel.read ready
        AsyncTask.requestThreadTermination handle
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Interrupted -> Task.yield unit
          _ -> Test.fail "expected thread termination to propagate"

    describe "onFailure" do
      it "preserves a typed error after observing it" \_ -> do
        observed <- Var.new Nothing
        let action = Task.throw ("original" :: Text) :: Task Text Unit
        result <-
          action
            |> Task.onFailure (\reason -> case reason of
                Task.TypedFailure err -> Var.set (Just err) observed
                _ -> Task.yield unit
              )
            |> Task.asResult
        result |> shouldBe (Err "original")
        observedError <- Var.get observed
        observedError |> shouldBe (Just "original")

      it "does not replace a typed error with an observer error" \_ -> do
        let action = Task.throw ("original" :: Text) :: Task Text Unit
        result <- action |> Task.onFailure (\_ -> Task.throw ("observer" :: Text)) |> Task.asResult
        result |> shouldBe (Err "original")

      it "does not replace a typed error with an observer exception" \_ -> do
        let action = Task.throw ("original" :: Text) :: Task Text Unit
        let observer _ = Task.fromIO (GhcPrelude.ioError (GhcPrelude.userError "observer"))
        result <- action |> Task.onFailure observer |> Task.asResult
        result |> shouldBe (Err "original")

      it "does not call the observer after success" \_ -> do
        observed <- Var.new False
        result <- Task.yield unit |> Task.onFailure (\_ -> Var.set True observed)
        result |> shouldBe unit
        wasObserved <- Var.get observed
        wasObserved |> shouldBe False

      it "observes cancellation and propagates it" \_ -> do
        readyBarrier <- Channel.new
        neverChannel <- Channel.newBounded 1 :: Task Text (Channel Unit)
        observed <- Var.new False
        let blockedAction :: Task Text Unit
            blockedAction = do
              Channel.write unit readyBarrier
              Channel.read neverChannel
        let action =
              blockedAction
                |> Task.onFailure (\reason -> case reason of
                    Task.Interrupted -> Var.set True observed
                    _ -> Task.yield unit
                  )
        handle <- AsyncTask.run action
        Channel.read readyBarrier
        AsyncTask.cancel handle
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Interrupted -> Task.yield unit
          _ -> Test.fail "expected cancellation to propagate"
        wasObserved <- Var.get observed
        wasObserved |> shouldBe True

    describe "AsyncTask.waitOutcome" do
      it "keeps typed task errors typed" \_ -> do
        handle <- AsyncTask.run (Task.throw ("task error" :: Text) :: Task Text Unit)
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Failed message -> message |> shouldBe "task error"
          _ -> Test.fail "expected a typed task error"

      it "reports runtime exceptions as crashes" \_ -> do
        let action = Task.fromIO (GhcPrelude.ioError (GhcPrelude.userError "child crash")) :: Task Text Unit
        handle <- AsyncTask.run action
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Crashed message -> message |> shouldSatisfy (Text.contains "child crash")
          _ -> Test.fail "expected a runtime crash"

    describe "AsyncTask.isWaitingForStateChange" do
      it "observes an STM wait and then its completion" \_ -> do
        channel <- Channel.newBounded 1 :: Task Text (Channel Unit)
        handle <- AsyncTask.run (Channel.read channel :: Task Text Unit)
        let waitForBlock attempts = do
              waiting <- AsyncTask.isWaitingForStateChange handle
              if waiting
                then Task.yield unit
                else
                  if attempts <= 0
                    then Test.fail "child never waited for the channel"
                    else do
                      AsyncTask.sleep 5
                      waitForBlock (attempts - 1)
        waitForBlock (200 :: Int)
        Channel.write unit channel
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Completed _ -> Task.yield unit
          _ -> Test.fail "child did not complete after the channel write"
        waiting <- AsyncTask.isWaitingForStateChange handle
        waiting |> shouldBe False

    describe "AsyncTask.requestThreadTermination" do
      it "delivers ThreadKilled before waiting for child cleanup" \_ -> do
        started <- Channel.new
        cleanupEntered <- Channel.new
        cleanupRelease <- Channel.new
        blocked <- Channel.newBounded 1 :: Task Text (Channel Unit)
        let child :: Task Text Unit
            child = do
              Channel.write unit started
              Channel.read blocked
        handle <- AsyncTask.run
          (child |> Task.onFailure (\_ -> do
            Channel.write unit cleanupEntered
            Channel.read cleanupRelease
          ))
        Channel.read started
        request <- AsyncTask.run (AsyncTask.requestThreadTermination handle :: Task Text Unit)
        deliveryRace <- AsyncTask.race
          (Channel.read cleanupEntered)
          (AsyncTask.sleep 1000 :: Task Text Unit)
        case deliveryRace of
          AsyncTask.LeftWon _ -> Task.yield unit
          AsyncTask.RightWon _ -> Test.fail "termination request was not delivered"
        requestRace <- AsyncTask.race
          (AsyncTask.waitOutcome request)
          (AsyncTask.sleep 1000 :: Task Text Unit)
        case requestRace of
          AsyncTask.LeftWon (AsyncTask.Completed _) -> Task.yield unit
          _ -> Test.fail "termination request waited for child cleanup"
        Channel.write unit cleanupRelease
        outcome <- AsyncTask.waitOutcome handle
        case outcome of
          AsyncTask.Interrupted -> Task.yield unit
          _ -> Test.fail "child did not terminate after cleanup"
