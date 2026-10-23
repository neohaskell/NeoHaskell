{-# OPTIONS_GHC -Wno-unused-imports #-}

module TaskSpec (spec) where

import AsyncTask qualified
import Channel (Channel)
import Channel qualified
import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.Async qualified as GhcAsync
import Control.Concurrent.MVar qualified as GhcMVar
import Control.DeepSeq qualified as DeepSeq
import Control.Exception qualified as GhcException
import Data.Either qualified as Either
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

    describe "asResultSafe" do
      it "catches synchronous IO exceptions as Err" \_ -> do
        let action = Task.fromIO (GhcException.throwIO (GhcException.ErrorCall "sync error"))
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

      it "propagates AsyncCancelled (does not catch it)" \_ -> do
        readyBarrier <- Task.fromIO GhcMVar.newEmptyMVar
        neverVar <- Task.fromIO GhcMVar.newEmptyMVar
        handle <- Task.fromIO (GhcAsync.async (do { GhcMVar.putMVar readyBarrier (); Task.runResult (Task.asResultSafe @Text (Task.fromIO (GhcMVar.readMVar neverVar))) }))
        Task.fromIO (GhcMVar.takeMVar readyBarrier)
        Task.fromIO (GhcAsync.cancel handle)
        outcome <- Task.fromIO (GhcAsync.waitCatch handle)
        case outcome of
          Either.Left someException ->
            GhcException.fromException @GhcAsync.AsyncCancelled someException
              |> shouldSatisfy (\m -> case m of { Just _ -> True; Nothing -> False })
          Either.Right _ ->
            Test.fail "expected async exception to propagate, but task returned a result"

      it "propagates ThreadKilled (does not catch it)" \_ -> do
        readyBarrier <- Task.fromIO GhcMVar.newEmptyMVar
        neverVar <- Task.fromIO GhcMVar.newEmptyMVar
        handle <- Task.fromIO (GhcAsync.async (do { GhcMVar.putMVar readyBarrier (); Task.runResult (Task.asResultSafe @Text (Task.fromIO (GhcMVar.readMVar neverVar))) }))
        Task.fromIO (GhcMVar.takeMVar readyBarrier)
        let threadId = GhcAsync.asyncThreadId handle
        Task.fromIO (GhcException.throwTo threadId GhcException.ThreadKilled)
        outcome <- Task.fromIO (GhcAsync.waitCatch handle)
        case outcome of
          Either.Left someException ->
            GhcException.fromException @GhcException.AsyncException someException
              |> shouldSatisfy (\m -> case m of { Just GhcException.ThreadKilled -> True; _ -> False })
          Either.Right _ ->
            Test.fail "expected ThreadKilled to propagate, but task returned a result"

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
