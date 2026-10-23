{-# OPTIONS_GHC -Wno-unused-imports #-}

module TaskSpec (spec) where

import AsyncTask qualified
import Channel (Channel)
import Channel qualified
import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.Async qualified as GhcAsync
import Control.Concurrent.MVar qualified as GhcMVar
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
