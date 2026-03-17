{-# OPTIONS_GHC -Wno-unused-imports #-}

module TaskSpec (spec) where

import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.Async qualified as GhcAsync
import Control.Concurrent.MVar qualified as GhcMVar
import Control.Exception qualified as GhcException
import Data.Either qualified as Either
import Core
import Maybe qualified
import Result (Result (..))
import Result qualified
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Task" do
    describe "asResultSafe" do
      it "catches synchronous IO exceptions as Err" \_ -> do
        let action = Task.fromIO (GhcException.throwIO (GhcException.ErrorCall "sync error"))
        result <- Task.fromIO (Task.runResult (Task.asResultSafe @Text action))
        case result of
          Ok innerResult -> (innerResult :: Result Text Unit) |> shouldSatisfy Result.isErr
          Err _ -> Test.fail "outer Task should not fail"

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
            GhcException.fromException @GhcException.SomeAsyncException someException
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
            GhcException.fromException @GhcException.SomeAsyncException someException
              |> shouldSatisfy (\m -> case m of { Just _ -> True; Nothing -> False })
          Either.Right _ ->
            Test.fail "expected ThreadKilled to propagate, but task returned a result"
