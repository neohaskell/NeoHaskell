-- | Verify queue retirement, replacement, and writer admission behavior.
--
-- The worker queue test runner calls 'spec' to exercise these transitions.
module WorkerQueueControlSpec (spec) where

import AsyncTask qualified
import Channel qualified
import ConcurrentMap qualified
import Core
import Service.Integration.Dispatcher.WorkerQueueControl qualified as WorkerQueueControl
import Task qualified
import Test


data TestWorker = TestWorker
  { workerId :: Int,
    queue :: WorkerQueueControl.WorkerQueueControl
  }


spec :: Spec Unit
spec =
  describe "Integration.WorkerQueueControl" do
    it "keeps a replacement and waits for an admitted writer before stop" \_ -> do
      oldQueue <- WorkerQueueControl.new
      replacementQueue <- WorkerQueueControl.new
      workers <- ConcurrentMap.new @Text @TestWorker
      ConcurrentMap.set "stream" TestWorker {workerId = 1, queue = oldQueue} workers

      writerEntered <- Channel.new
      releaseWriter <- Channel.new
      let write :: Task Text Unit
          write = do
            Channel.write unit writerEntered
            Channel.read releaseWriter
      writer <- AsyncTask.run (WorkerQueueControl.withAdmission oldQueue write)
      Channel.read writerEntered

      removed <- workers |> ConcurrentMap.removeIfM "stream" (\current ->
        if current.workerId == 1
          then WorkerQueueControl.markDraining current.queue
          else pure False
        )
      case removed of
        Just current -> current.workerId |> shouldBe 1
        Nothing -> Test.fail "expected the captured worker to be removed"

      ConcurrentMap.set "stream" TestWorker {workerId = 2, queue = replacementQueue} workers
      staleRemoval <- workers |> ConcurrentMap.removeIfM "stream" (\current ->
        if current.workerId == 1
          then WorkerQueueControl.markDraining current.queue
          else pure False
        )
      case staleRemoval of
        Nothing -> Task.yield unit
        Just _ -> Test.fail "stale reaper removed the replacement worker"
      current <- workers |> ConcurrentMap.get "stream"
      case current of
        Just replacement -> replacement.workerId |> shouldBe 2
        Nothing -> Test.fail "replacement worker disappeared"

      oldAdmission <- WorkerQueueControl.withAdmission oldQueue (Task.yield unit)
      oldAdmission |> shouldBe Nothing
      replacementAdmission <- WorkerQueueControl.withAdmission replacementQueue (Task.yield unit)
      replacementAdmission |> shouldBe (Just unit)

      claimed <- WorkerQueueControl.claimStop oldQueue
      claimed |> shouldBe True
      let stop :: Task Text Unit
          stop = do
            WorkerQueueControl.waitForWriters oldQueue
            WorkerQueueControl.finishStop oldQueue
      stopper <- AsyncTask.run stop
      waitForWriterBarrier stopper 1000
      Channel.write unit releaseWriter
      writerResult <- AsyncTask.waitFor writer
      writerResult |> shouldBe (Just unit)
      AsyncTask.waitFor stopper
      stoppedAdmission <- WorkerQueueControl.withAdmission oldQueue (Task.yield unit)
      stoppedAdmission |> shouldBe Nothing


waitForWriterBarrier :: AsyncTask.AsyncTask Text Unit -> Int -> Task Text Unit
waitForWriterBarrier stopper attempts = do
  waiting <- AsyncTask.isWaitingForStateChange stopper
  Task.unless waiting do
    Task.when (attempts == 0) (Test.fail "stop completed before the admitted writer left")
    AsyncTask.sleep 1
    waitForWriterBarrier stopper (attempts - 1)
