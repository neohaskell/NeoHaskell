module AsyncTask.RaceSpec (spec) where

import AsyncTask (RaceWinner (..))
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "AsyncTask.race" do
    it "left task wins when it finishes first" \_ -> do
      let leftTask = Task.yield (1 :: Int)
      let rightTask = AsyncTask.sleep 10000 |> Task.andThen (\_ -> Task.yield (2 :: Int))
      result <- AsyncTask.race leftTask rightTask
      case result of
        LeftWon val -> val |> shouldBe 1
        RightWon _ -> Test.fail "expected left to win"

    it "right task wins when it finishes first" \_ -> do
      let leftTask = AsyncTask.sleep 10000 |> Task.andThen (\_ -> Task.yield (1 :: Int))
      let rightTask = Task.yield (2 :: Int)
      result <- AsyncTask.race leftTask rightTask
      case result of
        RightWon val -> val |> shouldBe 2
        LeftWon _ -> Test.fail "expected right to win"

    it "loser is cancelled before its deferred effect occurs" \_ -> do
      effectVar <- ConcurrentVar.containing (False :: Bool)
      let loserTask = do
            AsyncTask.sleep 200
            ConcurrentVar.swap True effectVar
            Task.yield unit
      let winnerTask = Task.yield (42 :: Int)
      _ <- AsyncTask.race winnerTask loserTask
      -- Give the loser a short window to write if it were not cancelled
      AsyncTask.sleep 400
      written <- ConcurrentVar.peek effectVar
      written |> shouldBe False

    it "error in the winning task propagates" \_ -> do
      let failingTask = Task.throw ("winner-error" :: Text)
      let slowTask = AsyncTask.sleep 10000 |> Task.andThen (\_ -> Task.yield unit)
      result <- AsyncTask.race failingTask slowTask |> Task.asResult
      case result of
        Err err -> err |> shouldBe "winner-error"
        Ok _ -> Test.fail "expected the error from the winning task to propagate"

    it "error in the losing task is ignored when winner succeeds" \_ -> do
      let winnerTask = Task.yield (99 :: Int)
      let loserTask = AsyncTask.sleep 10000 |> Task.andThen (\_ -> Task.throw ("loser-error" :: Text))
      result <- AsyncTask.race winnerTask loserTask
      case result of
        LeftWon val -> val |> shouldBe 99
        RightWon _ -> Test.fail "expected left to win"
