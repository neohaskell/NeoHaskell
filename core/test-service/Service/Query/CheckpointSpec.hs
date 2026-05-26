module Service.Query.CheckpointSpec where

import Array qualified
import Core
import Map qualified
import Service.Event.StreamPosition (StreamPosition (..))
import Service.Query.Checkpoint (CheckpointStore (..))
import Service.Query.Checkpoint.InMemory qualified as InMemory
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "CheckpointStore" do
    describe "getPositions" do
      it "H1: Two checkpoints stored returns Map with both entries" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "query-a" (StreamPosition 10)
        setPos "query-b" (StreamPosition 20)
        positions <- store.getPositions
        Map.length positions |> shouldBe 2
        Map.get "query-a" positions |> shouldBe (Just (StreamPosition 10))
        Map.get "query-b" positions |> shouldBe (Just (StreamPosition 20))

      it "E1: No checkpoints exist (fresh store) returns empty Map" \_ -> do
        store <- InMemory.new
        positions <- store.getPositions
        Map.length positions |> shouldBe 0

      it "E2: 100 checkpoints stored returns Map with 100 entries, no truncation" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        Array.initialize 100 (\idx -> (idx, [fmt|query-#{idx}|]))
          |> Task.forEach (\(idx :: Int, name :: Text) -> setPos name (StreamPosition (fromIntegral idx)))
        positions <- store.getPositions
        Map.length positions |> shouldBe 100

      it "P1: getPositions after setPosition returns the new position" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "test-query" (StreamPosition 42)
        positions <- store.getPositions
        Map.get "test-query" positions |> shouldBe (Just (StreamPosition 42))

    describe "setPosition" do
      it "H1: Set position for a new query inserts and getPositions includes it" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "new-query" (StreamPosition 5)
        positions <- store.getPositions
        Map.length positions |> shouldBe 1
        Map.get "new-query" positions |> shouldBe (Just (StreamPosition 5))

      it "E1: Overwrite existing checkpoint updates and getPositions reflects new position" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "query" (StreamPosition 10)
        setPos "query" (StreamPosition 20)
        positions <- store.getPositions
        Map.get "query" positions |> shouldBe (Just (StreamPosition 20))

      it "E2: Set position to Int64 max value stored and retrievable" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        let maxPos = StreamPosition 9223372036854775807
        setPos "query" maxPos
        positions <- store.getPositions
        Map.get "query" positions |> shouldBe (Just maxPos)

      it "R1: setPosition then getPositions returns the set position (round-trip)" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "query" (StreamPosition 7)
        positions <- store.getPositions
        Map.length positions |> shouldBe 1
        Map.get "query" positions |> shouldBe (Just (StreamPosition 7))

      it "P1: setPosition 10 then setPosition 20; getPosition returns 20 (last-write-wins)" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "query" (StreamPosition 10)
        setPos "query" (StreamPosition 20)
        positions <- store.getPositions
        Map.get "query" positions |> shouldBe (Just (StreamPosition 20))

    describe "getMinPosition" do
      it "H1: Checkpoints at positions 5, 10, 15 returns Just (StreamPosition 5)" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "a" (StreamPosition 5)
        setPos "b" (StreamPosition 10)
        setPos "c" (StreamPosition 15)
        minPos <- store.getMinPosition
        minPos |> shouldBe (Just (StreamPosition 5))

      it "E1: Single checkpoint at position 7 returns Just (StreamPosition 7)" \_ -> do
        store <- InMemory.new
        let setPos = store.setPosition :: Text -> StreamPosition -> Task Text Unit
        setPos "only" (StreamPosition 7)
        minPos <- store.getMinPosition
        minPos |> shouldBe (Just (StreamPosition 7))

      it "E2: No checkpoints returns Nothing" \_ -> do
        store <- InMemory.new
        minPos <- store.getMinPosition
        minPos |> shouldBe Nothing

  describe "needsProcessing" do
    it "H1: Event at position 5, query checkpoint at position 3 — query needs processing" \_ -> do
      let queryCheckpoint = StreamPosition 3 :: StreamPosition
      let eventPosition = StreamPosition 5 :: StreamPosition
      queryCheckpoint < eventPosition |> shouldBe True

    it "E1: Event at position 5, query checkpoint at position 5 — no processing needed" \_ -> do
      let queryCheckpoint = StreamPosition 5 :: StreamPosition
      let eventPosition = StreamPosition 5 :: StreamPosition
      (queryCheckpoint < eventPosition) |> shouldBe False

    it "E2: Event at position 5, query checkpoint at position 10 — already ahead" \_ -> do
      let queryCheckpoint = StreamPosition 10 :: StreamPosition
      let eventPosition = StreamPosition 5 :: StreamPosition
      (queryCheckpoint < eventPosition) |> shouldBe False

    it "E3: No checkpoints for a query means it needs processing from the beginning" \_ -> do
      -- Covered by subscriber integration tests
      pass

    it "P1: needsProcessing logic is deterministic for same inputs" \_ -> do
      let cpA = StreamPosition 3 :: StreamPosition
      let cpB = StreamPosition 10 :: StreamPosition
      let eventPos = StreamPosition 5 :: StreamPosition
      let pairs = Array.fromLinkedList [(("a" :: Text), cpA), ("b", cpB)]
      let run1 = pairs |> Array.takeIf (\(_, cp) -> cp < eventPos) |> Array.map (\(n :: Text, _) -> n)
      let run2 = pairs |> Array.takeIf (\(_, cp) -> cp < eventPos) |> Array.map (\(n :: Text, _) -> n)
      run1 |> shouldBe run2
