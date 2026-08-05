-- | Property tests for the @CounterEntity@ @update@ fold — the event-replay layer.
--
-- Property tests drop to raw hspec + QuickCheck (the @Test@ wrapper's @it@ can't
-- take a QuickCheck property). @Prelude@ is imported qualified as @GhcPrelude@ for
-- the list/fold operations QuickCheck generates over. Generating an event sequence
-- and folding it with @update@ is the canonical NeoHaskell replay-property shape.
module Property.CounterReplaySpec (spec) where

import Core
import Prelude qualified as GhcPrelude
import Starter.Counter.Entity (CounterEntity (..), initialState, update)
import Starter.Counter.Event (CounterEvent (..))
import Starter.Counter.Events.CounterCreated qualified as CounterCreated
import Starter.Counter.Events.CounterIncremented qualified as CounterIncremented
import Test.Hspec qualified as Hspec
import Test.Hspec.QuickCheck qualified as HspecQC
import Test.QuickCheck qualified as QC
import Uuid qualified


spec :: Hspec.Spec
spec = Hspec.describe "CounterEntity replay" do
  HspecQC.prop "value equals the total of the replayed increments" \(amounts :: [QC.Positive Int]) -> do
    let counterId = Uuid.nil
    let created = CounterCreated CounterCreated.Event {entityId = counterId, label = "downloads"}
    let increments =
          amounts
            |> GhcPrelude.map \(QC.Positive a) ->
              CounterIncremented CounterIncremented.Event {entityId = counterId, amount = a}
    let events = created : increments
    let finalState = GhcPrelude.foldl (\entity event -> update event entity) initialState events
    let expected = GhcPrelude.sum (GhcPrelude.map (\(QC.Positive a) -> a) amounts)
    finalState.value QC.=== expected
