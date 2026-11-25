module Property.CounterReplaySpec (spec) where

import Core
import Prelude qualified as GhcPrelude
import Starter.Counter.Entity (CounterEntity (..), initialState, update)
import Starter.Counter.Events.CounterCreated (CounterCreated (..))
import Starter.Counter.Events.CounterIncremented (CounterIncremented (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.QuickCheck qualified as HspecQC
import Test.QuickCheck qualified as QC
import Uuid qualified


spec :: Hspec.Spec
spec = Hspec.describe "CounterEntity replay" do
  HspecQC.prop "value equals the total of the replayed increments" \(amounts :: [QC.Positive Int]) -> do
    let counterId = Uuid.nil
    let created = event CounterCreated {entityId = counterId, label = "downloads"}
    let increments =
          amounts
            |> GhcPrelude.map \(QC.Positive a) ->
              event CounterIncremented {entityId = counterId, amount = a}
    let events = created : increments
    let finalState = GhcPrelude.foldl (\entity event -> update event entity) initialState events
    let expected = GhcPrelude.sum (GhcPrelude.map (\(QC.Positive a) -> a) amounts)
    finalState.value QC.=== expected
