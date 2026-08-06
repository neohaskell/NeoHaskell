-- | Decider unit tests for @IncrementCounter.decide@ — the pure command handler.
--
-- This is the fastest, most numerous test layer: it exercises the accept/reject
-- rules of a command in isolation, with no HTTP and no event store. Rejections are
-- listed before the happy path so the data types are forced into shape first.
--
-- Add a new command? Copy this file to @test/Decider/<Context>/<Command>Spec.hs@;
-- hspec-discover (see @test/Spec.hs@) picks it up automatically.
module Decider.Counter.IncrementCounterSpec (spec) where

import Array qualified
import Core
import Decider qualified
import Service.Auth qualified as Auth
import Service.Command.Core (DecisionContext (..))
import Starter.Counter.Commands.IncrementCounter (IncrementCounter (..), decide)
import Starter.Counter.Entity (CounterEntity (..))
import Test
import Uuid qualified


-- | Run a @decide@ result to a concrete accept/reject decision for assertions.
runTestDecision :: Decision event -> Task Text (CommandResult event)
runTestDecision decision = do
  let decisionCtx = DecisionContext {genUuid = Uuid.generate}
  Decider.runDecision decisionCtx decision


spec :: Spec Unit
spec = describe "IncrementCounter.decide" do
  it "rejects when the counter does not exist" \_ctx -> do
    let cmd = IncrementCounter {entityId = Uuid.nil, amount = 1}
    result <- runTestDecision (decide cmd Nothing Auth.emptyContext)
    case result of
      RejectCommand msg ->
        msg |> shouldBe "Counter not found"
      AcceptCommand _ _ ->
        fail "expected a rejection when the counter does not exist"

  it "rejects a non-positive amount" \_ctx -> do
    let existing = CounterEntity {counterId = Uuid.nil, label = "downloads", value = 0}
    let cmd = IncrementCounter {entityId = Uuid.nil, amount = 0}
    result <- runTestDecision (decide cmd (Just existing) Auth.emptyContext)
    case result of
      RejectCommand msg ->
        msg |> shouldBe "Amount must be positive"
      AcceptCommand _ _ ->
        fail "expected a rejection for a non-positive amount"

  it "accepts a positive amount and emits a single event" \_ctx -> do
    let existing = CounterEntity {counterId = Uuid.nil, label = "downloads", value = 3}
    let cmd = IncrementCounter {entityId = Uuid.nil, amount = 5}
    result <- runTestDecision (decide cmd (Just existing) Auth.emptyContext)
    case result of
      RejectCommand _ ->
        fail "expected acceptance for a positive amount"
      AcceptCommand insertionType events -> do
        insertionType |> shouldBe ExistingStream
        Array.length events |> shouldBe 1
