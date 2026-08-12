module DeciderSpec where

import Core
import Decider (DecisionContext (..))
import Decider qualified
import Maybe qualified
import Result qualified
import Task qualified
import Test
import Uuid qualified


-- | The RFC 4122 DNS namespace — same fixture the published v5 vectors use,
-- so the value asserted here is checkable against any other implementation.
dnsNamespace :: Uuid
dnsNamespace =
  "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
    |> Uuid.fromText
    |> Maybe.getOrDie


-- | A context that CAN supply randomness — the ordinary production shape.
randomContext :: DecisionContext
randomContext = DecisionContext {genUuid = Uuid.generate}


-- | A context whose random source is unavailable. Any decision that touches
-- 'Decider.generateUuid' fails on it; a purely deterministic one must not.
exhaustedContext :: DecisionContext
exhaustedContext = DecisionContext {genUuid = Task.throw "no randomness available"}


-- | Derive a UUID deterministically and surface it through the rejection
-- reason, which is how a 'Decision' can report a value without an event ADT.
deriveThenReject :: Uuid -> Text -> Decision Text
deriveThenReject namespace name = do
  derived <- Decider.generateDeterministicUuid namespace name
  Decider.reject (Uuid.toText derived)


-- | The random counterpart, for the contrast in the no-randomness test.
generateThenReject :: Decision Text
generateThenReject = do
  derived <- Decider.generateUuid
  Decider.reject (Uuid.toText derived)


spec :: Spec Unit
spec = parallel do
  describe "Decider" do
    describe "generateDeterministicUuid" do
      it "generateDeterministicUuid agrees with Uuid.generateV5 and repeats" \_ -> do
        result <- Decider.runDecision randomContext (deriveThenReject dnsNamespace "python.org")

        case result of
          RejectCommand reason ->
            reason
              |> shouldBe (Uuid.generateV5 dnsNamespace "python.org" |> Uuid.toText)
          AcceptCommand _ _ ->
            fail "Expected the decision to terminate with the derived uuid"

        -- Running the same decision again yields the same uuid — a `Decision`
        -- carrying it is as stable as the pure function underneath.
        repeated <- Decider.runDecision randomContext (deriveThenReject dnsNamespace "python.org")
        repeated |> shouldBe result

        -- A different name under the same namespace lands elsewhere.
        other <- Decider.runDecision randomContext (deriveThenReject dnsNamespace "example.org")
        other |> shouldNotBe result

      it "generateDeterministicUuid does not draw from the random uuid source" \_ -> do
        -- The deterministic combinator must succeed even when the context
        -- cannot produce a random uuid at all: it is a `Return`, not an effect.
        deterministic <-
          Decider.runDecision exhaustedContext (deriveThenReject dnsNamespace "python.org")
            |> Task.asResult

        case deterministic of
          Ok (RejectCommand reason) ->
            reason
              |> shouldBe (Uuid.generateV5 dnsNamespace "python.org" |> Uuid.toText)
          Ok (AcceptCommand _ _) ->
            fail "Expected the decision to terminate with the derived uuid"
          Err message ->
            fail [fmt|Deterministic decision must not need randomness, but failed: #{message}|]

        -- The contrast that gives the assertion its teeth: on the very same
        -- context, the random combinator does fail.
        random <-
          Decider.runDecision exhaustedContext generateThenReject
            |> Task.asResult

        (random :: Result Text (CommandResult Text))
          |> Result.isErr
          |> shouldBe True
