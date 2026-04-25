module DeciderSpec (spec) where

import Core
import Decider qualified
import Test
import Uuid qualified
import Maybe qualified
import Task qualified
import Array qualified

spec :: Spec Unit
spec = do
  describe "Decider" do
    describe "generateDeterministicUuid" do
      it "generates the same UUID for the same input" \_ -> do
        let nsText = "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
        case Uuid.fromText nsText of
          Nothing -> Test.fail [fmt|Failed to parse namespace UUID: #{nsText}|]
          Just ns -> do
            let name = "www.example.com"
            let expected = Uuid.fromText "2ed6657d-e927-568b-95e1-2665a8aea6a2" |> Maybe.withDefault Uuid.nil
            
            result <- do
                        uuid <- Decider.generateDeterministicUuid ns name
                        Decider.acceptAny @Uuid [uuid]
                      |> Decider.runDecision (Decider.DecisionContext { Decider.genUuid = Task.yield Uuid.nil })
            
            case result of
              Decider.AcceptCommand _ events -> do
                 let generated = events |> Array.get 0 |> Maybe.withDefault Uuid.nil
                 generated |> shouldBe expected
              _ -> Test.fail "Decision rejected"

      it "handles unicode characters" \_ -> do
        let ns = Uuid.nil
        let name = "🚀 NeoHaskell"
        result <- do
                    uuid <- Decider.generateDeterministicUuid ns name
                    Decider.acceptAny @Uuid [uuid]
                  |> Decider.runDecision (Decider.DecisionContext { Decider.genUuid = Task.yield Uuid.nil })
        case result of
           Decider.AcceptCommand _ _ -> shouldBe True True
           _ -> Test.fail "Decision rejected"
