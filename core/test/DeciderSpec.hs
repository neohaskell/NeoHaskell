module DeciderSpec (spec) where

import Core
import Decider qualified
import Test
import Uuid qualified
import Maybe qualified
import Array qualified
import Task qualified

spec :: Spec Unit
spec = do
  describe "Uuid" do
    describe "generateV5" do
      it "is deterministic: same inputs produce the same UUID" \_ -> do
        let ns = Uuid.fromText "6ba7b810-9dad-11d1-80b4-00c04fd430c8" 
                 |> Maybe.withDefault (panic "invalid test namespace")
        let name = "www.example.com"
        
        let uuid1 = Uuid.generateV5 ns name
        let uuid2 = Uuid.generateV5 ns name
        uuid1 |> shouldBe uuid2

      it "produces the RFC 4122 v5 reference vector (DNS + example.com)" \_ -> do
        let ns = Uuid.fromText "6ba7b810-9dad-11d1-80b4-00c04fd430c8" 
                 |> Maybe.withDefault (panic "invalid test namespace")
        let name = "www.example.com"
        -- Expected UUID for DNS + "www.example.com" is 2ed6657d-e927-568b-95e1-2665a8aea6a2
        let expected = Uuid.fromText "2ed6657d-e927-568b-95e1-2665a8aea6a2" 
                       |> Maybe.withDefault (panic "invalid expected uuid")
        
        Uuid.generateV5 ns name |> shouldBe expected

  describe "Decider" do
    describe "generateDeterministicUuid" do
      it "works when bound in a decision chain" \_ -> do
        let ns = Uuid.nil
        let name = "test"
        let expected = Uuid.generateV5 ns name
        
        result <- do
                    uuid <- Decider.generateDeterministicUuid ns name
                    Decider.acceptAny @Uuid [uuid]
                  |> Decider.runDecision (Decider.DecisionContext { Decider.genUuid = Task.throw "GenUuid should not be called" })
        
        case result of
          Decider.AcceptCommand _ events -> do
             let generated = events |> Array.get 0 |> Maybe.withDefault (panic "no uuid")
             generated |> shouldBe expected
          _ -> Test.fail "Decision rejected"
