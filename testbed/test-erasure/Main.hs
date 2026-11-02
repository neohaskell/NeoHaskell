{-| Verify that the example application's owner check survives both auth modes.
Run this module through the @nhtestbed-erasure@ test suite.
-}
module Main (main) where

import Array qualified
import Core
import Decider (Decision (..))
import Decider qualified
import Erasable qualified
import Integration qualified
import Json qualified
import Service.Auth (UserClaims (..))
import Service.Auth qualified as Auth
import Service.Entity.Core qualified as Entity
import Service.Query.Core qualified as Query
import Task qualified
import Test
import Test.Hspec qualified as Hspec
import Testbed.Erasable.Core (ProtectedCreated (..), ProtectedEntity (..), ProtectedEvent (..), SecretPayload (..))
import Testbed.Erasable.Query (ProtectedView (..))
import Testbed.Erasable.Owned.Commands.Record (RecordErasureResult (..))
import Testbed.Erasable.Owned.Commands.Request qualified as Request
import Testbed.Erasable.Owned.Core (OwnedProtectedEntity (..))
import Text qualified
import Uuid qualified


main :: IO ()
main = Hspec.hspec spec


spec :: Spec Unit
spec = do
  describe "Generated protected query" do
    it "redacts nested optional values when its generated Show instance displays a reconstructed view" \_ -> do
      let decision = do
            value <- Erasable.new SecretPayload
              { name = "nested-private-name",
                aliases = [Nothing, Just "nested-private-alias"]
              }
            checked <- Erasable.map
              (\payload ->
                if payload.name == "nested-private-name" && payload.aliases == [Nothing, Just "nested-private-alias"]
                  then payload
                  else panic "Nested alias was missing from the protected payload")
              value
            verified <- Erasable.map
              (\payload ->
                if payload.name != "nested-private-name"
                  then panic "Mapped protected name was missing"
                  else if payload.aliases != [Nothing, Just "nested-private-alias"]
                    then panic "Mapped nested alias was missing"
                    else payload)
              checked
            let probe = ProtectedCreated
                  { createdId = Uuid.nil,
                    createdFirst = verified,
                    createdSecond = verified,
                    createdMapped = verified
                  }
            let probeEntity = Entity.updateImpl (ProtectedEventProtectedCreated probe) (Entity.initialStateImpl @ProtectedEntity)
            case Query.combine probeEntity Nothing :: Query.QueryAction ProtectedView of
              Query.Update view ->
                case Json.decode @ProtectedView (Json.toJSON view) of
                  Err _ -> panic "Generated query did not decode its protected fields"
                  Ok decoded ->
                    case decoded.firstSecret of
                      Nothing -> panic "Generated query lost its first protected field"
                      Just protected -> do
                        _ <- Erasable.map
                          (\payload ->
                            if payload.name != "nested-private-name"
                              then panic "Generated query lost the protected name"
                              else if payload.aliases != [Nothing, Just "nested-private-alias"]
                                then panic "Generated query lost the nested alias"
                                else payload)
                          protected
                        Accept AnyStreamState [verified]
              _ -> panic "Generated query was not updated"
      (result, _) <- Decider.runDecisionWithKeys
        (Decider.DecisionContext {genUuid = Uuid.generate})
        (\_ -> Task.throw "No stored key")
        decision
      case result of
        Decider.RejectCommand _ -> fail "Expected protected value"
        Decider.AcceptCommand _ values -> case Array.first values of
          Nothing -> fail "Expected protected value"
          Just value -> do
            let created = ProtectedCreated
                  { createdId = Uuid.nil,
                    createdFirst = value,
                    createdSecond = value,
                    createdMapped = value
                  }
            let event = ProtectedEventProtectedCreated created
            case Json.decode @ProtectedEvent (Json.toJSON event) of
              Err _ -> fail "Generated event did not decode its protected fields"
              Ok decodedEvent -> do
                case decodedEvent of
                  ProtectedEventProtectedCreated decoded -> do
                    expectProtectedField value decoded.createdFirst
                    expectProtectedField value decoded.createdSecond
                    expectProtectedField value decoded.createdMapped
                let entity = Entity.updateImpl decodedEvent (Entity.initialStateImpl @ProtectedEntity)
                expectProtectedFields entity.firstSecret entity.secondSecret entity.mappedSecret value
                case Query.combine entity Nothing :: Query.QueryAction ProtectedView of
                  Query.Update view -> do
                    expectProtectedFields view.firstSecret view.secondSecret view.mappedSecret value
                    let stored = Json.toJSON view
                    case Json.decode @ProtectedView stored of
                      Err _ -> fail "Generated query did not decode its protected fields"
                      Ok decoded -> do
                        expectProtectedFields decoded.firstSecret decoded.secondSecret decoded.mappedSecret value
                        let displayed = Text.fromLinkedList (show decoded)
                        displayed |> shouldSatisfy (Text.contains "Erasable <REDACTED>")
                        displayed |> shouldSatisfy (\output -> not (Text.contains "nested-private-name" output))
                        displayed |> shouldSatisfy (\output -> not (Text.contains "nested-private-alias" output))
                        displayed |> shouldSatisfy (\output -> not (Text.contains (Erasable.reference value) output))
                        displayed |> shouldSatisfy (\output -> not (Text.contains "$erasable" output))
                  Query.Delete -> fail "Protected query was deleted"
                  Query.NoOp -> fail "Protected query was not updated"

  describe "Protected erasure result command" do
    it "keeps its protected payload out of Show" \_ -> do
      let decision = do
            value <- Erasable.new SecretPayload
              { name = "private-command-value",
                aliases = [Nothing]
              }
            Accept AnyStreamState [value]
      (result, _) <- Decider.runDecisionWithKeys
        (Decider.DecisionContext {genUuid = Uuid.generate})
        (\_ -> Task.throw "No stored key")
        decision
      case result of
        Decider.RejectCommand _ -> fail "Expected protected value"
        Decider.AcceptCommand _ values -> case Array.first values of
          Nothing -> fail "Expected protected value"
          Just value -> do
            let command = RecordErasureSuccess
                  { caseId = Uuid.nil,
                    reference = Erasable.reference value,
                    wasAlreadyErased = False,
                    rejectResult = False,
                    throwResult = False,
                    staleFirst = value
                  }
            let payload = Integration.makeCommandPayload command
            let displayed = Text.fromLinkedList (show payload)
            displayed |> shouldSatisfy (\text -> not (Text.contains (Erasable.reference value) text))
            displayed |> shouldSatisfy (\text -> not (Text.contains "$erasable" text))
            displayed |> shouldSatisfy (\text -> not (Text.contains "private-command-value" text))
            payload.commandType |> shouldBe "RecordErasureResult"
            case Json.decode @RecordErasureResult payload.commandData of
              Err _ -> fail "Protected result command did not decode"
              Ok _ -> pass

  describe "Owned protected erasure authorization" do
    it "rejects a different authenticated owner" \_ -> do
      let entity = (def :: OwnedProtectedEntity) {caseId = Uuid.nil, ownerId = "owner"}
      let claims = UserClaims
            { sub = "other-user",
              email = Nothing,
              name = Nothing,
              permissions = [],
              tenantId = Nothing,
              rawClaims = []
            }
      context <- Auth.authenticatedContext claims
      let command = Request.RequestFirstErasure
            { caseId = Uuid.nil,
              reference = "owner-reference",
              rejectResult = False,
              simulateUnknownReference = False
            }
      result <- Decider.runDecision (Decider.DecisionContext {genUuid = Uuid.generate}) (Request.decide command (Just entity) context)
      case result of
        Decider.RejectCommand reason -> reason |> shouldBe "Protected record is not owned by this user"
        Decider.AcceptCommand _ _ -> fail "Another user accepted the erasure request"

    it "rejects a trusted transport without a user" \_ -> do
      let entity = (def :: OwnedProtectedEntity) {caseId = Uuid.nil, ownerId = "owner"}
      context <- Auth.trustedContext
      let command = Request.RetryFirstErasure {caseId = Uuid.nil}
      result <- Decider.runDecision (Decider.DecisionContext {genUuid = Uuid.generate}) (Request.decide command (Just entity) context)
      case result of
        Decider.RejectCommand reason -> reason |> shouldBe "Authentication required"
        Decider.AcceptCommand _ _ -> fail "A userless transport accepted the erasure request"


expectProtectedFields :: Maybe (Erasable.Erasable SecretPayload) -> Maybe (Erasable.Erasable SecretPayload) -> Maybe (Erasable.Erasable SecretPayload) -> Erasable.Erasable SecretPayload -> Task Text Unit
expectProtectedFields first second mapped original =
  case (first, second, mapped) of
    (Just firstValue, Just secondValue, Just mappedValue) -> do
      expectProtectedField original firstValue
      expectProtectedField original secondValue
      expectProtectedField original mappedValue
    _ -> fail "A protected field was missing"


expectProtectedField :: Erasable.Erasable SecretPayload -> Erasable.Erasable SecretPayload -> Task Text Unit
expectProtectedField expected actual = do
  Erasable.reference actual |> shouldBe (Erasable.reference expected)
  (Json.toJSON actual == Json.toJSON expected) |> shouldBe True
