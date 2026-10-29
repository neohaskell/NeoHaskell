{-| Verify that the example application's owner check survives both auth modes.
Run this module through the @nhtestbed-erasure@ test suite.
-}
module Main (main) where

import Core
import Decider qualified
import Service.Auth (UserClaims (..))
import Service.Auth qualified as Auth
import Test
import Test.Hspec qualified as Hspec
import Testbed.Erasable.Owned.Commands.Request qualified as Request
import Testbed.Erasable.Owned.Core (OwnedProtectedEntity (..))
import Uuid qualified


main :: IO ()
main = Hspec.hspec spec


spec :: Spec Unit
spec = do
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
              simulateUnknownReference = False,
              pauseAfterCommit = False
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
