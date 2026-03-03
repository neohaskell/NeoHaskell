module Auth.OAuth2.AuditLogSpec (spec) where

import Auth.OAuth2.AuditLog (AuthAuditEvent (..), AuthEventType (..), logAuditEvent, mkAuditEvent)
import Auth.OAuth2.Types (TokenSet (..), mkAccessToken)
import Core
import Result qualified
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "mkAuditEvent" do
    it "creates an audit event with a hashed token ID" \_ -> do
      let rawToken = "raw-access-token"
      event <- mkAuditEvent "actor-1" TokenIssued (mkTokenSet rawToken)
      case event of
        AuthAuditEvent {tokenId = eventTokenId} -> do
          eventTokenId |> shouldSatisfy (\tokenId -> tokenId != "")
          eventTokenId |> shouldNotBe rawToken

    it "records the correct event type" \_ -> do
      event <- mkAuditEvent "actor-2" TokenRefreshed (mkTokenSet "refresh-token")
      case event of
        AuthAuditEvent {eventType = actualType} ->
          actualType |> shouldBe TokenRefreshed

    it "records the correct actor ID" \_ -> do
      event <- mkAuditEvent "actor-3" TokenRevoked (mkTokenSet "revoke-token")
      case event of
        AuthAuditEvent {actorId = actualActorId} ->
          actualActorId |> shouldBe "actor-3"

  describe "logAuditEvent" do
    it "completes without error" \_ -> do
      event <- mkAuditEvent "actor-4" TokenIssued (mkTokenSet "log-token")
      result <- logAuditEvent event |> Task.asResult
      result |> shouldSatisfy Result.isOk


mkTokenSet :: Text -> TokenSet
mkTokenSet accessToken =
  TokenSet
    { accessToken = mkAccessToken accessToken
    , refreshToken = Nothing
    , expiresInSeconds = Nothing
    , expiresAt = Nothing
    , ttl = Nothing
    }
