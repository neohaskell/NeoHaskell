module Auth.OAuth2.AuditLogSpec (spec) where

import Auth.OAuth2.AuditLog (AuthAuditEvent (..), AuthEventType (..), logAuditEvent, mkAuditEvent)
import Auth.OAuth2.Types (TokenSet (..), mkAccessToken)
import Char (isDigit)
import Core
import Result qualified
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "mkAuditEvent" do
    it "creates an audit event with a SHA-256 hashed token ID (64 hex chars)" \_ -> do
      let rawToken = "raw-access-token"
      event <- mkAuditEvent "actor-1" TokenIssued (mkTokenSet rawToken)
      case event of
        AuthAuditEvent {tokenId = eventTokenId} -> do
          -- SHA-256 produces 64 lowercase hex characters
          Text.length eventTokenId |> shouldBe 64
          eventTokenId |> shouldSatisfy isLowercaseHex
          -- Must not be the raw token
          eventTokenId |> shouldNotBe rawToken

    it "produces deterministic token hash for same input" \_ -> do
      let rawToken = "deterministic-test-token"
      event1 <- mkAuditEvent "actor-a" TokenIssued (mkTokenSet rawToken)
      event2 <- mkAuditEvent "actor-b" TokenIssued (mkTokenSet rawToken)
      case (event1, event2) of
        (AuthAuditEvent {tokenId = tid1}, AuthAuditEvent {tokenId = tid2}) ->
          tid1 |> shouldBe tid2

    it "records the correct event type" \_ -> do
      event <- mkAuditEvent "actor-2" TokenRefreshed (mkTokenSet "refresh-token")
      case event of
        AuthAuditEvent {eventType = actualType} ->
          actualType |> shouldBe TokenRefreshed

    it "pseudonymizes actorId with SHA-256 hash (64 hex chars)" \_ -> do
      event <- mkAuditEvent "actor-3" TokenRevoked (mkTokenSet "revoke-token")
      case event of
        AuthAuditEvent {actorId = actualActorId} -> do
          -- actorId is now SHA-256 hashed, not stored verbatim
          Text.length actualActorId |> shouldBe 64
          actualActorId |> shouldSatisfy isLowercaseHex
          -- Must not be the raw actor ID
          actualActorId |> shouldNotBe "actor-3"

    it "produces deterministic actor hash for same input" \_ -> do
      event1 <- mkAuditEvent "same-actor" TokenIssued (mkTokenSet "token-1")
      event2 <- mkAuditEvent "same-actor" TokenRefreshed (mkTokenSet "token-2")
      case (event1, event2) of
        (AuthAuditEvent {actorId = aid1}, AuthAuditEvent {actorId = aid2}) ->
          aid1 |> shouldBe aid2

  describe "logAuditEvent" do
    it "completes without error" \_ -> do
      event <- mkAuditEvent "actor-4" TokenIssued (mkTokenSet "log-token")
      result <- logAuditEvent event |> Task.asResult
      result |> shouldSatisfy Result.isOk


-- | Check if a text is valid lowercase hexadecimal (0-9, a-f).
isLowercaseHex :: Text -> Bool
isLowercaseHex txt = do
  let isHexChar c = isDigit c || (c >= 'a' && c <= 'f')
  Text.all isHexChar txt


mkTokenSet :: Text -> TokenSet
mkTokenSet accessToken =
  TokenSet
    { accessToken = mkAccessToken accessToken
    , refreshToken = Nothing
    , expiresInSeconds = Nothing
    , expiresAt = Nothing
    , ttl = Nothing
    }
