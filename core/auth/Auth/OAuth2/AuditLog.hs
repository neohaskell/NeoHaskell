-- | Auth audit logging for GDPR compliance (W5-B, issue #336).
--
-- Provides an immutable audit trail for authentication lifecycle events.
-- All actor IDs are pseudonymous internal identifiers (not directly identifying).
--
-- = Security Notes
--
-- * tokenId is a SHA-256 hash of the token — one-way, non-reversible
-- * actorId should be a pseudonymous internal ID, not a user email or name
-- * Audit events should be stored in an append-only manner
module Auth.OAuth2.AuditLog (
  -- * Types
  AuthAuditEvent (..),
  AuthEventType (..),
  -- * Construction
  mkAuditEvent,
  -- * Logging
  logAuditEvent,
) where

import Auth.OAuth2.Types (TokenSet (..), unwrapAccessToken)
import Core
import Crypto.Hash qualified as Hash
import Data.ByteArray.Encoding qualified as GhcEncoding
import Data.ByteString qualified as GhcBS
import Data.Text.Encoding qualified as GhcTextEncoding
import DateTime qualified
import Log qualified
import Task qualified


-- | Types of authentication lifecycle events.
data AuthEventType
  = TokenIssued
  | TokenRefreshed
  | TokenRevoked
  deriving (Generic, Show, Eq)


-- | An immutable audit record for an authentication event.
--
-- GDPR Note: actorId must be a pseudonymous internal ID.
-- If it is a directly identifying value (email, name), the audit log
-- table must be included in DSAR and erasure workflows.
data AuthAuditEvent = AuthAuditEvent
  { -- | Pseudonymous internal actor identifier.
    actorId :: !Text
  , -- | Type of authentication event.
    eventType :: !AuthEventType
  , -- | SHA-256 hash of the token (one-way, non-reversible).
    tokenId :: !Text
  , -- | When the event occurred.
    occurredAt :: !DateTime
  }
  deriving (Generic, Show, Eq)


-- | Create an audit event by hashing the token from a TokenSet.
-- Uses SHA-256 for the token hash (one-way, non-reversible).
{-# INLINE mkAuditEvent #-}
mkAuditEvent :: Text -> AuthEventType -> TokenSet -> Task Text AuthAuditEvent
mkAuditEvent actorId eventType tokenSet = do
  now <- DateTime.now
  let rawToken = unwrapAccessToken tokenSet.accessToken
  let tokenBytes = GhcTextEncoding.encodeUtf8 rawToken
  let digest = Hash.hash tokenBytes :: Hash.Digest Hash.SHA256
  let hashBytes = GhcEncoding.convertToBase GhcEncoding.Base16 digest :: GhcBS.ByteString
  let tokenId = GhcTextEncoding.decodeUtf8 hashBytes
  Task.yield
    AuthAuditEvent
      { actorId = actorId
      , eventType = eventType
      , tokenId = tokenId
      , occurredAt = now
      }


-- | Log an audit event using structured logging.
logAuditEvent :: AuthAuditEvent -> Task Text Unit
logAuditEvent event = do
  let actor = event.actorId
  let authEventType = show event.eventType
  let token = event.tokenId
  Log.info
    [fmt|auth_audit: actor=#{actor} event=#{authEventType} token_id=#{token}|]
