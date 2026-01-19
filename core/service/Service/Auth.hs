-- | Request context for command authentication.
--
-- This module provides the 'RequestContext' type that is passed to all
-- command decide functions, enabling authorization decisions in the domain layer.
--
-- = Design Philosophy
--
-- * **Uniform signature**: ALL commands receive RequestContext, no type-level complexity
-- * **Explicit context**: Type signature shows what's available
-- * **Domain-driven auth**: Commands decide their own authorization logic
--
-- = Usage
--
-- @
-- -- Anonymous-friendly (ignore context)
-- decide :: CreateCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
-- decide _ entity _ctx = ...
--
-- -- Auth-required
-- decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
-- decide cmd entity ctx = case ctx.user of
--   Nothing -> Decider.reject "Authentication required"
--   Just user | entity.ownerId /= user.sub -> Decider.reject "Not your cart"
--   Just user -> Decider.acceptExisting [...]
-- @
module Service.Auth (
  -- * Request Context
  RequestContext (..),

  -- * User Identity (re-exported from Auth.Claims)
  UserClaims (..),

  -- * Building Context
  emptyContext,
  anonymousContext,
  authenticatedContext,
) where

import Auth.Claims (UserClaims (..))
import Basics
import Data.Time (UTCTime)
import Data.Time.Calendar qualified as GhcCalendar
import Data.Time.Clock qualified as GhcClock
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Uuid (Uuid)
import Uuid qualified


-- | Context for command execution.
--
-- Available to ALL decide functions regardless of auth requirements.
-- This provides a uniform interface - commands decide what auth logic they need.
data RequestContext = RequestContext
  { user :: Maybe UserClaims,
    -- ^ User identity from validated JWT. Nothing if anonymous/unauthenticated.
    requestId :: Uuid,
    -- ^ Unique identifier for this request (for tracing/logging)
    timestamp :: UTCTime
    -- ^ When the request was received
  }
  deriving (Generic, Show)


-- | Create an empty context for testing or internal use.
-- Uses a nil UUID and Unix epoch timestamp.
emptyContext :: RequestContext
emptyContext =
  RequestContext
    { user = Nothing,
      requestId = Uuid.nil,
      timestamp = GhcClock.UTCTime (GhcCalendar.fromGregorian 1970 1 1) 0
    }


-- | Create an anonymous context with generated request ID and current timestamp.
anonymousContext :: Task err RequestContext
anonymousContext = do
  reqId <- Uuid.generate
  now <- GhcClock.getCurrentTime |> Task.fromIO
  Task.yield
    RequestContext
      { user = Nothing,
        requestId = reqId,
        timestamp = now
      }


-- | Create an authenticated context with the given user claims.
authenticatedContext :: UserClaims -> Task err RequestContext
authenticatedContext claims = do
  reqId <- Uuid.generate
  now <- GhcClock.getCurrentTime |> Task.fromIO
  Task.yield
    RequestContext
      { user = Just claims,
        requestId = reqId,
        timestamp = now
      }
