module Decider (
  -- * The Decision Monad
  Decision (..),
  DecisionContext (..),
  CommandResult (..),

  -- * Execution
  runDecision,

  -- * Smart Constructors
  generateUuid,
  acceptAny,
  acceptNew,
  acceptExisting,
  acceptAfter,
  reject,
) where

import Applicable
import Array (Array)
import Basics
import Control.Monad qualified as Monad
import Mappable
import Service.Event (InsertionType (..), StreamPosition)
import Task (Task)
import Task qualified
import Text (Text)
import Thenable
import Uuid (Uuid)


-- | The result of running a Decision.
--
-- Either the command is accepted with events to persist, or rejected with a reason.
data CommandResult event
  = AcceptCommand InsertionType (Array event)
  | RejectCommand Text
  deriving (Eq, Show, Ord, Generic)


-- | The Decision monad for expressing command acceptance/rejection logic.
--
-- Decision is a free monad that allows you to:
--
-- * Generate UUIDs via 'generateUuid'
-- * Accept commands with events via 'acceptNew', 'acceptExisting', 'acceptAny', 'acceptAfter'
-- * Reject commands with a reason via 'reject'
--
-- Decisions are pure and can be easily tested without side effects.
data Decision a where
  Return :: a -> Decision a
  Bind :: Decision a -> (a -> Decision b) -> Decision b
  GenUuid :: Decision Uuid
  Accept :: InsertionType -> Array a -> Decision a
  Reject :: Text -> Decision a


instance Functor Decision where
  fmap f m = m |> Thenable.andThen (\r -> f r |> Return)


instance Applicative Decision where
  pure = Return
  (<*>) = Monad.ap


instance Monad Decision where
  m >>= f = Bind m f


-- | Context required to execute a Decision.
--
-- Provides effectful capabilities like UUID generation.
data DecisionContext = DecisionContext
  { genUuid :: Task Text Uuid
  }


-- | Execute a Decision within a Task context, producing a CommandResult.
--
-- The Decision must terminate with either 'Accept' or 'Reject'. A bare 'Return'
-- or an 'Accept'/'Reject' in the middle of a chain will result in an error.
runDecision :: (HasCallStack) => DecisionContext -> Decision a -> Task Text (CommandResult a)
runDecision ctx = go
 where
  go :: forall a. (HasCallStack) => Decision a -> Task Text (CommandResult a)
  go (Return _) = Task.throw "Decision didn't terminate with accept/reject"
  go (Bind m f) = case m of
    GenUuid -> do
      uuid <- ctx.genUuid
      f uuid |> go
    Accept _ _ -> Task.throw "Accept must be the last statement"
    Reject _ -> Task.throw "Reject must be the last statement"
    Return a -> go (f a)
    Bind m' f' -> go (Bind m' (\x -> Bind (f' x) f))
  go GenUuid = Task.throw "Unbound GenUuid"
  go (Accept s events) = AcceptCommand s events |> Task.yield
  go (Reject reason) = RejectCommand reason |> Task.yield


-- | Generate a UUID within a Decision context.
--
-- This allows you to generate unique identifiers when constructing events.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   cartId <- Decision.generateUuid
--   Decision.acceptNew [CartCreated {cartCreatedId = cartId}]
-- @
generateUuid :: Decision Uuid
generateUuid = GenUuid


-- | Accept a command regardless of stream state, emitting the given events.
--
-- Use this when you don't care whether the entity exists or not. This is the least
-- safe option as it doesn't enforce any stream state constraints.
--
-- Example:
--
-- @
-- decide cmd entity =
--   Decision.acceptAny [EventOccurred]
-- @
acceptAny :: (Array a) -> Decision a
acceptAny events =
  Accept AnyStreamState events


-- | Accept a command only if the stream doesn't exist yet, emitting the given events.
--
-- Use this for entity creation commands. The command will fail if the entity already exists,
-- preventing duplicate creation.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Just _ ->
--       Decision.reject "Cart already exists!"
--     Nothing -> do
--       cartId <- Decision.generateUuid
--       Decision.acceptNew [CartCreated {cartCreatedId = cartId}]
-- @
acceptNew :: (Array a) -> Decision a
acceptNew events =
  Accept StreamCreation events


-- | Accept a command only if the stream already exists, emitting the given events.
--
-- Use this for entity update commands. The command will fail if the entity doesn't exist,
-- ensuring you can't update something that hasn't been created.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Nothing ->
--       Decision.reject "Cart does not exist!"
--     Just _ ->
--       Decision.acceptExisting [CartUpdated]
-- @
acceptExisting :: (Array a) -> Decision a
acceptExisting events =
  Accept ExistingStream events


-- | Accept a command only if the stream is at the specified position, emitting the given events.
--
-- Use this for optimistic concurrency control. The command will fail if the stream has
-- been modified since the position was read, preventing concurrent modification conflicts.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Nothing ->
--       Decision.reject "Cart does not exist!"
--     Just e -> do
--       let expectedPosition = e.version
--       Decision.acceptAfter expectedPosition [CartUpdated]
-- @
acceptAfter :: StreamPosition -> (Array a) -> Decision a
acceptAfter pos events =
  Accept (InsertAfter pos) events


-- | Reject a command with a reason message.
--
-- Use this when a command cannot be processed due to business rule violations,
-- invalid state, or other constraints.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Just _ ->
--       Decision.reject "Cart already exists!"
--     Nothing -> do
--       cartId <- Decision.generateUuid
--       Decision.acceptNew [CartCreated {cartCreatedId = cartId}]
-- @
reject :: Text -> Decision a
reject reason = Reject reason
