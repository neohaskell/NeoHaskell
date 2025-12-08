module Decision (
  Decision,
  generateUuid,
  acceptAny,
  acceptNew,
  acceptExisting,
  acceptAfter,
  reject,
) where

import Array (Array)
import Service.Command.Core (Decision (..))
import Service.Event (InsertionType (..), StreamPosition)
import Text (Text)
import Uuid (Uuid)


-- | Generate a UUID within a Decision context.
--
-- This allows you to generate unique identifiers when constructing events.
--
-- Example:
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