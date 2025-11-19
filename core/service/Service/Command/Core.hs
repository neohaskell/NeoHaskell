module Service.Command.Core (
  Command (..),
  TenantCommand (..),
  CommandResult (..),
) where

import Core
import Service.Event (InsertionType)
import Service.Event.StreamId (StreamId)


-- | Result of a command decision.
--   Either the command is accepted with events to append, or rejected with an error message.
data CommandResult event
  = AcceptCommand InsertionType (Array event)
  | RejectCommand Text
  deriving (Eq, Show, Ord, Generic)


-- | Command typeclass for regular (non-tenant) commands.
--   Commands represent the intent to change entity state.
--   The `decide` method contains the business logic that determines
--   whether the command should be accepted or rejected.
class Command command where
  -- | The entity type that this command operates on
  type Entity


  -- | Extract the stream ID from the command.
  --   This determines which entity stream the command targets.
  streamId :: command -> StreamId


  -- | Make a decision about the command given the current entity state.
  --   Returns either:
  --   - AcceptCommand with InsertionType and events to append
  --   - RejectCommand with an error message
  --
  --   The entity parameter is Maybe Entity:
  --   - Nothing means the entity doesn't exist yet (typical for creation commands)
  --   - Just entity means the entity exists (typical for update commands)
  decide :: command -> Maybe Entity -> CommandResult event


-- | TenantCommand typeclass for multi-tenant commands.
--   Similar to Command but includes a tenant ID parameter.
--   This allows commands to be scoped to specific tenants.
class TenantCommand command where
  -- | The entity type that this command operates on
  type Entity


  -- | Extract the stream ID from the command, potentially incorporating the tenant ID.
  --   This allows tenant-scoped entity identification.
  streamId :: command -> Uuid -> StreamId


  -- | Make a decision about the command given the current entity state and tenant ID.
  --   The tenant ID can be used in business logic and event generation.
  decide :: command -> Maybe Entity -> Uuid -> CommandResult event
