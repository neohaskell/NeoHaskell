{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Command.Core (
  Command (..),
  CommandResult (..),
  EventOf,
  EntityOf,
) where

import Array (Array)
import Basics
import Maybe (Maybe)
import Service.Event (InsertionType)
import Service.Event.StreamId (StreamId)
import Text (Text)
import Uuid (Uuid)


-- | Result of a command decision.
--   Either the command is accepted with events to append, or rejected with an error message.
data CommandResult event
  = AcceptCommand InsertionType (Array event)
  | RejectCommand Text
  deriving (Eq, Show, Ord, Generic)


type family EntityOf (command :: Type) :: Type


-- | Command typeclass for commands.
--   Commands represent the intent to change entity state.
--   The `decide` method contains the business logic that determines
--   whether the command should be accepted or rejected.
class Command command where
  -- | The entity type that this command operates on
  type IsMultiTenant command :: Bool


  type IsMultiTenant command = False


  streamId :: StreamIdFunction (IsMultiTenant command) command


  decide :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))


-- | Extract the stream ID from the command.
--   This determines which entity stream the command targets.
type family StreamIdFunction (isTenant :: Bool) command where
  StreamIdFunction 'False command = command -> StreamId
  StreamIdFunction 'True command = Uuid -> command -> StreamId


-- | Make a decision about the command given the current entity state.
--   Returns either:
--   - AcceptCommand with InsertionType and events to append
--   - RejectCommand with an error message
--
--   The entity parameter is Maybe Entity:
--   - Nothing means the entity doesn't exist yet (typical for creation commands)
--   - Just entity means the entity exists (typical for update commands)
type family DecideFunction (isTenant :: Bool) command entity event where
  DecideFunction 'False command entity event =
    command -> Maybe entity -> CommandResult event
  DecideFunction 'True command entity event =
    Uuid -> command -> Maybe entity -> CommandResult event


type family EventOf (entityType :: Type)