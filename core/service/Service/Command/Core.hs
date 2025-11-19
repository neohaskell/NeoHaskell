{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Command.Core (
  Command (..),
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


-- | Command typeclass for commands.
--   Commands represent the intent to change entity state.
--   The `decide` method contains the business logic that determines
--   whether the command should be accepted or rejected.
class Command command where
  -- | The entity type that this command operates on
  type Entity command


  type Multitenant command :: Bool
  type Multitenant command = False


  streamId :: StreamIdSignature (Multitenant command) command


  decide :: DecideSignature (Multitenant command) command (Entity command) event


-- | Extract the stream ID from the command.
--   This determines which entity stream the command targets.
type family StreamIdSignature (isTenant :: Bool) command where
  StreamIdSignature 'False command = command -> StreamId
  StreamIdSignature 'True command = Uuid -> command -> StreamId


-- | Make a decision about the command given the current entity state.
--   Returns either:
--   - AcceptCommand with InsertionType and events to append
--   - RejectCommand with an error message
--
--   The entity parameter is Maybe Entity:
--   - Nothing means the entity doesn't exist yet (typical for creation commands)
--   - Just entity means the entity exists (typical for update commands)
type family DecideSignature (isTenant :: Bool) command entity event where
  DecideSignature 'False command entity event =
    command -> Maybe entity -> CommandResult event
  DecideSignature 'True command entity event =
    Uuid -> command -> Maybe entity -> CommandResult event