{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Command.Core (
  -- * Command Abstraction
  Command (..),
  CommandResult (..),

  -- * Type Families
  NameOf,
  TransportOf,

  -- * Multi-Tenancy Support
  GetEntityIdFunction,
  DecideFunction,

  -- * Re-exported from Service.Entity (backward compatibility)
  Entity (..),
  Event (..),
  EntityOf,
  EventOf,

  -- * Re-exported from Decider (backward compatibility)
  Decision (..),
  DecisionContext (..),
  runDecision,
) where

import Basics
import Decider (CommandResult (..), Decision (..), DecisionContext (..), runDecision)
import Maybe (Maybe)
import Service.Entity.Core (Entity (..), EntityOf, Event (..), EventOf)
import Uuid (Uuid)


-- | Maps a type to its symbolic name.
--
-- Example:
--
-- @
-- type instance NameOf CreateCart = "CreateCart"
-- @
type family NameOf (t :: Type) :: Symbol


-- | Maps a command type to its transport adapter.
--
-- Example:
--
-- @
-- type instance TransportOf CreateCart = WebTransport
-- @
type family TransportOf (commandType :: Type) :: Type


-- | The Command typeclass defines the contract for all commands.
--
-- Commands represent user intent to change the system. Each command must provide:
--
-- * 'getEntityIdImpl': Extract the entity ID from the command (Nothing for creation)
-- * 'decideImpl': Pure decision logic that produces events or rejects
class Command command where
  -- | Whether this command supports multi-tenancy.
  --
  -- When True, 'getEntityIdImpl' and 'decideImpl' receive a tenant UUID as first argument.
  type IsMultiTenant command :: Bool
  type IsMultiTenant command = False

  -- | Extract the entity ID from a command.
  --
  -- Returns Nothing for creation commands where no entity exists yet.
  getEntityIdImpl :: GetEntityIdFunction (IsMultiTenant command) command (EntityIdType (EntityOf command))

  -- | The decision function that determines whether to accept or reject the command.
  --
  -- This is pure business logic that takes the command and optional current entity state,
  -- returning a Decision that either accepts with events or rejects with a reason.
  decideImpl :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))


-- | Determines the signature of 'getEntityIdImpl' based on multi-tenancy.
--
-- * Single-tenant: @command -> Maybe id@
-- * Multi-tenant: @Uuid -> command -> Maybe id@
type family GetEntityIdFunction (isTenant :: Bool) command id where
  GetEntityIdFunction False command id =
    command -> Maybe id
  GetEntityIdFunction True command id =
    Uuid -> command -> Maybe id


-- | Determines the signature of 'decideImpl' based on multi-tenancy.
--
-- * Single-tenant: @command -> Maybe entity -> Decision event@
-- * Multi-tenant: @Uuid -> command -> Maybe entity -> Decision event@
type family DecideFunction (isTenant :: Bool) command entity event where
  DecideFunction 'False command entity event =
    command -> Maybe entity -> Decision event
  DecideFunction 'True command entity event =
    Uuid -> command -> Maybe entity -> Decision event
