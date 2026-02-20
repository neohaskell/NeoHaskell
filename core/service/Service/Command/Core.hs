{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Command.Core (
  -- * Command Abstraction
  Command (..),
  CommandResult (..),

  -- * Type Families
  NameOf,
  TransportsOf,
  NamesOf,
  AppendSymbols,
  AllKnownSymbols (..),

  -- * Multi-Tenancy Support
  GetEntityIdFunction,
  DecideFunction,

  -- * Request Context
  RequestContext (..),
  UserClaims (..),

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

import Array (Array)
import Array qualified
import Basics
import Decider (CommandResult (..), Decision (..), DecisionContext (..), runDecision)
import GHC.TypeLits qualified as GHC
import Maybe (Maybe)
import Record qualified
import Service.Auth (RequestContext (..), UserClaims (..))
import Service.Entity.Core (Entity (..), EntityOf, Event (..), EventOf)
import Text (Text)
import Text qualified
import Uuid (Uuid)


-- | Maps a type to its symbolic name.
--
-- Example:
--
-- @
-- type instance NameOf CreateCart = "CreateCart"
-- @
type family NameOf (t :: Type) :: Symbol


-- | Maps a command type to its transport adapters (type-level list).
--
-- Example:
--
-- @
-- type instance TransportsOf CreateCart = '[WebTransport]
-- @
type family TransportsOf (commandType :: Type) :: [Type]


-- | Extracts the symbolic names from a type-level list of types.
--
-- Example:
--
-- @
-- NamesOf '[WebTransport, CliTransport] ~ '["WebTransport", "CliTransport"]
-- @
type family NamesOf (types :: [Type]) :: [Symbol] where
  NamesOf '[] = '[]
  NamesOf (t ': ts) = NameOf t ': NamesOf ts


-- | Appends two type-level symbol lists.
type family AppendSymbols (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  AppendSymbols '[] ys = ys
  AppendSymbols (x ': xs) ys = x ': AppendSymbols xs ys


-- | Reifies a type-level list of symbols into an Array of Text values.
class AllKnownSymbols (names :: [Symbol]) where
  allSymbolTexts :: Array Text


instance AllKnownSymbols '[] where
  allSymbolTexts = Array.empty


instance (Record.KnownSymbol name, AllKnownSymbols rest) => AllKnownSymbols (name ': rest) where
  allSymbolTexts = do
    let this = GHC.symbolVal (Record.Proxy @name) |> Text.fromLinkedList
    Array.prepend (Array.fromLinkedList [this]) (allSymbolTexts @rest)


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
-- ALL commands receive RequestContext for uniform authorization support.
--
-- * Single-tenant: @command -> Maybe entity -> RequestContext -> Decision event@
-- * Multi-tenant: @Uuid -> command -> Maybe entity -> RequestContext -> Decision event@
type family DecideFunction (isTenant :: Bool) command entity event where
  DecideFunction 'False command entity event =
    command -> Maybe entity -> RequestContext -> Decision event
  DecideFunction 'True command entity event =
    Uuid -> command -> Maybe entity -> RequestContext -> Decision event
