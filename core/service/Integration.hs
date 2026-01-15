-- | # Integration
--
-- This module provides the integration pattern for connecting domain events
-- to external systems. Integrations come in two flavors:
--
-- * **Outbound**: React to domain events and trigger external effects
-- * **Inbound**: Listen to external sources and translate to domain commands
--
-- == Two Personas
--
-- The integration API is designed for two distinct users:
--
-- * **Nick (Integration Developer)**: Builds reusable integration packages
--   with HTTP calls, retries, authentication. Implements 'ToAction' instances.
--
-- * **Jess (Integration User)**: Configures integrations by instantiating
--   config records. Never sees 'Task', error handling, or networking code.
--
-- == Usage (Jess's perspective)
--
-- @
-- userIntegrations :: User -> UserEvent -> Integration.Outbound
-- userIntegrations user event = case event of
--   UserRegistered info -> Integration.batch
--     [ Integration.outbound Sendgrid.Email
--         { to = user.email
--         , templateId = "welcome"
--         }
--     ]
--   _ -> Integration.none
-- @
module Integration (
  -- * Outbound Types
  Outbound,
  Action,

  -- * Outbound Construction (Jess's API)
  batch,
  none,
  outbound,

  -- * Typeclass (Nick's API)
  ToAction (..),

  -- * Action Construction (Nick's API)
  action,
  emitCommand,
  noCommand,

  -- * Inbound Types
  Inbound (..),
  InboundConfig (..),
  inbound,

  -- * Errors
  IntegrationError (..),
  CommandPayload (..),

  -- * Testing Utilities
  -- | These functions expose behavior for testing and debugging.
  -- In production, the runtime handles action execution.
  runAction,
  getActions,

  -- * Runtime
  -- | These functions are used by the Application runtime to execute integrations.
  runInbound,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Proxy (Proxy (..))
import GHC.TypeLits qualified as GHC
import Json qualified
import Maybe (Maybe (..))
import Service.Command.Core (NameOf)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Error types for integration failures.
data IntegrationError
  = NetworkError Text
  | AuthenticationError Text
  | ValidationError Text
  | RateLimited Int -- ^ Retry after N seconds
  | PermanentFailure Text
  | UnexpectedError Text
  deriving (Eq, Show)


-- | Payload for emitting a command from an integration.
data CommandPayload = CommandPayload
  { commandType :: Text
  , commandData :: Json.Value
  }
  deriving (Eq, Show)


-- | Internal representation of an action.
data ActionInternal = ActionInternal
  { _execute :: Task IntegrationError (Maybe CommandPayload)
  }


-- | A single outbound action (type-erased).
-- Created by Nick's 'ToAction' instances, used by Jess via 'outbound'.
newtype Action = Action ActionInternal


-- | Collection of outbound actions for a single event.
-- Created by 'batch' or 'none'.
newtype Outbound = Outbound (Array Action)


-- | Typeclass for converting config records to actions.
-- Implemented by Nick in integration packages.
class ToAction config where
  toAction :: config -> Action


-- | Convert a config record to a type-erased action.
-- Used by Jess to wrap config records for 'batch'.
--
-- @
-- Integration.batch
--   [ Integration.outbound Sendgrid.Email { ... }
--   , Integration.outbound Slack.Message { ... }
--   ]
-- @
outbound :: forall config. (ToAction config) => config -> Action
outbound config = toAction config


-- | Collect multiple actions into an 'Outbound' value.
--
-- @
-- cartIntegrations cart event = case event of
--   ItemAdded {} -> Integration.batch
--     [ Integration.outbound Command.Emit { ... }
--     ]
-- @
batch :: Array Action -> Outbound
batch actions = Outbound actions


-- | No integrations for this event.
--
-- @
-- cartIntegrations cart event = case event of
--   CartCreated {} -> Integration.none
--   _ -> Integration.none
-- @
none :: Outbound
none = Outbound Array.empty


-- | Create an action from a Task. Used by Nick in 'ToAction' instances.
--
-- @
-- instance ToAction Email where
--   toAction config = Integration.action do
--     response <- Http.post "..." |> Http.send
--     Integration.emitCommand (config.onSuccess response)
-- @
action :: Task IntegrationError (Maybe CommandPayload) -> Action
action task = Action (ActionInternal task)


-- | Emit a command as the result of an integration action.
-- The command will be dispatched to the appropriate service.
--
-- The command type must have a NameOf instance (derived via command TH).
--
-- @
-- instance ToAction Email where
--   toAction config = Integration.action do
--     response <- sendEmail config
--     Integration.emitCommand (config.onSuccess response)
-- @
emitCommand ::
  forall command name.
  (Json.ToJSON command, name ~ NameOf command, GHC.KnownSymbol name) =>
  command ->
  Task IntegrationError (Maybe CommandPayload)
emitCommand cmd = do
  let cmdName = GHC.symbolVal (Proxy @name) |> Text.fromLinkedList
  let payload =
        CommandPayload
          { commandType = cmdName
          , commandData = Json.encode cmd
          }
  Task.yield (Just payload)


-- | No command to emit. Used when integration doesn't produce a follow-up command.
noCommand :: Task IntegrationError (Maybe CommandPayload)
noCommand = Task.yield Nothing


-- | Configuration for an inbound integration worker.
data InboundConfig command = InboundConfig
  { run :: (command -> Task IntegrationError Unit) -> Task IntegrationError Unit
  }


-- | An inbound integration worker (type-erased).
newtype Inbound = Inbound InboundInternal


data InboundInternal = InboundInternal
  { _runWorker :: (CommandPayload -> Task IntegrationError Unit) -> Task IntegrationError Unit
  }


-- | Create an inbound integration from a config.
-- Used by Nick in integration packages.
--
-- @
-- webhook config = Integration.inbound
--   { run = \emit -> do
--       Http.serve "/webhooks/stripe" \request -> do
--         payload <- Stripe.verifyAndParse request
--         emit (config.toCommand payload)
--   }
-- @
inbound ::
  forall command name.
  (Json.ToJSON command, name ~ NameOf command, GHC.KnownSymbol name) =>
  InboundConfig command ->
  Inbound
inbound config = do
  let cmdName = GHC.symbolVal (Proxy @name) |> Text.fromLinkedList
  let worker =
        InboundInternal
          { _runWorker = \emitPayload ->
              config.run \cmd -> do
                let payload =
                      CommandPayload
                        { commandType = cmdName
                        , commandData = Json.encode cmd
                        }
                emitPayload payload
          }
  Inbound worker


-- ============================================================================
-- Testing Utilities
-- ============================================================================

-- | Execute an Action and return its result.
--
-- This function exposes the internal Task for testing and debugging.
-- In production, the runtime executes actions and dispatches resulting commands.
--
-- @
-- action <- Integration.outbound myConfig
-- result <- Integration.runAction action
-- case result of
--   Just payload -> -- command was emitted
--   Nothing -> -- no command to emit
-- @
runAction :: Action -> Task IntegrationError (Maybe CommandPayload)
runAction action = case action of
  Action internal -> internal._execute


-- | Extract actions from an Outbound for inspection.
--
-- This function exposes the internal action list for testing and debugging.
-- In production, the runtime iterates over actions automatically.
--
-- @
-- let outbound = Integration.batch [action1, action2]
-- let actions = Integration.getActions outbound
-- Array.length actions  -- 2
-- @
getActions :: Outbound -> Array Action
getActions outbound = case outbound of
  Outbound actions -> actions


-- | Run an inbound integration worker.
--
-- The worker runs indefinitely, calling the emit callback whenever it has
-- a command to dispatch. This function exposes the internal runner for
-- the Application runtime.
--
-- @
-- Integration.runInbound periodicCartCreator dispatchCommand
-- @
runInbound ::
  Inbound ->
  (CommandPayload -> Task IntegrationError Unit) ->
  Task IntegrationError Unit
runInbound inboundIntegration emit = case inboundIntegration of
  Inbound internal -> internal._runWorker emit
