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
) where

import Array (Array)
import Basics
import Json qualified
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Text (Text)
import TypeName qualified


-- | Error types for integration failures.
data IntegrationError
  = NetworkError Text
  | AuthenticationError Text
  | ValidationError Text
  | RateLimited Int -- ^ Retry after N seconds
  | PermanentFailure Text
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
none = Outbound []


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
-- @
-- instance ToAction Email where
--   toAction config = Integration.action do
--     response <- sendEmail config
--     Integration.emitCommand (config.onSuccess response)
-- @
emitCommand ::
  forall command.
  (Json.ToJSON command, TypeName.Inspectable command) =>
  command ->
  Task IntegrationError (Maybe CommandPayload)
emitCommand cmd = do
  let payload =
        CommandPayload
          { commandType = TypeName.get cmd
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
  forall command.
  (Json.ToJSON command, TypeName.Inspectable command) =>
  InboundConfig command ->
  Inbound
inbound config = do
  let worker =
        InboundInternal
          { _runWorker = \emitPayload ->
              config.run \cmd -> do
                let payload =
                      CommandPayload
                        { commandType = TypeName.get cmd
                        , commandData = Json.encode cmd
                        }
                emitPayload payload
          }
  Inbound worker
