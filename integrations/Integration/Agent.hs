{-# LANGUAGE AllowAmbiguousTypes #-}
-- | # Integration.Agent — Provider-Agnostic AI Agent via Tool Calling
--
-- == Two-Persona Model
--
-- * __Jess (Integration User)__: Configures agents with pure records.
--   No JSON, no HTTP, no provider details visible.
--
-- * __Nick (Integration Developer)__: Implements 'ToAction' in
--   "Integration.Agent.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Agent qualified as Agent
--
-- cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
-- cartIntegrations cart event = case event of
--   CustomerAsked question -> Integration.batch
--     [ Agent.agent
--         question
--         [ Agent.commandTool \@AddItem
--         , Agent.commandTool \@RemoveItem
--         ]
--         "anthropic/claude-3.5-sonnet"
--         (\\err -> AgentFailed { cartId = cart.id, error = err })
--         |> Integration.outbound
--     ]
--   _ -> Integration.none
-- @
--
-- == Data Privacy Notice
--
-- The user prompt is sent to third-party AI providers via OpenRouter.
-- Do not include PII, credentials, or confidential business data in prompts
-- unless appropriate data processing agreements are in place.
-- See ADR-0045 for security and privacy considerations.
module Integration.Agent
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)
  , CommandTool (..)

    -- * Config Helpers
  , defaultConfig

    -- * Smart Constructors
  , commandTool
  , agent
  ) where

import Array (Array)
import Basics
import Data.Proxy (Proxy (..))
import Documented (Documented)
import Documented qualified
import GHC.TypeLits qualified as GhcSymbol
import Integration.Agent.Types (CommandTool (..), Config (..), Request (..), defaultConfig)
import Integration.Agent.Types qualified as Types
import Json qualified
import Schema (ToSchema)
import Schema qualified
import Schema.JsonSchema qualified
import Service.Command.Core (NameOf)
import Text (Text)
import Text qualified


-- | Extract a tool definition from a command type.
--
-- Uses 'NameOf' for the wire name, 'Documented' for the description,
-- and 'ToSchema' for the JSON Schema. The full tool definition JSON is
-- pre-computed once and stored in 'CommandTool.toolDefinition'.
--
-- For best performance, bind at module level (creates a CAF):
--
-- @
-- addItemTool :: CommandTool
-- addItemTool = Agent.commandTool \@AddItem
-- @
{-# INLINE commandTool #-}
commandTool ::
  forall command name.
  ( ToSchema command
  , Documented command
  , NameOf command ~ name
  , GhcSymbol.KnownSymbol name
  ) =>
  CommandTool
commandTool = do
  let name = GhcSymbol.symbolVal (Proxy @name) |> Text.fromLinkedList
  let desc = Documented.description @command
  let schema = Schema.toSchema @command |> Schema.JsonSchema.toJsonSchema
  let description = case desc of
        "" -> name
        d -> d
  let definition =
        Json.object
          [ ("type", Json.toJSON ("function" :: Text))
          , ( "function"
            , Json.object
                [ ("name", Json.toJSON name)
                , ("description", Json.toJSON description)
                , ("parameters", schema)
                ]
            )
          ]
  CommandTool
    { toolName = name
    , toolDescription = description
    , toolDefinition = definition
    }


-- | Smart constructor for an Agent.Request with default config.
--
-- @
-- Agent.agent
--   customerMessage
--   [ Agent.commandTool \@AddItem
--   , Agent.commandTool \@RemoveItem
--   ]
--   "anthropic/claude-3.5-sonnet"
--   (\\err -> AgentFailed { error = err })
-- @
{-# INLINE agent #-}
agent ::
  forall command.
  Text ->
  Array CommandTool ->
  Text ->
  (Text -> command) ->
  Request command
agent prompt tools model onError =
  Request
    { prompt
    , tools
    , model
    , config = Types.defaultConfig
    , onError
    }
