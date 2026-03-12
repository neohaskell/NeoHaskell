-- | Shared test fixtures for Integration.Agent tests.
module Integration.Agent.TestFixtures
  ( -- * Command Types
    AddItemCommand (..)
  , ClearCartCommand (..)
  , AgentCommand (..)

    -- * Fixtures
  , validArgsJson
  , malformedArgsJson
  , longName

    -- * Tool Fixtures
  , addItemTool
  , clearCartTool
  ) where

import Basics
import Documented (Documented)
import Documented qualified
import Integration.Agent qualified as Agent
import Integration.Agent.Types (CommandTool)
import Json qualified

import Result (Result (..))

import Schema (ToSchema)

import Service.Command.Core (NameOf)
import Text (Text)
import Text qualified


-- | Test command with Documented description.
data AddItemCommand = AddItemCommand
  { cartId :: Text
  , stockId :: Text
  , quantity :: Int
  }
  deriving (Show, Eq, Generic)


instance Json.FromJSON AddItemCommand where
  parseJSON = Json.withObject "AddItemCommand" \obj -> do
    cartId <- obj Json..: "cartId"
    stockId <- obj Json..: "stockId"
    quantity <- obj Json..: "quantity"
    Json.yield AddItemCommand {cartId, stockId, quantity}


instance Json.ToJSON AddItemCommand where
  toJSON cmd =
    Json.object
      [ ("cartId", Json.toJSON cmd.cartId)
      , ("stockId", Json.toJSON cmd.stockId)
      , ("quantity", Json.toJSON cmd.quantity)
      ]


instance ToSchema AddItemCommand


instance Documented AddItemCommand where
  description = "Add an item to the shopping cart"


type instance NameOf AddItemCommand = "AddItem"


-- | Test command with empty description (fallback test).
data ClearCartCommand = ClearCartCommand
  { cartId :: Text
  }
  deriving (Show, Eq, Generic)


instance Json.FromJSON ClearCartCommand where
  parseJSON = Json.withObject "ClearCartCommand" \obj -> do
    cartId <- obj Json..: "cartId"
    Json.yield ClearCartCommand {cartId}


instance Json.ToJSON ClearCartCommand where
  toJSON cmd =
    Json.object
      [ ("cartId", Json.toJSON cmd.cartId)
      ]


instance ToSchema ClearCartCommand


instance Documented ClearCartCommand where
  description = ""


type instance NameOf ClearCartCommand = "ClearCart"


-- | Sum type for pipeline tests.
data AgentCommand
  = CommandOk AddItemCommand
  | CommandError Text
  deriving (Show, Eq, Generic)


instance Json.FromJSON AgentCommand where
  parseJSON v =
    case Json.decode v of
      Ok cmd -> Json.yield (CommandOk cmd)
      Err _ -> Json.fail "Could not decode AgentCommand"


instance Json.ToJSON AgentCommand where
  toJSON cmd =
    case cmd of
      CommandOk inner -> Json.toJSON inner
      CommandError err -> Json.toJSON err


type instance NameOf AgentCommand = "AgentCommand"


-- | Canonical valid args JSON text.
validArgsJson :: Text
validArgsJson = "{\"cartId\":\"cart-1\",\"stockId\":\"stock-1\",\"quantity\":2}"


-- | Canonical malformed args JSON text.
malformedArgsJson :: Text
malformedArgsJson = "{\"cartId\":\"cart-1\",\"quantity\":"


-- | Long tool name fixture (150 X characters).
longName :: Text
longName = Text.repeat 150 "X"


-- | Pre-built AddItem tool fixture.
addItemTool :: CommandTool
addItemTool = Agent.commandTool @AddItemCommand


-- | Pre-built ClearCart tool fixture.
clearCartTool :: CommandTool
clearCartTool = Agent.commandTool @ClearCartCommand
