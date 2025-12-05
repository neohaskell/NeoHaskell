{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Service.Adapter.Direct (
  -- * Direct Adapter
  DirectAdapter(..),
  DirectAdapterState(..),
  DirectConfig(..),
  defaultConfig,
) where

import Basics
import Bytes qualified
import Service.Adapter (ServiceAdapter(..))
import Service.Error (ServiceError(..))
import Service.Protocol (TransportProtocol(..))
import Task qualified

-- | Configuration for the Direct adapter
data DirectConfig = DirectConfig
  { -- | Number of retries on failure
    retryCount :: Int
  }
  deriving (Eq, Show, Generic)

-- | Default configuration for DirectAdapter
defaultConfig :: DirectConfig
defaultConfig = DirectConfig
  { retryCount = 0
  }

-- | Direct adapter that executes commands in-process
-- Despite being "direct", it still operates on the Bytes interface
data DirectAdapter = DirectAdapter
  { config :: DirectConfig
  }
  deriving (Eq, Show, Generic)

-- | Runtime state for the Direct adapter
data DirectAdapterState = DirectAdapterState
  { isShutdown :: Bool
  }

-- | Direct is a transport protocol for in-process execution
instance TransportProtocol "Direct" where
  type ProtocolConfig "Direct" = DirectConfig
  type ProtocolState "Direct" = DirectAdapterState

-- | ServiceAdapter instance for DirectAdapter
instance ServiceAdapter DirectAdapter where
  type AdapterProtocol DirectAdapter = "Direct"
  type AdapterState DirectAdapter = DirectAdapterState

  -- | Initialize the Direct adapter
  initializeAdapter _adapter = do
    -- TODO: Get or create CommandHandler instance
    -- For now, create a placeholder
    Task.yield DirectAdapterState
      { isShutdown = False
      }

  -- | Execute a command through the Direct adapter
  -- Note: The command has already been looked up and deserialized by ServiceRuntime
  -- The adapter's role is to:
  -- 1. Run the command through CommandHandler (which executes the Decision)
  -- 2. Handle the CommandHandlerResult
  -- 3. Serialize the result back to Bytes
  executeCommand _adapter state _commandName _bytes = do
    -- Check if adapter is shutdown
    if state.isShutdown
      then Task.throw ServiceAlreadyShutdown
      else do
        -- TODO: Actual implementation will:
        -- 1. Receive the already-deserialized command from ServiceRuntime
        -- 2. Get or create CommandHandler with EventStore and EntityFetcher
        -- 3. Execute command through CommandHandler.execute
        -- 4. Transform CommandHandlerResult to appropriate response
        -- 5. Serialize response to Bytes
        -- For now, return empty bytes as placeholder
        Task.yield (Bytes.fromLegacy "")

  -- | Shutdown the Direct adapter
  shutdownAdapter _adapter _state = do
    -- Mark as shutdown
    Task.yield unit