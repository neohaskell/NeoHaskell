{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Service.Runtime (
  -- * Service Runtime
  ServiceRuntime(..),
) where

import Basics
import Bytes (Bytes)
import Service.Error (ServiceError)
import Task (Task)
import Text (Text)

-- | ServiceRuntime is the deployed, runnable form of a service
-- It provides command lookup, execution and shutdown capabilities
data ServiceRuntime = ServiceRuntime
  { -- | Find a command by name and execute it with a Bytes payload
    -- The runtime looks up the command in the service definition,
    -- instantiates the appropriate handler, and delegates to the adapter
    execute :: Text -> Bytes -> Task ServiceError Bytes,
    -- | Gracefully shutdown all adapters
    shutdown :: Task ServiceError Unit
  }