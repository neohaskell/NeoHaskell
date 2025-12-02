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
-- It provides command execution and shutdown capabilities
data ServiceRuntime = ServiceRuntime
  { -- | Execute a command by name with a Bytes payload
    execute :: Text -> Bytes -> Task ServiceError Bytes,
    -- | Gracefully shutdown all adapters
    shutdown :: Task ServiceError Unit
  }