module Service.Error (
  -- * Service Errors
  ServiceError(..),
) where

import Basics
import Text (Text)

-- | Errors that can occur during service operations
data ServiceError
  = -- | Command with the given name was not found
    CommandNotFound Text
  | -- | Command decoding failed with the given error message
    CommandDecodingFailed Text
  | -- | Command execution failed with the given error message
    CommandExecutionFailed Text
  | -- | Server for the given API was not found
    ServerNotFound Text
  | -- | Service has already been shut down
    ServiceAlreadyShutdown
  deriving (Show, Eq, Generic)