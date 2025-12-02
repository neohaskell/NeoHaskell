module Service.Error (
  -- * Service Errors
  ServiceError(..),
) where

import Basics
import Text (Text)

-- | Errors that can occur during service operations
data ServiceError
  = CommandNotFound Text
  | AdapterInitializationFailed Text Text -- adapter name, error message
  | CommandDecodingFailed Text -- error message
  | CommandExecutionFailed Text -- error message
  | AdapterNotFound Text -- protocol name
  | ServiceAlreadyShutdown
  deriving (Show, Eq, Generic)