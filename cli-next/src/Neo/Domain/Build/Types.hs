module Neo.Domain.Build.Types where

import Core


-- ================================================================================
-- DOMAIN TYPES
-- ================================================================================

-- Basic identifiers and enums
type BuildId = Text


data BuildStatus
  = Initializing
  | LoadingConfig
  | PreparingNixExpression
  | RunningNixBuild
  | Completed
  | Failed Text
  deriving (Show, Eq)


data LogLevel = Info | Warning | Error
  deriving (Show, Eq)


data LogEntry = LogEntry
  { timestamp :: Int,
    level :: LogLevel,
    message :: Text
  }
  deriving (Show)


data OutputType = StdOut | StdErr
  deriving (Show, Eq)


data BuildErrorType
  = ConfigurationError
  | NixBuildError
  | SystemError
  deriving (Show, Eq)


data ErrorDetails = ErrorDetails
  { errorType :: BuildErrorType,
    message :: Text,
    details :: Maybe Text
  }
  deriving (Show)


-- Configuration types
data ProjectConfiguration = ProjectConfiguration
  { projectName :: Text,
    overrideNeohaskell :: Maybe Text,
    dependencies :: Array Text
  }
  deriving (Show)


data NixBuildConfig = NixBuildConfig
  { neoJsonPath :: Path,
    srcPath :: Path,
    neohaskellSource :: Text
  }
  deriving (Show)
