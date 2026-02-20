module Service.Transport.Cli.Output (
  OutputFormat (..),
  formatResponse,
) where

import Basics
import Json qualified
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Text (Text)


data OutputFormat
  = JsonOutput
  | PrettyOutput
  | QuietOutput
  deriving (Show, Eq)


-- | Format a CommandResponse for CLI output
formatResponse :: OutputFormat -> CommandResponse -> Text
formatResponse format response =
  case format of
    JsonOutput -> Json.encodeText response
    PrettyOutput -> Json.encodeText response
    QuietOutput ->
      case response of
        Response.Accepted {entityId} -> entityId
        Response.Rejected {reason} -> reason
        Response.Failed {error} -> error
