{-| Handle neoctl's help, version, and invalid startup arguments.
The transport delegates command handlers to NeoHaskell's existing CLI transport.
-}
module Neoctl.Transport (transport) where

import Core
import Console qualified
import Neoctl.Invocation (Invocation (..))
import Paths_neoctl qualified
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Transport (Endpoints (..), Transport (..))
import Service.Transport qualified as Transport
import Service.Transport.Cli (CliTransport (..), cli)
import Task qualified
import Version qualified


newtype NeoctlTransport = NeoctlTransport
  { invocation :: Invocation
  }


type instance NameOf NeoctlTransport = "NeoctlTransport"


deriveKnownHash "NeoctlTransport"


transport :: Invocation -> NeoctlTransport
transport invocation = NeoctlTransport
  { invocation = invocation
  }


cliConfiguration :: CliTransport
cliConfiguration = cli
  { programName = "neoctl"
  , version = Version.toText Paths_neoctl.version
  , description = "Display usage and packaged version information for neoctl."
  }


instance Transport NeoctlTransport where
  type Request NeoctlTransport = Bytes
  type Response NeoctlTransport = Bytes
  type RunnableTransport NeoctlTransport = Task Text Unit

  assembleTransport endpoints = do
    let selectedInvocation = endpoints.transport.invocation
    let configuration = cliConfiguration
    let configuredName = configuration.programName
    let configuredVersion = configuration.version
    let description = configuration.description
    case selectedInvocation of
      ShowHelp -> Console.print [fmt|Usage: #{configuredName} [--help|-h|--version]
#{description}|]
      ShowVersion -> Console.print [fmt|#{configuredName} #{configuredVersion}|]
      InvalidArguments -> Task.throw [fmt|Invalid arguments. Run #{configuredName} --help for usage.|]

  runTransport _transport runnable = runnable

  buildHandler _endpointTransport commandProxy handler =
    Transport.buildHandler cliConfiguration commandProxy handler
