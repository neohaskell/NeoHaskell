module Main (main) where

import Prelude (IO)

import Service.ApplicationSpec qualified
import Service.CommandHandlerSpec qualified
import Service.CommandSpec qualified
import Service.EventStore.InMemorySpec qualified
import Service.EventStore.SimpleSpec qualified
import Service.EventStore.Postgres.SubscriptionStoreSpec qualified
import Service.EventStore.Postgres.NotificationsSpec qualified
import Service.EventStore.PostgresSpec qualified
import Service.FileUpload.BlobStore.LocalSpec qualified
import Service.FileUpload.CoreSpec qualified
import Service.FileUpload.DownloadSpec qualified
import Service.FileUpload.FileStateStore.InMemorySpec qualified
import Service.FileUpload.FileStateStore.PostgresSpec qualified
import Service.FileUpload.LifecycleSpec qualified
import Service.FileUpload.ResolverSpec qualified
import Service.FileUpload.RoutesSpec qualified
import Service.Integration.AdapterSpec qualified
import Service.Integration.DispatchRegistrySpec qualified
import Service.Integration.SelectionSpec qualified
import Service.Integration.WireupSpec qualified
import Service.Query.EndpointSpec qualified
import Service.Query.RegistrySpec qualified
import Service.Query.SubscriberSpec qualified
import Service.Query.THSpec qualified
import Service.Query.UpdaterSpec qualified
import Service.QueryObjectStore.InMemorySpec qualified
import Service.SnapshotCache.InMemorySpec qualified
import Service.Transport.CliSpec qualified
import Service.Transport.Cli.OutputSpec qualified
import Service.Transport.InternalSpec qualified
import Service.Transport.McpSpec qualified
import Service.Transport.Mcp.JsonRpcSpec qualified
import Service.Transport.Mcp.ProtocolSpec qualified
import Service.Transport.Mcp.ResponseSpec qualified
import Service.Transport.WebSpec qualified
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "Service.Application" Service.ApplicationSpec.spec
  Hspec.describe "Service.CommandHandler" Service.CommandHandlerSpec.spec
  Hspec.describe "Service.Command" Service.CommandSpec.spec
  Hspec.describe "Service.EventStore.InMemory" Service.EventStore.InMemorySpec.spec
  Hspec.describe "Service.EventStore.Simple" Service.EventStore.SimpleSpec.spec
  Hspec.describe "Service.EventStore.Postgres.SubscriptionStore" Service.EventStore.Postgres.SubscriptionStoreSpec.spec
  Hspec.describe "Service.EventStore.Postgres" Service.EventStore.PostgresSpec.spec
  Hspec.describe "Service.EventStore.Postgres.Notifications" Service.EventStore.Postgres.NotificationsSpec.spec
  Hspec.describe "Service.FileUpload.BlobStore.Local" Service.FileUpload.BlobStore.LocalSpec.spec
  Hspec.describe "Service.FileUpload.Core" Service.FileUpload.CoreSpec.spec
  Hspec.describe "Service.FileUpload.Download" Service.FileUpload.DownloadSpec.spec
  Hspec.describe "Service.FileUpload.FileStateStore.InMemory" Service.FileUpload.FileStateStore.InMemorySpec.spec
  Hspec.describe "Service.FileUpload.FileStateStore.Postgres" Service.FileUpload.FileStateStore.PostgresSpec.spec
  Hspec.describe "Service.FileUpload.Lifecycle" Service.FileUpload.LifecycleSpec.spec
  Hspec.describe "Service.FileUpload.Resolver" Service.FileUpload.ResolverSpec.spec
  Hspec.describe "Service.FileUpload.Routes" Service.FileUpload.RoutesSpec.spec
  Hspec.describe "Service.Integration.Adapter" Service.Integration.AdapterSpec.spec
  Hspec.describe "Service.Integration.DispatchRegistry" Service.Integration.DispatchRegistrySpec.spec
  Hspec.describe "Service.Integration.Selection" Service.Integration.SelectionSpec.spec
  Hspec.describe "Service.Integration.Wireup" Service.Integration.WireupSpec.spec
  Hspec.describe "Service.Query.Endpoint" Service.Query.EndpointSpec.spec
  Hspec.describe "Service.Query.Registry" Service.Query.RegistrySpec.spec
  Hspec.describe "Service.Query.Subscriber" Service.Query.SubscriberSpec.spec
  Hspec.describe "Service.Query.TH" Service.Query.THSpec.spec
  Hspec.describe "Service.Query.Updater" Service.Query.UpdaterSpec.spec
  Hspec.describe "Service.QueryObjectStore.InMemory" Service.QueryObjectStore.InMemorySpec.spec
  Hspec.describe "Service.SnapshotCache.InMemory" Service.SnapshotCache.InMemorySpec.spec
  Hspec.describe "Service.Transport.Cli" Service.Transport.CliSpec.spec
  Hspec.describe "Service.Transport.Cli.Output" Service.Transport.Cli.OutputSpec.spec
  Hspec.describe "Service.Transport.Internal" Service.Transport.InternalSpec.spec
  Hspec.describe "Service.Transport.Mcp" Service.Transport.McpSpec.spec
  Hspec.describe "Service.Transport.Mcp.JsonRpc" Service.Transport.Mcp.JsonRpcSpec.spec
  Hspec.describe "Service.Transport.Mcp.Protocol" Service.Transport.Mcp.ProtocolSpec.spec
  Hspec.describe "Service.Transport.Mcp.Response" Service.Transport.Mcp.ResponseSpec.spec
  Hspec.describe "Service.Transport.Web" Service.Transport.WebSpec.spec
