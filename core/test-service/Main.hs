module Main (main) where

import Prelude (IO)

import Service.ApplicationSpec qualified
import Service.CommandHandlerSpec qualified
import Service.CommandSpec qualified
import Service.EventStore.InMemorySpec qualified
import Service.EventStore.SimpleSpec qualified
import Service.EventStore.Postgres.SubscriptionStoreSpec qualified
import Service.EventStore.PostgresSpec qualified
import Service.FileUpload.BlobStore.LocalSpec qualified
import Service.FileUpload.CoreSpec qualified
import Service.FileUpload.DownloadSpec qualified
import Service.FileUpload.FileStateStore.InMemorySpec qualified
import Service.FileUpload.FileStateStore.PostgresSpec qualified
import Service.FileUpload.LifecycleSpec qualified
import Service.FileUpload.ResolverSpec qualified
import Service.FileUpload.RoutesSpec qualified
import Service.Query.EndpointSpec qualified
import Service.Query.RegistrySpec qualified
import Service.Query.SubscriberSpec qualified
import Service.Query.THSpec qualified
import Service.Query.UpdaterSpec qualified
import Service.QueryObjectStore.InMemorySpec qualified
import Service.SnapshotCache.InMemorySpec qualified
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
  Hspec.describe "Service.FileUpload.BlobStore.Local" Service.FileUpload.BlobStore.LocalSpec.spec
  Hspec.describe "Service.FileUpload.Core" Service.FileUpload.CoreSpec.spec
  Hspec.describe "Service.FileUpload.Download" Service.FileUpload.DownloadSpec.spec
  Hspec.describe "Service.FileUpload.FileStateStore.InMemory" Service.FileUpload.FileStateStore.InMemorySpec.spec
  Hspec.describe "Service.FileUpload.FileStateStore.Postgres" Service.FileUpload.FileStateStore.PostgresSpec.spec
  Hspec.describe "Service.FileUpload.Lifecycle" Service.FileUpload.LifecycleSpec.spec
  Hspec.describe "Service.FileUpload.Resolver" Service.FileUpload.ResolverSpec.spec
  Hspec.describe "Service.FileUpload.Routes" Service.FileUpload.RoutesSpec.spec
  Hspec.describe "Service.Query.Endpoint" Service.Query.EndpointSpec.spec
  Hspec.describe "Service.Query.Registry" Service.Query.RegistrySpec.spec
  Hspec.describe "Service.Query.Subscriber" Service.Query.SubscriberSpec.spec
  Hspec.describe "Service.Query.TH" Service.Query.THSpec.spec
  Hspec.describe "Service.Query.Updater" Service.Query.UpdaterSpec.spec
  Hspec.describe "Service.QueryObjectStore.InMemory" Service.QueryObjectStore.InMemorySpec.spec
  Hspec.describe "Service.SnapshotCache.InMemory" Service.SnapshotCache.InMemorySpec.spec
  Hspec.describe "Service.Transport.Web" Service.Transport.WebSpec.spec
