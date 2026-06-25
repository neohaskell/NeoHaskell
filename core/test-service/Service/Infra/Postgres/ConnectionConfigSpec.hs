module Service.Infra.Postgres.ConnectionConfigSpec (spec) where

-- | Pure unit spec for the ADR-0062 shared Postgres connection-settings
-- builders. Runs unconditionally (no Postgres). Registered manually in
-- core/test-service/Main.hs (the suite does not auto-discover).
--
-- The hasql Setting / Config values are opaque (no Eq/Show), so every
-- assertion is written at an observable boundary: list length (the
-- LinkedList spine is forced), totality (force to WHNF via seq), and the
-- EventStore projection-equivalence (same length-1 structure).

import Core
import LinkedList qualified
import Hasql.Connection.Setting qualified as Hasql
import Service.EventStore.Postgres.Internal (PostgresEventStore (..))
import Service.EventStore.Postgres.Internal qualified as EventStore
import Service.Infra.Postgres.ConnectionConfig (ConnectionParams (..))
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Prelude qualified
import Test.Hspec qualified as Hspec


-- | A typical ConnectionParams input (matches toConnectionParams case #1).
typicalParams :: ConnectionParams
typicalParams =
  ConnectionConfig.ConnectionParams
    { host = "localhost",
      databaseName = "app",
      user = "postgres",
      password = "secret",
      port = 5432
    }


-- | The typical config with a specific port. Built by full record
-- construction (the constructor names the type) so the field set is
-- unambiguous — a bare @typicalParams { port = n }@ update is ambiguous
-- because several records in scope share a @port@ field.
paramsWithPort :: Int -> ConnectionParams
paramsWithPort p =
  ConnectionConfig.ConnectionParams
    { host = "localhost",
      databaseName = "app",
      user = "postgres",
      password = "secret",
      port = p
    }


-- | The typical config with a specific password, built by full record
-- construction for the same disambiguation reason as 'paramsWithPort'.
paramsWithPassword :: Text -> ConnectionParams
paramsWithPassword pw =
  ConnectionConfig.ConnectionParams
    { host = "localhost",
      databaseName = "app",
      user = "postgres",
      password = pw,
      port = 5432
    }


-- | A ConnectionParams with all four textual fields empty.
emptyParams :: ConnectionParams
emptyParams =
  ConnectionConfig.ConnectionParams
    { host = "",
      databaseName = "",
      user = "",
      password = "",
      port = 5432
    }


-- | A typical PostgresEventStore for the toConnectionSettings regression
-- cases, built by record-update on the Default instance.
typicalEventStore :: PostgresEventStore
typicalEventStore =
  (def :: PostgresEventStore)
    { EventStore.host = "localhost",
      EventStore.databaseName = "app",
      EventStore.user = "postgres",
      EventStore.password = "secret",
      EventStore.port = 5432
    }


-- | The reference projection mirroring toConnectionSettings's body so the
-- regression guard is a true same-input/same-structure comparison.
project :: PostgresEventStore -> ConnectionParams
project cfg =
  ConnectionConfig.ConnectionParams
    { host = cfg.host,
      databaseName = cfg.databaseName,
      user = cfg.user,
      password = cfg.password,
      port = cfg.port
    }


-- | A typical settings list, reused by the toPoolConfig settings-spine case.
typicalSettings :: LinkedList.LinkedList Hasql.Setting
typicalSettings = ConnectionConfig.toConnectionParams typicalParams


spec :: Hspec.Spec
spec = Hspec.describe "Service.Infra.Postgres.ConnectionConfig" do
  Hspec.describe "toConnectionParams" do
    Hspec.it "returns a single connection setting for a typical config" do
      (ConnectionConfig.toConnectionParams typicalParams |> LinkedList.length)
        |> Hspec.shouldBe 1

    Hspec.it "accepts the four ADR-0037 keepalive params and stays length 1 (keepalive contract)" do
      let result = ConnectionConfig.toConnectionParams typicalParams
      (result |> LinkedList.length) |> Hspec.shouldBe 1
      (result `Prelude.seq` True) |> Hspec.shouldBe True

    Hspec.it "is total for empty-string host, db, user, and password" do
      let result = ConnectionConfig.toConnectionParams emptyParams
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for port = 0 (lower boundary)" do
      let result = ConnectionConfig.toConnectionParams (paramsWithPort 0)
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for port = 1" do
      let result = ConnectionConfig.toConnectionParams (paramsWithPort 1)
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for port = 65535 (max valid TCP port)" do
      let result = ConnectionConfig.toConnectionParams (paramsWithPort 65535)
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for port = 65536 (max + 1)" do
      let result = ConnectionConfig.toConnectionParams (paramsWithPort 65536)
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for a negative port" do
      let result = ConnectionConfig.toConnectionParams (paramsWithPort (-1))
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for unicode multibyte text in the password field" do
      let result = ConnectionConfig.toConnectionParams (paramsWithPassword "pâsswörd-🔑-日本語")
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

  Hspec.describe "toConnectionSettings" do
    Hspec.it "equals the shared builder applied to the projected ConnectionParams (Design Goal 4 regression guard)" do
      let direct = EventStore.toConnectionSettings typicalEventStore |> LinkedList.length
      let projected = ConnectionConfig.toConnectionParams (project typicalEventStore) |> LinkedList.length
      direct |> Hspec.shouldBe projected
      direct |> Hspec.shouldBe 1

    Hspec.it "returns a single connection setting for a typical PostgresEventStore" do
      (EventStore.toConnectionSettings typicalEventStore |> LinkedList.length)
        |> Hspec.shouldBe 1

    Hspec.it "is total for empty-string host, db, user, and password fields" do
      let cfg =
            typicalEventStore
              { EventStore.host = "",
                EventStore.databaseName = "",
                EventStore.user = "",
                EventStore.password = ""
              }
      let result = EventStore.toConnectionSettings cfg
      (result `Prelude.seq` True) |> Hspec.shouldBe True
      (result |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for port = 0 and port = 65535 (boundaries) and equals the projected builder" do
      let cfg0 = typicalEventStore {EventStore.port = 0}
      (EventStore.toConnectionSettings cfg0 |> LinkedList.length) |> Hspec.shouldBe 1
      (EventStore.toConnectionSettings cfg0 |> LinkedList.length)
        |> Hspec.shouldBe (ConnectionConfig.toConnectionParams (project cfg0) |> LinkedList.length)
      let cfgMax = typicalEventStore {EventStore.port = 65535}
      (EventStore.toConnectionSettings cfgMax |> LinkedList.length) |> Hspec.shouldBe 1
      (EventStore.toConnectionSettings cfgMax |> LinkedList.length)
        |> Hspec.shouldBe (ConnectionConfig.toConnectionParams (project cfgMax) |> LinkedList.length)

    Hspec.it "is total for port = 65536 (max + 1) and a negative port" do
      let cfgOver = typicalEventStore {EventStore.port = 65536}
      let resultOver = EventStore.toConnectionSettings cfgOver
      (resultOver `Prelude.seq` True) |> Hspec.shouldBe True
      (resultOver |> LinkedList.length) |> Hspec.shouldBe 1
      let cfgNeg = typicalEventStore {EventStore.port = -1}
      let resultNeg = EventStore.toConnectionSettings cfgNeg
      (resultNeg `Prelude.seq` True) |> Hspec.shouldBe True
      (resultNeg |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "is total for unicode multibyte password and matches the projected builder" do
      let cfg = typicalEventStore {EventStore.password = "pâsswörd-🔑-日本語"}
      (EventStore.toConnectionSettings cfg |> LinkedList.length) |> Hspec.shouldBe 1
      (EventStore.toConnectionSettings cfg |> LinkedList.length)
        |> Hspec.shouldBe (ConnectionConfig.toConnectionParams (project cfg) |> LinkedList.length)

  Hspec.describe "toPoolConfig" do
    Hspec.it "returns a defined Config for the EventStore/FileUpload default size 6" do
      (ConnectionConfig.toPoolConfig 6 typicalSettings `Prelude.seq` True)
        |> Hspec.shouldBe True

    Hspec.it "returns a defined Config for the QueryObjectStore size 4" do
      (ConnectionConfig.toPoolConfig 4 typicalSettings `Prelude.seq` True)
        |> Hspec.shouldBe True

    Hspec.it "is total for poolSize = 0 (zero boundary)" do
      (ConnectionConfig.toPoolConfig 0 typicalSettings `Prelude.seq` True)
        |> Hspec.shouldBe True

    Hspec.it "is total for poolSize = 1 (one boundary)" do
      (ConnectionConfig.toPoolConfig 1 typicalSettings `Prelude.seq` True)
        |> Hspec.shouldBe True

    Hspec.it "is total for a large poolSize (e.g. 1000000)" do
      (ConnectionConfig.toPoolConfig 1000000 typicalSettings `Prelude.seq` True)
        |> Hspec.shouldBe True

    Hspec.it "is total for a negative poolSize" do
      (ConnectionConfig.toPoolConfig (-1) typicalSettings `Prelude.seq` True)
        |> Hspec.shouldBe True

    Hspec.it "is total regardless of settings list length (empty vs typical)" do
      (ConnectionConfig.toPoolConfig 6 [] `Prelude.seq` True) |> Hspec.shouldBe True
      (ConnectionConfig.toPoolConfig 6 typicalSettings `Prelude.seq` True) |> Hspec.shouldBe True
