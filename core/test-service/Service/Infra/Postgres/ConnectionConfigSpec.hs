module Service.Infra.Postgres.ConnectionConfigSpec (spec) where

-- | Pure unit spec for the ADR-0062 shared Postgres connection-settings
-- builders. Runs unconditionally (no Postgres). Registered manually in
-- core/test-service/Main.hs (the suite does not auto-discover).
--
-- The hasql Setting / Config values are opaque (no Eq/Show), so the
-- contract is asserted against the INSPECTABLE 'ResolvedParams' that the
-- opaque builder is constructed from: every host/db/user/password/port
-- value AND all four ADR-0037 keepalive entries are checked exactly, so a
-- dropped keepalive or a mis-mapped field fails the test. Port validation
-- (1..65535) is asserted on both boundaries and out-of-range inputs.

import Core
import LinkedList qualified
import Result qualified
import Service.Infra.Postgres.ConnectionConfig (ConnectionParams (..), ResolvedParams (..))
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Prelude qualified
import Test.Hspec qualified as Hspec


-- | A typical ConnectionParams input.
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
-- unambiguous -- a bare @typicalParams { port = n }@ update is ambiguous
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


-- | The four ADR-0037 keepalive entries every resolved param set must carry.
expectedKeepalives :: ResolvedParams -> Bool
expectedKeepalives resolved =
  resolved.keepalives Prelude.== "1"
    && resolved.keepalivesIdle Prelude.== "30"
    && resolved.keepalivesInterval Prelude.== "10"
    && resolved.keepalivesCount Prelude.== "5"


spec :: Hspec.Spec
spec = Hspec.describe "Service.Infra.Postgres.ConnectionConfig" do
  Hspec.describe "validatePort" do
    Hspec.it "rejects port 0 (lower boundary, below range)" do
      ConnectionConfig.validatePort 0
        |> Hspec.shouldBe (Err "port must be in 1..65535, got 0")

    Hspec.it "accepts port 1 (lower boundary)" do
      ConnectionConfig.validatePort 1 |> Hspec.shouldBe (Ok 1)

    Hspec.it "accepts port 5432 (typical)" do
      ConnectionConfig.validatePort 5432 |> Hspec.shouldBe (Ok 5432)

    Hspec.it "accepts port 65535 (upper boundary)" do
      ConnectionConfig.validatePort 65535 |> Hspec.shouldBe (Ok 65535)

    Hspec.it "rejects port 65536 (above range) instead of wrapping to 0" do
      ConnectionConfig.validatePort 65536
        |> Hspec.shouldBe (Err "port must be in 1..65535, got 65536")

    Hspec.it "rejects a negative port instead of wrapping to 65535" do
      ConnectionConfig.validatePort (-1)
        |> Hspec.shouldBe (Err "port must be in 1..65535, got -1")

  Hspec.describe "resolveParams" do
    Hspec.it "maps every field exactly and carries all four keepalives" do
      case ConnectionConfig.resolveParams typicalParams of
        Err err -> Prelude.error (Prelude.show err)
        Ok resolved -> do
          resolved.host |> Hspec.shouldBe "localhost"
          resolved.databaseName |> Hspec.shouldBe "app"
          resolved.user |> Hspec.shouldBe "postgres"
          resolved.password |> Hspec.shouldBe "secret"
          resolved.port |> Hspec.shouldBe 5432
          resolved.keepalives |> Hspec.shouldBe "1"
          resolved.keepalivesIdle |> Hspec.shouldBe "30"
          resolved.keepalivesInterval |> Hspec.shouldBe "10"
          resolved.keepalivesCount |> Hspec.shouldBe "5"

    Hspec.it "does not drop or reorder the keepalive contract" do
      case ConnectionConfig.resolveParams typicalParams of
        Err err -> Prelude.error (Prelude.show err)
        Ok resolved -> expectedKeepalives resolved |> Hspec.shouldBe True

    Hspec.it "preserves unicode multibyte text in the password field" do
      case ConnectionConfig.resolveParams (paramsWithPassword "p\xe2ssw\xf6rd-\x1f511-\x65e5\x672c\x8a9e") of
        Err err -> Prelude.error (Prelude.show err)
        Ok resolved -> resolved.password |> Hspec.shouldBe "p\xe2ssw\xf6rd-\x1f511-\x65e5\x672c\x8a9e"

    Hspec.it "preserves empty-string host, db, and user fields" do
      let emptyParams =
            ConnectionConfig.ConnectionParams
              { host = "",
                databaseName = "",
                user = "",
                password = "",
                port = 5432
              }
      case ConnectionConfig.resolveParams emptyParams of
        Err err -> Prelude.error (Prelude.show err)
        Ok resolved -> do
          resolved.host |> Hspec.shouldBe ""
          resolved.databaseName |> Hspec.shouldBe ""
          resolved.user |> Hspec.shouldBe ""
          resolved.password |> Hspec.shouldBe ""

    Hspec.it "fails fast on port 0 rather than producing settings" do
      ConnectionConfig.resolveParams (paramsWithPort 0)
        |> Result.isErr
        |> Hspec.shouldBe True

    Hspec.it "fails fast on a negative port" do
      ConnectionConfig.resolveParams (paramsWithPort (-1))
        |> Result.isErr
        |> Hspec.shouldBe True

    Hspec.it "fails fast on port 65536" do
      ConnectionConfig.resolveParams (paramsWithPort 65536)
        |> Result.isErr
        |> Hspec.shouldBe True

  Hspec.describe "toConnectionParams" do
    Hspec.it "returns a single connection setting for a typical config" do
      case ConnectionConfig.toConnectionParams typicalParams of
        Err err -> Prelude.error (Prelude.show err)
        Ok settings -> (settings |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "accepts the boundary port 1" do
      ConnectionConfig.toConnectionParams (paramsWithPort 1)
        |> Result.isOk
        |> Hspec.shouldBe True

    Hspec.it "accepts the boundary port 65535" do
      ConnectionConfig.toConnectionParams (paramsWithPort 65535)
        |> Result.isOk
        |> Hspec.shouldBe True

    Hspec.it "rejects an out-of-range port instead of silently wrapping" do
      -- The Ok payload (LinkedList Setting) has no Show, so assert the Err
      -- branch via a case rather than shouldBe on the whole Result.
      case ConnectionConfig.toConnectionParams (paramsWithPort 70000) of
        Err err -> err |> Hspec.shouldBe "port must be in 1..65535, got 70000"
        Ok _ -> Prelude.error "expected an out-of-range port to be rejected"

  Hspec.describe "toPoolConfig" do
    Hspec.it "returns a defined Config for the EventStore/FileUpload size 6" do
      case ConnectionConfig.toConnectionParams typicalParams of
        Err err -> Prelude.error (Prelude.show err)
        Ok settings ->
          (ConnectionConfig.toPoolConfig 6 settings `Prelude.seq` True)
            |> Hspec.shouldBe True

    Hspec.it "returns a defined Config for the QueryObjectStore size 4" do
      case ConnectionConfig.toConnectionParams typicalParams of
        Err err -> Prelude.error (Prelude.show err)
        Ok settings ->
          (ConnectionConfig.toPoolConfig 4 settings `Prelude.seq` True)
            |> Hspec.shouldBe True

    Hspec.it "is total regardless of settings list length (empty vs typical)" do
      (ConnectionConfig.toPoolConfig 6 [] `Prelude.seq` True) |> Hspec.shouldBe True
