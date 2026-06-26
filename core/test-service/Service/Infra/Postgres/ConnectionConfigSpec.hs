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
import Service.Infra.Postgres.ConnectionConfig (ConnectionParams (..), ResolvedParams (..), SslMode (..))
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
      port = 5432,
      sslMode = ConnectionConfig.SslModeUnset,
      sslRootCert = Nothing
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
      port = p,
      sslMode = ConnectionConfig.SslModeUnset,
      sslRootCert = Nothing
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
      port = 5432,
      sslMode = ConnectionConfig.SslModeUnset,
      sslRootCert = Nothing
    }


-- | The four ADR-0037 keepalive entries every resolved param set must carry.
expectedKeepalives :: ResolvedParams -> Bool
expectedKeepalives resolved =
  resolved.keepalives Prelude.== "1"
    && resolved.keepalivesIdle Prelude.== "30"
    && resolved.keepalivesInterval Prelude.== "10"
    && resolved.keepalivesCount Prelude.== "5"


-- | The typical config with explicit TLS fields, full record construction
-- (the constructor names the type) so the field set is unambiguous.
paramsWithSsl :: ConnectionConfig.SslMode -> Maybe Text -> ConnectionParams
paramsWithSsl mode rootCert =
  ConnectionConfig.ConnectionParams
    { host = "localhost",
      databaseName = "app",
      user = "postgres",
      password = "secret",
      port = 5432,
      sslMode = mode,
      sslRootCert = rootCert
    }


-- | Extract the length of an Ok payload, failing hard on Err (mirrors existing
-- idiom at spec lines 99/113/156). The Ok payload LinkedList Setting has no
-- Show, so we cannot shouldBe on the whole Result.
lengthOfOk :: Result Text (LinkedList a) -> Int
lengthOfOk result =
  case result of
    Ok xs -> LinkedList.length xs
    Err e -> Prelude.error (Prelude.show e)


-- | Extract the Ok (key, value) param-pair list, failing hard on Err. The pairs
-- have Eq/Show (unlike the opaque hasql Setting), so the contract can be
-- asserted exactly -- which key/value pairs are present, and which are NOT.
pairsOfOk :: Result Text (LinkedList (Text, Text)) -> LinkedList (Text, Text)
pairsOfOk result =
  case result of
    Ok pairs -> pairs
    Err e -> Prelude.error (Prelude.show e)


-- | True when the (key, value) pair list contains the given key (value
-- ignored) -- used to assert presence/absence of a param by name.
hasKey :: Text -> LinkedList (Text, Text) -> Bool
hasKey key pairs =
  pairs |> Prelude.any (\(k, _) -> k Prelude.== key)


-- | A remote-host config mirroring the #694 reproduction: a non-localhost host
-- that the bug silently replaced with the local Unix-socket default. Full
-- record construction for the same disambiguation reason as 'paramsWithPort'.
paramsRemoteHost :: ConnectionConfig.SslMode -> ConnectionParams
paramsRemoteHost mode =
  ConnectionConfig.ConnectionParams
    { host = "db.example.com",
      databaseName = "app",
      user = "postgres",
      password = "secret",
      port = 5432,
      sslMode = mode,
      sslRootCert = Nothing
    }

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
                port = 5432,
                sslMode = ConnectionConfig.SslModeUnset,
                sslRootCert = Nothing
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

  Hspec.describe "toConnectionParams (#694 regression: one connection setting)" do
    -- #694: hasql's 'staticConnectionSettings' applies every
    -- 'ConnectionSetting.connection' by REPLACING the whole connection string
    -- (last-wins). Emitting the TLS params as a SEPARATE connection Setting
    -- therefore silently dropped host/port/dbname and libpq fell back to the
    -- local Unix socket. The contract is now: ALL params live in exactly ONE
    -- connection Setting, so the list length is 1 for EVERY sslMode -- never
    -- 2+, which is the smoking gun of the host-dropping bug.
    Hspec.it "emits exactly ONE connection setting for SslModeRequire (a 2nd would drop host via last-wins)" do
      case ConnectionConfig.toConnectionParams (paramsWithSsl SslModeRequire Nothing) of
        Err err -> Prelude.error (Prelude.show err)
        Ok settings -> (settings |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "emits exactly ONE connection setting for verify-full + rootCert (sslmode+sslrootcert+channel_binding must not split it)" do
      case ConnectionConfig.toConnectionParams (paramsWithSsl SslModeVerifyFull (Just "/etc/ca.pem")) of
        Err err -> Prelude.error (Prelude.show err)
        Ok settings -> (settings |> LinkedList.length) |> Hspec.shouldBe 1

    Hspec.it "emits exactly ONE connection setting for EVERY SslMode (table sweep: none may add a host-dropping 2nd setting)" do
      let modes :: [SslMode]
          modes =
            [ SslModeUnset,
              SslModeDisable,
              SslModeAllow,
              SslModePrefer,
              SslModeRequire,
              SslModeVerifyCa,
              SslModeVerifyFull
            ]
      let allSingle =
            modes
              |> Prelude.all
                (\m -> lengthOfOk (ConnectionConfig.toConnectionParams (paramsWithSsl m (Just "/etc/ca.pem"))) Prelude.== 1)
      allSingle |> Hspec.shouldBe True

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

  Hspec.describe "sslModeToText" do
    Hspec.it "maps SslModeRequire to Just \"require\" (the canonical opt-in token)" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModeRequire
        |> Hspec.shouldBe (Just "require")

    Hspec.it "maps SslModeUnset to Nothing (no token — the default emits no sslmode param)" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModeUnset
        |> Hspec.shouldBe Nothing

    Hspec.it "maps SslModeDisable to Just \"disable\"" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModeDisable
        |> Hspec.shouldBe (Just "disable")

    Hspec.it "maps SslModeAllow to Just \"allow\"" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModeAllow
        |> Hspec.shouldBe (Just "allow")

    Hspec.it "maps SslModePrefer to Just \"prefer\"" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModePrefer
        |> Hspec.shouldBe (Just "prefer")

    Hspec.it "maps SslModeVerifyCa to Just \"verify-ca\"" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModeVerifyCa
        |> Hspec.shouldBe (Just "verify-ca")

    Hspec.it "maps SslModeVerifyFull to Just \"verify-full\" (the high-assurance token)" do
      ConnectionConfig.sslModeToText ConnectionConfig.SslModeVerifyFull
        |> Hspec.shouldBe (Just "verify-full")

    Hspec.it "is total over the 7-constructor enum (table sweep guards a future dropped/typo'd token)" do
      let table :: [(ConnectionConfig.SslMode, Maybe Text)]
          table =
            [ (ConnectionConfig.SslModeUnset, Nothing),
              (ConnectionConfig.SslModeDisable, Just "disable"),
              (ConnectionConfig.SslModeAllow, Just "allow"),
              (ConnectionConfig.SslModePrefer, Just "prefer"),
              (ConnectionConfig.SslModeRequire, Just "require"),
              (ConnectionConfig.SslModeVerifyCa, Just "verify-ca"),
              (ConnectionConfig.SslModeVerifyFull, Just "verify-full")
            ]
      (table |> Prelude.all (\(m, t) -> ConnectionConfig.sslModeToText m Prelude.== t))
        |> Hspec.shouldBe True

  Hspec.describe "textToSslMode" do
    Hspec.it "parses \"require\" to Ok SslModeRequire (the canonical operator token)" do
      ConnectionConfig.textToSslMode "require"
        |> Hspec.shouldBe (Ok SslModeRequire)

    Hspec.it "parses the empty string to Ok SslModeUnset (default-off: operator sets nothing)" do
      ConnectionConfig.textToSslMode ""
        |> Hspec.shouldBe (Ok SslModeUnset)

    Hspec.it "parses the literal \"unset\" to Ok SslModeUnset (the explicit default token)" do
      ConnectionConfig.textToSslMode "unset"
        |> Hspec.shouldBe (Ok SslModeUnset)

    Hspec.it "parses an unknown token \"requre\" to Err naming the bad value and the valid set" do
      ConnectionConfig.textToSslMode "requre"
        |> Hspec.shouldBe (Err "unknown DB_SSL_MODE \"requre\"; expected one of: unset, disable, allow, prefer, require, verify-ca, verify-full")

    Hspec.it "rejects a unicode-multibyte unknown token to Err (multibyte boundary on the Err branch)" do
      ConnectionConfig.textToSslMode "requëre"
        |> Hspec.shouldBe (Err "unknown DB_SSL_MODE \"requëre\"; expected one of: unset, disable, allow, prefer, require, verify-ca, verify-full")

    Hspec.it "parses \"disable\" to Ok SslModeDisable" do
      ConnectionConfig.textToSslMode "disable"
        |> Hspec.shouldBe (Ok SslModeDisable)

    Hspec.it "parses \"verify-full\" to Ok SslModeVerifyFull (hyphenated high-assurance token)" do
      ConnectionConfig.textToSslMode "verify-full"
        |> Hspec.shouldBe (Ok SslModeVerifyFull)

    Hspec.it "case-folds an uppercase token \"REQUIRE\" to Ok SslModeRequire" do
      ConnectionConfig.textToSslMode "REQUIRE"
        |> Hspec.shouldBe (Ok SslModeRequire)

    Hspec.it "trims surrounding whitespace \"  require  \" to Ok SslModeRequire" do
      ConnectionConfig.textToSslMode "  require  "
        |> Hspec.shouldBe (Ok SslModeRequire)

  Hspec.describe "resolveParams (additive sslMode/sslRootCert)" do
    Hspec.it "carries SslModeRequire and Just rootCert path through unchanged" do
      case ConnectionConfig.resolveParams (paramsWithSsl SslModeRequire (Just "/etc/ca.pem")) of
        Err _ -> Prelude.error "expected Ok but got Err"
        Ok r -> do
          r.sslMode |> Hspec.shouldBe SslModeRequire
          r.sslRootCert |> Hspec.shouldBe (Just "/etc/ca.pem")

    Hspec.it "carries SslModeUnset and Nothing through as the default-off mirror (no-regression)" do
      case ConnectionConfig.resolveParams (paramsWithSsl SslModeUnset Nothing) of
        Err _ -> Prelude.error "expected Ok but got Err"
        Ok r -> do
          r.sslMode |> Hspec.shouldBe SslModeUnset
          r.sslRootCert |> Hspec.shouldBe Nothing

    Hspec.it "carries SslModeVerifyFull with Just rootCert through (high-assurance copy-through)" do
      case ConnectionConfig.resolveParams (paramsWithSsl SslModeVerifyFull (Just "/etc/postgresql/azure-roots.pem")) of
        Err _ -> Prelude.error "expected Ok but got Err"
        Ok r -> do
          r.sslMode |> Hspec.shouldBe SslModeVerifyFull
          r.sslRootCert |> Hspec.shouldBe (Just "/etc/postgresql/azure-roots.pem")

    Hspec.it "carries Nothing rootCert distinctly from a path (boundary: no cert supplied)" do
      case ConnectionConfig.resolveParams (paramsWithSsl SslModeVerifyFull Nothing) of
        Err _ -> Prelude.error "expected Ok but got Err"
        Ok r ->
          r.sslRootCert |> Hspec.shouldBe Nothing

  Hspec.describe "toParamPairs (#694: base params survive every sslMode)" do
    -- The smoking gun of #694: with a set sslMode, the host/port/dbname/user/
    -- password coordinates were dropped and libpq fell back to the local Unix
    -- socket. The whole base set must now ride in the SAME param list as the
    -- TLS params, so every base coordinate is present for EVERY mode.
    Hspec.it "keeps the remote host param under SslModeRequire (the exact param #694 dropped)" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsRemoteHost SslModeRequire))
      (pairs |> Prelude.elem ("host", "db.example.com")) |> Hspec.shouldBe True

    Hspec.it "keeps the remote host param under SslModeVerifyFull" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsRemoteHost SslModeVerifyFull))
      (pairs |> Prelude.elem ("host", "db.example.com")) |> Hspec.shouldBe True

    Hspec.it "keeps every base coordinate (host/port/dbname/user/password) for EVERY sslMode -- table sweep" do
      let modes :: [SslMode]
          modes =
            [ SslModeUnset,
              SslModeDisable,
              SslModeAllow,
              SslModePrefer,
              SslModeRequire,
              SslModeVerifyCa,
              SslModeVerifyFull
            ]
      let baseKeys :: [Text]
          baseKeys = ["host", "port", "dbname", "user", "password"]
      let allModesKeepBase =
            modes
              |> Prelude.all
                ( \mode -> do
                    let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl mode Nothing))
                    baseKeys |> Prelude.all (\k -> hasKey k pairs)
                )
      allModesKeepBase |> Hspec.shouldBe True

  Hspec.describe "toParamPairs (additive sslMode/sslRootCert)" do
    Hspec.it "with SslModeUnset emits exactly the base params and NO ssl params (byte-identical no-regression baseline)" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeUnset Nothing))
      pairs
        |> Hspec.shouldBe
          [ ("host", "localhost"),
            ("port", "5432"),
            ("dbname", "app"),
            ("user", "postgres"),
            ("password", "secret"),
            ("keepalives", "1"),
            ("keepalives_idle", "30"),
            ("keepalives_interval", "10"),
            ("keepalives_count", "5")
          ]

    Hspec.it "with SslModeRequire appends sslmode=require after the base params (and nothing else)" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeRequire Nothing))
      pairs
        |> Hspec.shouldBe
          [ ("host", "localhost"),
            ("port", "5432"),
            ("dbname", "app"),
            ("user", "postgres"),
            ("password", "secret"),
            ("keepalives", "1"),
            ("keepalives_idle", "30"),
            ("keepalives_interval", "10"),
            ("keepalives_count", "5"),
            ("sslmode", "require")
          ]

    Hspec.it "with SslModeVerifyFull + cert emits exactly sslmode + sslrootcert + channel_binding and NO sslcert/sslkey (no-mTLS guarantee, ADR-0064 §3)" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeVerifyFull (Just "/etc/ca.pem")))
      (pairs |> Prelude.elem ("sslmode", "verify-full")) |> Hspec.shouldBe True
      (pairs |> Prelude.elem ("sslrootcert", "/etc/ca.pem")) |> Hspec.shouldBe True
      (pairs |> Prelude.elem ("channel_binding", "require")) |> Hspec.shouldBe True
      (pairs |> hasKey "sslcert") |> Hspec.shouldBe False
      (pairs |> hasKey "sslkey") |> Hspec.shouldBe False

    Hspec.it "with SslModeVerifyFull but Nothing rootCert emits sslmode only -- no sslrootcert, no channel_binding (boundary: empty cert path)" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeVerifyFull Nothing))
      (pairs |> Prelude.elem ("sslmode", "verify-full")) |> Hspec.shouldBe True
      (pairs |> hasKey "sslrootcert") |> Hspec.shouldBe False
      (pairs |> hasKey "channel_binding") |> Hspec.shouldBe False

    Hspec.it "with SslModeVerifyCa + cert emits sslrootcert but NOT channel_binding (channel_binding is verify-full only)" do
      let pairs = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeVerifyCa (Just "/etc/ca.pem")))
      (pairs |> Prelude.elem ("sslrootcert", "/etc/ca.pem")) |> Hspec.shouldBe True
      (pairs |> hasKey "channel_binding") |> Hspec.shouldBe False

    Hspec.it "does NOT emit sslrootcert for a non-verifying mode even when a cert path is supplied (verify-only gating)" do
      -- 'require' is non-verifying: a root cert is meaningless, so require+cert
      -- must equal require+no-cert (sslmode only).
      let reqWithCert = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeRequire (Just "/etc/ca.pem")))
      let reqNoCert = pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl SslModeRequire Nothing))
      reqWithCert |> Hspec.shouldBe reqNoCert
      (reqWithCert |> hasKey "sslrootcert") |> Hspec.shouldBe False

    Hspec.it "ignores a root cert for every non-verifying mode (disable/allow/prefer/require) -- table sweep of the verify-only gate" do
      let nonVerifying :: [SslMode]
          nonVerifying = [SslModeDisable, SslModeAllow, SslModePrefer, SslModeRequire]
      let noneEmitRootCert =
            nonVerifying
              |> Prelude.all
                ( \mode ->
                    Prelude.not (hasKey "sslrootcert" (pairsOfOk (ConnectionConfig.toParamPairs (paramsWithSsl mode (Just "/etc/ca.pem")))))
                )
      noneEmitRootCert |> Hspec.shouldBe True

  Hspec.describe "textToSslMode / sslModeToText round-trip" do
    Hspec.it "round-trips every token-bearing SslMode: parse (toText m) back to Ok m" do
      let modes :: [SslMode]
          modes = [SslModeDisable, SslModeAllow, SslModePrefer, SslModeRequire, SslModeVerifyCa, SslModeVerifyFull]
      let roundTrips =
            modes
              |> Prelude.all
                ( \m ->
                    case ConnectionConfig.sslModeToText m of
                      Just token -> ConnectionConfig.textToSslMode token Prelude.== Ok m
                      Nothing -> False
                )
      roundTrips |> Hspec.shouldBe True
