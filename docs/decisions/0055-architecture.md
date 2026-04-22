# ADR-0055 — Phase 5 Architecture Design

This document translates the approved ADR-0055 (declarative integrations with real/fake parity) into a drop-in implementation blueprint. The implementer follows it literally; every file path, type, signature and wire-up point below is intentional. No section is advisory — where the ADR left a choice, this document resolves it.

All paths are repo-relative to `/Users/nick/Source/NeoHaskell/.claude/worktrees/inspiring-cerf-72a1fa/`.

---

## 1. Module Layout

All production modules land in the existing `nhcore` `library` stanza. All test-only modules land in a **new** `library testing` stanza (see §5). Path assembly uses the existing `hs-source-dirs` convention — production modules under `core/service/`, test-only modules under `core/service/` (same tree; separated by cabal exposed-modules, not by directory).

| Path | Kind | Cabal stanza | Purpose |
|------|------|--------------|---------|
| `core/service/Service/Integration/Adapter.hs` | new | `library` | Declares the `Integration` and `InboundIntegration` typeclasses plus `InboundHandle`. Production-safe (no QuickCheck in class head). |
| `core/service/Service/Integration/IntegrationError.hs` | new | `library` | The narrowed `IntegrationError` ADT from ADR §11. Broken out of `Integration.hs` so `Adapter.hs` can import it without cycles. |
| `core/service/Service/Integration/Selection.hs` | new | `library` | `Selection` ADT, CLI-flag+env-var parsing, startup validation, dispatcher-shim construction, `FakeNameRegistry` type. |
| `core/service/Service/Integration/Canonical.hs` | new | `library` | First-party RFC 8785 canonical-JSON encoder, SHA-256 hasher, `Canonical.version`. |
| `core/service/Service/Integration/Fixture.hs` | new | `library` | Production-safe fixture *lookup* (read-only path), including path validation against the project root. |
| `core/service/Service/Integration/Debug.hs` | new | `library` | `IntegrationDebug` structured-log record and its redaction-aware `log` helper. |
| `core/service/Service/Integration/FixtureKey.hs` | new | `library` | `FixtureKey` newtype wrapping canonical hash for DevEx ergonomics (Phase 4 polish item). |
| `core/service/Service/Integration/Inbound.hs` | new | `library` | `InboundHandle`, `controllableHandle`, `controllableHandleWith`. Production-safe. |
| `core/service/Service/Integration/DispatchRegistry.hs` | new | `library` | `DispatchRegistry`, `TypeRep`-keyed map of pre-bound `request -> Task IntegrationError (Response request)` closures. Populated at startup, read at emit time. |
| `core/service/Service/Integration/ShimEmit.hs` | new | `library` | The production `emit` shim — one-direct-call hot path, `{-# INLINE #-}`. |
| `core/service/Service/Integration/Canonical/Version.hs` | new | `library` | Standalone module holding `Canonical.version :: Int` so fixture code can import without pulling encoder module. |
| `core/service/Integration.hs` | modified | `library` | Top-level re-export. Drop `ToAction`, `Integration.inbound`, `Integration.Inbound`, `Integration.outbound`; re-export new typeclasses + `IntegrationError`. |
| `core/service/Service/Integration/Types.hs` | modified | `library` | `OutboundRunner.processEvent` signature unchanged; its body now invokes the dispatcher shim (see §7). `WorkerState` unchanged. |
| `core/service/Service/Application.hs` | modified | `library` | `Application.run` gains the selection-parsing step (§6). `Application.withInbound` accepts an `InboundIntegration`-backed worker (§8). |
| `core/service/Service/Application/Integrations.hs` | modified | `library` | `createTypedOutboundRunner` consumes the new `Integration.emit` shim instead of `Integration.runAction`. `withInbound` delegates to a `runInbound`/`runInboundFake` split. |
| `core/service/Service/OutboundIntegration/TH.hs` | modified | `library` | Generated `handleEventImpl` body calls `Integration.emit` instead of `Integration.outbound`. |
| `core/service/Integration/Timer.hs` | modified | `library` | Timer integration (shipped with nhcore) rewritten as an `InboundIntegration` instance. |
| `core/service/Integration/Command.hs` | modified | `library` | Command-emitting inbound rewritten as an `InboundIntegration` instance. |
| `core/service/Integration/Exit.hs` | modified | `library` | Exit integration rewritten as an `InboundIntegration` instance. |
| `core/service/Integration/Lifecycle.hs` | modified | `library` | Stateful outbound lifecycle helper retained; no longer depends on `ToAction`. |
| `core/service/Test/Integration.hs` | new | `library testing` | Umbrella module re-exporting `simulate`, `record`, `promote`, `fakeProperty`, `RedactionRule`, `defaultRedactionRules`. |
| `core/service/Test/Integration/Simulate.hs` | new | `library testing` | `simulate :: Trigger inbound -> Task IntegrationError ()`. |
| `core/service/Test/Integration/Fixture.hs` | new | `library testing` | `record`, `promote`, `RedactionRule`, `defaultRedactionRules`, entropy / secret scan. |
| `core/service/Test/Integration/Property.hs` | new | `library testing` | `fakeProperty` — user-facing property helper. |
| `core/service/Test/Integration/Contract.hs` | new | `library testing` | Auto-generated contract-test harness (schema-drift detection). |
| `core/service/Test/Integration/EntropyScan.hs` | new | `library testing` | Entropy + known-prefix secret scanner used by `Fixture.record` / `Fixture.promote`. |
| `core/service/test/Canonical/Vectors/` | new | `test-suite nhcore-test-core` | RFC 8785 conformance test vectors (JSON files, one per case). |
| `core/test/Service/Integration/AdapterSpec.hs` | new | `test-suite nhcore-test-service` | Class-shape and default-runFake behaviour tests. |
| `core/test/Service/Integration/CanonicalSpec.hs` | new | `test-suite nhcore-test-core` | Canonical-JSON conformance tests (drives the `Canonical/Vectors/` tree). |
| `core/test/Service/Integration/FixtureSpec.hs` | new | `test-suite nhcore-test-service` | Fixture lookup + path-traversal safety. |
| `core/test/Service/Integration/SelectionSpec.hs` | new | `test-suite nhcore-test-service` | CLI flag + env-var gate behaviour. |
| `core/test/Service/Integration/DispatchRegistrySpec.hs` | new | `test-suite nhcore-test-service` | `TypeRep`-keyed closure lookup. |
| `core/test/Service/Integration/InboundSpec.hs` | new | `test-suite nhcore-test-integration` | `controllableHandle` bounded-channel behaviour. |
| `core/test/Service/Integration/RecorderSpec.hs` | new | `test-suite nhcore-test-service` | Recorder entropy scan + `NEOHASKELL_RECORD_FIXTURES` gating + promote gate. |
| `core/test/Service/Integration/PropertySpec.hs` | new | `test-suite nhcore-test-service` | `fakeProperty` smoke test. |
| `core/test/Service/Integration/ContractSpec.hs` | new | `test-suite nhcore-test-service` | Schema-drift detection against a planted-drift response type. |
| `core/service/test/Canonical/Vectors/README.md` | new | — | (auto-ignored by cabal) Human note on where the vectors came from. Not a Haskell file. |

Deletions:

- `core/service/Integration.hs`'s public surface loses `ToAction (..)`, `action`, `outbound`, `inbound`, `InboundConfig`, `Inbound (..)`, `runAction`, `getActions`, `runInbound`. Nothing else in the file is removed (`ActionContext`, `FileAccessContext`, `ImmutableProviderRegistry`, `CommandPayload`, `emitCommand`, `noCommand`, `encodeCommand`, `makeCommandPayload`, `fromMap`, `lookup`, `entries` all stay — they are consumed by `OutboundIntegration` and OAuth2 machinery unrelated to this ADR).

No files are *physically* deleted; only the exported surface of `core/service/Integration.hs` narrows.

---

## 2. Type Definitions

Every definition below compiles under the project-wide `Strict` and `NoImplicitPrelude` extensions. Deriving clauses preserve the order seen in existing nhcore modules: `Eq, Show, Generic` (then any JSON / schema / Arbitrary extensions). `Strict` is on, so **no `!` annotations appear**; `{-# UNPACK #-}` is used only on primitive-bearing newtypes. Type parameters are descriptive (`request`, `response`, `inbound`, `trigger`) — never single-letter.

### 2.1 `Integration` typeclass — `Service.Integration.Adapter`

```haskell
class Integration request where
  type Response request :: Type

  runReal :: request -> Task IntegrationError (Response request)

  runFake :: request -> Task IntegrationError (Response request)
  default runFake ::
    forall.
    (QuickCheck.Arbitrary (Response request)) =>
    request ->
    Task IntegrationError (Response request)
  runFake _request = do
    generated <- Task.fromIO (QuickCheck.generate QuickCheck.arbitrary)
    Task.yield generated
```

No role annotation needed (no coercion tricks). Associated-type family `Response` is open; each instance ties it to the concrete response record.

### 2.2 `InboundIntegration` typeclass — `Service.Integration.Adapter`

```haskell
class InboundIntegration inbound where
  type Trigger inbound :: Type

  runReal ::
    (Trigger inbound -> Task IntegrationError Unit) ->
    Task IntegrationError Void

  runFake :: InboundHandle (Trigger inbound)
  default runFake ::
    forall.
    (QuickCheck.Arbitrary (Trigger inbound)) =>
    InboundHandle (Trigger inbound)
  runFake = Inbound.controllableHandle
```

### 2.3 `InboundHandle` — `Service.Integration.Inbound`

```haskell
data InboundHandle trigger = InboundHandle
  { drive :: (trigger -> Task IntegrationError Unit) -> Task IntegrationError Void
  , inject :: trigger -> Task IntegrationError Unit
  , capacity :: Int
  }
```

No `deriving` — contains function fields. `drive` is what the runtime calls in fake mode (analogue of `runReal`); `inject` is what `Test.Integration.Simulate.simulate` calls to push a synthetic trigger. `capacity` is the bounded-channel capacity used by `controllableHandle` (default 1024).

### 2.4 `IntegrationError` — `Service.Integration.IntegrationError`

Replaces the old ADT in `core/service/Integration.hs`. Re-exported from `Integration` top-level module.

```haskell
data IntegrationError
  = TransportFailure Text
  | AuthenticationFailure
  | PermanentFailure Text
  | TransientFailure Text
  | ValidationFailure Text
  deriving (Eq, Show, Generic)
```

`AuthenticationFailure` carries no payload per ADR §11. `Generic` derivation is intentional — enables `ToJSON` (hand-written; see §3) without leaking `Generic` downstream for Redacted-like safety reasons.

### 2.5 `IntegrationDebug` — `Service.Integration.Debug`

```haskell
data IntegrationDebug = IntegrationDebug
  { integrationName :: Text
  , requestHash :: Text
  , statusCode :: Maybe Int
  , authHeader :: Redacted Text
  , responseSnippet :: Redacted Text
  }
  deriving (Generic)
```

Deliberately no `Eq`, no `Show` (the `Show` instance is hand-written in `Service.Integration.Debug` to render `Redacted` fields correctly and nothing else), no `ToJSON`. Logger renders via a redaction-aware formatter (also defined in this module).

### 2.6 `Selection` — `Service.Integration.Selection`

```haskell
data Selection
  = Real
  | Fake
  | Hybrid (Array Text)
  deriving (Eq, Show, Generic)
```

The `Array Text` in `Hybrid` holds the ADR-canonical integration names (values of `NameOf request` for each `--fake=NAME` the user passed). Never user-input text directly — each entry is normalised at parse time via the `NameOf` validation regex.

### 2.7 `RedactionRule` + `defaultRedactionRules` — `Test.Integration.Fixture`

```haskell
data RedactionRule
  = RedactJsonPath Text Text
  | RedactHeader Text Text
  | RedactMatching Regex Text
  | AllowEntropyPath Text
  deriving (Eq, Show, Generic)

defaultRedactionRules :: Array RedactionRule
```

`Regex` is the existing NeoHaskell type (already present via the megaparsec / text-regex stack — if absent, the implementer adds `regex-tdfa` to cabal; see §5). `AllowEntropyPath` corresponds to the ADR's `RedactionRule.allow` escape hatch.

### 2.8 `FixtureKey` — `Service.Integration.FixtureKey`

Phase-4 DevEx polish: a newtype around the hex hash so accidental misuse (passing a raw `Text` filename into a fixture-lookup API) is a type error.

```haskell
newtype FixtureKey = FixtureKey Text
  deriving (Eq, Show, Generic)

FixtureKey.fromRequest :: (Integration request, ToEncoding request) => request -> FixtureKey
FixtureKey.toText :: FixtureKey -> Text
```

Constructor exported only from the module itself; all other modules must use `fromRequest`. No `IsString` instance (forces the through-hash construction path).

### 2.9 `Canonical.version` and encoder signatures — `Service.Integration.Canonical`

```haskell
version :: Int
version = 1

encode :: (Aeson.ToEncoding value) => value -> ByteString
hash   :: (Aeson.ToEncoding value) => value -> Text
```

`ByteString` is the `bytestring` strict variant (already a direct dependency, see `core/nhcore.cabal:30`). `Text` is the nhcore `Text` — not `Data.Text`. `hash` returns lowercase hex, 64 characters, full SHA-256 (no truncation).

### 2.10 `DispatchRegistry` — `Service.Integration.DispatchRegistry`

Type-erased storage for pre-bound dispatcher closures, keyed by `TypeRep`:

```haskell
newtype DispatchClosure = DispatchClosure
  (forall request. (Integration request, Typeable request) =>
    request -> Task IntegrationError (Response request))

newtype DispatchRegistry = DispatchRegistry
  (Map TypeRep (Unknown.Unknown))
  deriving (Generic)

DispatchRegistry.empty :: DispatchRegistry

DispatchRegistry.register ::
  forall request.
  (Integration request, Typeable request) =>
  (request -> Task IntegrationError (Response request)) ->
  DispatchRegistry ->
  DispatchRegistry

DispatchRegistry.lookup ::
  forall request.
  (Integration request, Typeable request) =>
  DispatchRegistry ->
  Maybe (request -> Task IntegrationError (Response request))
```

The `Unknown.Unknown` existential is the nhcore trait type (`core/core/Unknown.hs`, already used by options-parser — see `core/options-parser/Command.hs:48`) that lets us round-trip through `TypeRep` without pulling in `unsafeCoerce` at call sites. `DispatchRegistry.lookup` performs a `cast`-style round-trip and yields `Maybe` on mismatch; the emit shim interprets `Nothing` as a panic-level error (missing registration) and reports through `IntegrationError.PermanentFailure [fmt|...not registered...|]`.

### 2.11 `FakeNameRegistry` — `Service.Integration.Selection`

```haskell
newtype FakeNameRegistry = FakeNameRegistry (Set Text)
  deriving (Eq, Show, Generic)

FakeNameRegistry.member :: Text -> FakeNameRegistry -> Bool
FakeNameRegistry.fromArray :: Array Text -> FakeNameRegistry
```

Used by the hybrid-mode dispatcher to decide per-`NameOf` whether to route to `runReal` or the fake/fixture path.

---

## 3. Public API Signatures

Only public (`exposed-modules`) signatures appear here. Imports are specified in §4.

### 3.1 `module Service.Integration.Adapter`

```haskell
class Integration request where
  type Response request :: Type
  runReal :: request -> Task IntegrationError (Response request)
  runFake :: request -> Task IntegrationError (Response request)
  default runFake ::
    (QuickCheck.Arbitrary (Response request)) =>
    request -> Task IntegrationError (Response request)

class InboundIntegration inbound where
  type Trigger inbound :: Type
  runReal ::
    (Trigger inbound -> Task IntegrationError Unit) ->
    Task IntegrationError Void
  runFake :: InboundHandle (Trigger inbound)
  default runFake ::
    (QuickCheck.Arbitrary (Trigger inbound)) =>
    InboundHandle (Trigger inbound)
```

### 3.2 `module Service.Integration.IntegrationError`

```haskell
data IntegrationError
  = TransportFailure Text
  | AuthenticationFailure
  | PermanentFailure Text
  | TransientFailure Text
  | ValidationFailure Text

instance Aeson.ToJSON IntegrationError   -- hand-written, no Generic derive
instance Aeson.FromJSON IntegrationError  -- hand-written
```

### 3.3 `module Service.Integration.Inbound`

```haskell
controllableHandle ::
  forall trigger.
  (QuickCheck.Arbitrary trigger) =>
  InboundHandle trigger

controllableHandleWith ::
  forall trigger.
  Int ->
  InboundHandle trigger
```

### 3.4 `module Service.Integration.Selection`

```haskell
data Selection = Real | Fake | Hybrid (Array Text)

-- Parse CLI args + env vars into a Selection. Errors strictly per ADR §3.
parseSelection :: Task Text Selection

-- Startup-time validation + logging.
validateOrThrow :: Selection -> Task Text Selection

-- Look up the fake-set for hybrid mode.
isFakeByName :: Text -> Selection -> Bool

newtype FakeNameRegistry = FakeNameRegistry (Set Text)

fromSelection :: Selection -> FakeNameRegistry
```

### 3.5 `module Service.Integration.Canonical`

```haskell
encode :: (Aeson.ToEncoding value) => value -> ByteString
hash   :: (Aeson.ToEncoding value) => value -> Text
version :: Int
```

### 3.6 `module Service.Integration.Canonical.Version`

```haskell
version :: Int
```

(Identical to `Canonical.version`; split out so fixture code can import it without pulling the encoder module.)

### 3.7 `module Service.Integration.Fixture`

```haskell
-- Read-only, production-safe.
data FixtureMiss
  = FixtureFileAbsent
  | FixtureOutsideRoot
  | FixtureNameInvalid Text
  | FixtureDecodeError Text

lookupResponse ::
  forall request.
  (Integration request, Aeson.FromJSON (Response request),
   Aeson.ToEncoding request, Typeable request,
   GHC.KnownSymbol (NameOf request)) =>
  Path -> -- project root
  request ->
  Task Text (Result FixtureMiss (Response request))

-- Pure path builder + validator. Exposed for testing.
resolvePath ::
  Path ->   -- project root (absolute)
  Text ->   -- integration name (after NameOf validation)
  FixtureKey ->
  Task Text Path

validateIntegrationName :: Text -> Result Text Text
```

### 3.8 `module Service.Integration.FixtureKey`

```haskell
newtype FixtureKey = FixtureKey Text

fromRequest ::
  forall request.
  (Aeson.ToEncoding request) =>
  request -> FixtureKey

toText :: FixtureKey -> Text
```

### 3.9 `module Service.Integration.Debug`

```haskell
data IntegrationDebug = IntegrationDebug
  { integrationName :: Text
  , requestHash :: Text
  , statusCode :: Maybe Int
  , authHeader :: Redacted Text
  , responseSnippet :: Redacted Text
  }

log :: IntegrationDebug -> Task Text Unit
```

### 3.10 `module Service.Integration.DispatchRegistry`

```haskell
data DispatchRegistry

empty :: DispatchRegistry

register ::
  forall request.
  (Integration request, Typeable request) =>
  (request -> Task IntegrationError (Response request)) ->
  DispatchRegistry ->
  DispatchRegistry

lookup ::
  forall request.
  (Integration request, Typeable request) =>
  DispatchRegistry ->
  Maybe (request -> Task IntegrationError (Response request))
```

### 3.11 `module Service.Integration.ShimEmit`

The production hot-path helper. **Never** performs a per-call env-var read or a per-call `Selection` case.

```haskell
emit ::
  forall request.
  (Integration request, Typeable request) =>
  DispatchRegistry ->
  request ->
  Task IntegrationError (Response request)
{-# INLINE emit #-}
```

### 3.12 `module Integration` (top-level re-export, narrowed)

```haskell
module Integration
  ( Integration (..)
  , InboundIntegration (..)
  , InboundHandle
  , IntegrationError (..)
  , ActionContext (..)
  , FileAccessContext (..)
  , ImmutableProviderRegistry
  , CommandPayload (..)
  , emitCommand, noCommand, makeCommandPayload, encodeCommand
  , fromMap, lookup, entries
  ) where
```

`ToAction`, `Integration.outbound`, `Integration.inbound`, `Integration.Inbound` are **gone**. The `Action` / `ActionContext` surface stays (OAuth2 machinery consumes it).

### 3.13 `module Test.Integration.Simulate`

```haskell
simulate ::
  forall inbound.
  (InboundIntegration inbound) =>
  Trigger inbound ->
  Task IntegrationError Unit
```

### 3.14 `module Test.Integration.Fixture` (test-only)

```haskell
record ::
  forall request.
  (Integration request, Aeson.ToJSON request, Aeson.ToEncoding request,
   GHC.KnownSymbol (NameOf request), Typeable request,
   Aeson.ToJSON (Response request), Aeson.FromJSON (Response request)) =>
  Array RedactionRule ->
  Array request ->
  Task IntegrationError Unit

promote ::
  forall request.
  (Integration request, GHC.KnownSymbol (NameOf request)) =>
  Array Text ->
  Task IntegrationError Unit

data RedactionRule = ...
defaultRedactionRules :: Array RedactionRule
```

### 3.15 `module Test.Integration.Property` (test-only)

```haskell
fakeProperty ::
  forall request.
  (Integration request, QuickCheck.Arbitrary request, Show request) =>
  (request -> Response request -> QuickCheck.Property) ->
  QuickCheck.Property
```

### 3.16 `module Test.Integration.Contract` (test-only)

```haskell
-- Generate the per-instance contract test bundle.
contractTests ::
  forall request.
  (Integration request, QuickCheck.Arbitrary request,
   Aeson.ToJSON (Response request), Aeson.FromJSON (Response request),
   Aeson.ToEncoding (Response request), Schema.ToSchema (Response request),
   Typeable request, GHC.KnownSymbol (NameOf request)) =>
  Spec Unit
```

### 3.17 `module Test.Integration.EntropyScan` (test-only)

```haskell
scanForSecrets ::
  Array RedactionRule ->
  Aeson.Value ->
  Result (Array Text) Aeson.Value -- Err if any secrets were found; Ok if clean
```

---

## 4. Module-by-Module Import Map

Imports follow the style guide: types unqualified, modules qualified, nhcore modules before Haskell ecosystem modules. `Test.QuickCheck qualified as QuickCheck` is the project-wide alias (already used in `core/test/OutboundIntegrationSpec.hs`). `Service.Integration.Canonical qualified as Canonical` is the new alias.

### 4.1 `Service/Integration/Adapter.hs`

```haskell
module Service.Integration.Adapter
  ( Integration (..)
  , InboundIntegration (..)
  ) where

import Core
import Service.Integration.Inbound (InboundHandle)
import Service.Integration.Inbound qualified as Inbound
import Service.Integration.IntegrationError (IntegrationError)
import Task qualified
import Test.QuickCheck qualified as QuickCheck
```

### 4.2 `Service/Integration/IntegrationError.hs`

```haskell
module Service.Integration.IntegrationError (IntegrationError (..)) where

import Core
import Data.Aeson qualified as Aeson
import Text (Text)
```

### 4.3 `Service/Integration/Inbound.hs`

```haskell
module Service.Integration.Inbound
  ( InboundHandle (..)
  , controllableHandle
  , controllableHandleWith
  ) where

import Core
import Channel (Channel)
import Channel qualified
import Service.Integration.IntegrationError (IntegrationError)
import Task qualified
import Test.QuickCheck qualified as QuickCheck
```

### 4.4 `Service/Integration/Selection.hs`

```haskell
module Service.Integration.Selection
  ( Selection (..)
  , FakeNameRegistry
  , parseSelection
  , validateOrThrow
  , isFakeByName
  , fromSelection
  ) where

import Core
import Array qualified
import Command qualified
import Environment qualified
import Log qualified
import Set (Set)
import Set qualified
import Task qualified
import Text qualified
```

### 4.5 `Service/Integration/Canonical.hs`

```haskell
module Service.Integration.Canonical (encode, hash, version) where

import Core
import Crypto.Hash qualified as Hash
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as AesonEnc
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Scientific qualified as Scientific
import Service.Integration.Canonical.Version qualified as Version
import Text qualified
```

### 4.6 `Service/Integration/Canonical/Version.hs`

```haskell
module Service.Integration.Canonical.Version (version) where

version :: Int
version = 1
```

### 4.7 `Service/Integration/Fixture.hs`

```haskell
module Service.Integration.Fixture
  ( FixtureMiss (..)
  , lookupResponse
  , resolvePath
  , validateIntegrationName
  ) where

import Core
import Data.Aeson qualified as Aeson
import File qualified
import Path qualified
import Service.Command.Core (NameOf)
import Service.Integration.Adapter (Integration (..))
import Service.Integration.Canonical qualified as Canonical
import Service.Integration.FixtureKey (FixtureKey)
import Service.Integration.FixtureKey qualified as FixtureKey
import Service.Integration.IntegrationError (IntegrationError)
import System.Directory qualified as GhcDirectory
import System.FilePath qualified as GhcFilePath
import Task qualified
import Text qualified
```

### 4.8 `Service/Integration/FixtureKey.hs`

```haskell
module Service.Integration.FixtureKey (FixtureKey, fromRequest, toText) where

import Core
import Data.Aeson qualified as Aeson
import Service.Integration.Canonical qualified as Canonical
```

### 4.9 `Service/Integration/Debug.hs`

```haskell
module Service.Integration.Debug (IntegrationDebug (..), log) where

import Core
import Log qualified
import Redacted qualified
import Task qualified
```

### 4.10 `Service/Integration/DispatchRegistry.hs`

```haskell
module Service.Integration.DispatchRegistry
  ( DispatchRegistry
  , empty
  , register
  , lookup
  ) where

import Core
import Data.Typeable (TypeRep, Typeable, typeRep)
import Map qualified
import Service.Integration.Adapter (Integration, Response)
import Service.Integration.IntegrationError (IntegrationError)
import Unknown qualified
```

### 4.11 `Service/Integration/ShimEmit.hs`

```haskell
module Service.Integration.ShimEmit (emit) where

import Core
import Service.Integration.Adapter (Integration, Response)
import Service.Integration.DispatchRegistry (DispatchRegistry)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.IntegrationError (IntegrationError (..))
import Task qualified
import TypeName qualified
```

### 4.12 `Test/Integration/Simulate.hs`

```haskell
module Test.Integration.Simulate (simulate) where

import Core
import Service.Integration.Adapter (InboundIntegration (..))
import Service.Integration.Inbound (InboundHandle (..))
import Service.Integration.IntegrationError (IntegrationError)
import Task qualified
```

### 4.13 `Test/Integration/Fixture.hs`

```haskell
module Test.Integration.Fixture
  ( record
  , promote
  , RedactionRule (..)
  , defaultRedactionRules
  ) where

import Core
import Array qualified
import Data.Aeson qualified as Aeson
import Environment qualified
import File qualified
import Path qualified
import Service.Command.Core (NameOf)
import Service.Integration.Adapter (Integration (..))
import Service.Integration.Canonical qualified as Canonical
import Service.Integration.FixtureKey qualified as FixtureKey
import Service.Integration.IntegrationError (IntegrationError (..))
import Task qualified
import Test.Integration.EntropyScan qualified as EntropyScan
import Text qualified
```

### 4.14 `Test/Integration/Property.hs`

```haskell
module Test.Integration.Property (fakeProperty) where

import Core
import Service.Integration.Adapter (Integration (..))
import Task qualified
import Test.QuickCheck qualified as QuickCheck
```

### 4.15 `Test/Integration/Contract.hs`

```haskell
module Test.Integration.Contract (contractTests) where

import Core
import Data.Aeson qualified as Aeson
import Environment qualified
import Schema qualified
import Service.Integration.Adapter (Integration (..))
import Task qualified
import Test qualified
import Test.QuickCheck qualified as QuickCheck
```

### 4.16 `Test/Integration/EntropyScan.hs`

```haskell
module Test.Integration.EntropyScan (scanForSecrets) where

import Core
import Array qualified
import Data.Aeson qualified as Aeson
import Text qualified
```

---

## 5. Cabal Changes

All edits to `core/nhcore.cabal`.

### 5.1 `library` stanza — add new exposed modules

Under `exposed-modules:` (insert alphabetically where appropriate — Haskell cabal has no ordering requirement, but convention in `nhcore.cabal` is alphabetical within a family prefix):

```
+   Service.Integration.Adapter
+   Service.Integration.Canonical
+   Service.Integration.Canonical.Version
+   Service.Integration.Debug
+   Service.Integration.DispatchRegistry
+   Service.Integration.Fixture
+   Service.Integration.FixtureKey
+   Service.Integration.Inbound
+   Service.Integration.IntegrationError
+   Service.Integration.Selection
+   Service.Integration.ShimEmit
```

`Service.Integration.Dispatcher` and `Service.Integration.Types` are already exposed (`core/nhcore.cabal:192-193`) and stay.

### 5.2 `library` stanza — no new deps

The five new production-side libraries are all already in `common_cfg.build-depends` (see `core/nhcore.cabal:23-96`): `aeson` (for `ToEncoding`), `bytestring` (for canonical encoder output), `crypton` (for `SHA-256` via `Crypto.Hash`), `QuickCheck` (for the constrained default, not linked through the class dict), `scientific` (for ECMA-262-compliant number formatting — already pinned at `>= 0.3 && < 0.4`). **No new build-depend lines are required for the production library.**

### 5.3 New `library testing` stanza

Immediately after the main `library` stanza and before the first `test-suite`:

```cabal
library testing
  import: common_cfg
  visibility: public
  exposed-modules:
    Test.Integration
    Test.Integration.Contract
    Test.Integration.EntropyScan
    Test.Integration.Fixture
    Test.Integration.Property
    Test.Integration.Simulate
  hs-source-dirs: service
  default-language: GHC2021
  build-depends:
    base,
    nhcore,
    QuickCheck,
    aeson,
    bytestring,
    crypton,
    hspec,
    regex-tdfa,
```

`regex-tdfa` is new — add it to `common_cfg.build-depends` (it is not yet listed). This is the single new Hackage dep this ADR introduces, used exclusively by the test-only entropy scanner and `RedactionRule.RedactMatching`.

### 5.4 Test-suite updates

Each test-suite that exercises Test-side modules gains `nhcore:testing` in its `build-depends`:

```
test-suite nhcore-test
  ...
  build-depends:
    base,
    hspec,
    nhcore,
+   nhcore:testing,

test-suite nhcore-test-service
  ...
  build-depends:
    base,
    hspec,
    nhcore,
+   nhcore:testing,

test-suite nhcore-test-integration
  ...
  build-depends:
    base,
    hspec,
    nhcore,
+   nhcore:testing,

test-suite nhcore-test-core
  ...
  build-depends:
    base,
    hspec,
    nhcore,
+   nhcore:testing,
```

`nhcore-test-auth` does not reference `Test.Integration.*` and keeps its existing deps.

### 5.5 New other-modules for canonical conformance

In `test-suite nhcore-test-core`, add:

```
+   Service.Integration.CanonicalSpec
```

In `test-suite nhcore-test-service`, add:

```
+   Service.Integration.AdapterSpec
+   Service.Integration.FixtureSpec
+   Service.Integration.SelectionSpec
+   Service.Integration.DispatchRegistrySpec
+   Service.Integration.RecorderSpec
+   Service.Integration.PropertySpec
+   Service.Integration.ContractSpec
```

In `test-suite nhcore-test-integration`, add:

```
+   Service.Integration.InboundSpec
```

---

## 6. Wire-Up Integration with `Application.run`

The selection logic lands in `core/service/Service/Application.hs` `run`, between step 1 (`.env` load, line 762) and step 2 (config load, line 765). The ordering is deliberate: `.env` is loaded *before* we read `NEOHASKELL_ALLOW_FAKE_INTEGRATIONS`, so a team that stores the gate env var in `.env` for local dev still works. CLI args are always visible (already-process-args), so they are parsed after the env is primed.

### 6.1 Precise hook location

```haskell
-- In core/service/Service/Application.hs, between lines 762 and 764:
     -- 1.5. Resolve integration-selection mode (CLI flag + env-var gate, ADR-0055 §3)
     selection <- Selection.parseSelection
       |> Task.mapError (\err -> [fmt|Integration selection error: #{err}|])
     selection <- Selection.validateOrThrow selection
```

The `Selection` value then travels alongside `app.dispatcherConfig` down to `Integrations.startIntegrationSubscriber` and `Integrations.startInboundWorkers` (both of which gain a new `Selection` parameter in §8's migration).

### 6.2 `Selection.parseSelection`

Implementation outline (signatures only, per the phase-5 contract):

```haskell
parseSelection :: Task Text Selection
```

Reads, in order:

1. `NEOHASKELL_ALLOW_FAKE_INTEGRATIONS` via `Environment.getVariable`. Missing or anything other than `"1"` ⇒ the gate is closed.
2. CLI arg parsing via the project's OptEnvConf wiring in `core/options-parser/Command.hs` — we add a dedicated parser `Command.flag` call for `--integrations=real|fake|hybrid` and a multi-value `--fake=NAME` parser. Parser definitions live in `Service.Integration.Selection`, invoked from `parseSelection` using the same `OptEnvConf.runParserOn` pattern used by `Config.load` (see `core/config/Config.hs:187-190`).
3. If CLI said `fake` or `hybrid` and the gate is closed, throw:

```
--integrations=fake requires NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1
in the process environment. This gate exists so production binaries
cannot silently dispatch to fake implementations. Set the env var in
your test harness or staging deployment only — never in production.
See: https://neohaskell.org/docs/integrations/fake-mode
```

(Exact text from ADR §3. Constructed via `[fmt|...|]` with `#{flag}` interpolation for the first line.)

4. If hybrid mode, collect the `--fake=NAME` values. Each NAME must pass `validateIntegrationName` (see §9) before being accepted. Invalid names ⇒ startup aborts with `PermanentFailure [fmt|--fake=#{name}: name must match [A-Za-z0-9_]+|]`.

### 6.3 `Selection.validateOrThrow`

- Logs, at `ERROR` level, one line naming the active fake list when mode is non-Real (per ADR §3, always visible in aggregators).
- Writes the selection to `Log.withScope [("integrations.mode", modeText)]` so every subsequent log line inherits the scope.

### 6.4 `/health` exposure

In `core/service/Service/Transport/Web.hs` (where `HealthCheckConfig` is consumed), the JSON body of the `/health` response gains two fields: `integrations.mode` and `integrations.fakes` (as an array). The `Selection` value is plumbed into the health handler via the existing `Application.healthCheckConfig` field — an optional `Selection` field is added to the in-memory health state (not to the CLI-exposed config record, to preserve backward compatibility of `Web.HealthCheckConfig`).

### 6.5 Where pre-bound closures are stored

The dispatcher uses a `TypeRep`-keyed registry (§2.10, §11). The registry is built once, at step 1.5.5 (a new sub-step added alongside the selection resolve):

```haskell
     -- 1.5.5. Build the per-instance dispatcher registry.
     let dispatchRegistry =
           Integrations.buildDispatchRegistry selection app.registeredIntegrations
```

`app.registeredIntegrations` is an existential-bearing `Array` (already implemented for other factories — see `DeferredOutboundLifecycleReg` at `core/service/Service/Application.hs:330-336`) into which every `OutboundIntegration` TH invocation pushes its associated `Integration request` witnesses. The registry is then passed down the same way `app.dispatcherConfig` is, reaching:

- `Integrations.startIntegrationSubscriber` (for outbound emit calls inside `createTypedOutboundRunner`)
- `Integrations.startInboundWorkers` (for inbound integrations to pick `drive` vs `runReal`)

### 6.6 INLINE pragma on the shim

`Service.Integration.ShimEmit.emit` carries `{-# INLINE emit #-}` (pattern seen at `core/service/Service/OutboundIntegration/TH.hs:158` for `handleEventImpl` and at `core/core/Task.hs:69` for `Task.yield`). This inlines the `DispatchRegistry.lookup` + closure invocation at every call site, preserving the 50k req/s hot path claimed in ADR §3.

### 6.7 Rationale for `TypeRep`-keyed map vs. alternatives

- TH-generated monomorphic wrapper per instance — rejected. It would require every `Integration` instance site to run a companion TH macro beyond `outboundIntegration`; developer ergonomics suffer and we fork from the ADR-0049 style where TH is only at the handler level.
- `reflection`-style trick — rejected. Adds a library (`reflection`) that isn't currently in `common_cfg.build-depends` and produces opaque stack traces.
- `TypeRep`-keyed `HashMap` — **chosen**. `DispatchRegistry` is populated once (`O(1)` writes at startup per integration), looked up once per emit (`O(log n)` via `Map.lookup`, or `O(1)` with `HashMap` — we use `Map` from nhcore because it is already a Core re-export at `core/core/Core.hs:27`, and `log n` at n ≈ 50 integrations is ~6 comparisons, well within the emit budget). This mirrors the existing dispatcher's use of `ConcurrentMap StreamId EntityWorker` (`core/service/Service/Integration/Dispatcher.hs:252`): the dispatcher already routes by runtime key, so layering a `TypeRep` key on top is a local extension of an accepted pattern.

The lookup inlines into a static direct-call at each `OutboundIntegration` emission point because GHC can see the `TypeRep` at the call site (it is a constant derived from `Typeable` — the instance dict is compile-time). In a `--integrations=real` build, the inlined lookup result is a call to `runReal`; no case-over-`Selection` survives at emit time because the closure was built with `Selection` already case-analysed.

---

## 7. Integration with OutboundIntegration (ADR-0049)

The `OutboundIntegration` typeclass (`core/service/Service/OutboundIntegration/Core.hs:34-46`) is unchanged in shape. The `outboundIntegration` TH macro (`core/service/Service/OutboundIntegration/TH.hs`) is unchanged in shape. The *body* of `handleEventImpl` now produces `Integration.Outbound` whose individual `Action`s are built via the new shim rather than the old `ToAction` instance path.

### 7.1 What changes in `createTypedOutboundRunner`

`core/service/Service/Application/Integrations.hs:78-102` constructs an `OutboundRunner`. Today its `processEvent` closure calls `Integration.runAction ctx action`. The rewrite:

- `Integration.runAction` is **removed** from the public API (see §3.12).
- The call site at `core/service/Service/Application/Integrations.hs:96` is changed to invoke the dispatcher shim for each emitted `Integration request` value produced by `handleEventImpl`. Mechanically, `Integration.Outbound` becomes a thin wrapper around `Array (SomeDispatchCall)` where `SomeDispatchCall` is an existential that holds the `request` value and its `Integration request + Typeable request` dictionaries. Dispatch is then:

```haskell
processOne :: DispatchRegistry -> SomeDispatchCall -> Task Text (Maybe CommandPayload)
processOne registry (SomeDispatchCall request) = do
  response <- Integration.emit registry request
    |> Task.mapError toText
  Task.yield Nothing  -- follow-up CommandPayload emission, if any, remains Nick's code
```

- `Integration.getActions` stays (test-only now) — renamed `Integration.getCalls` to reflect the new type, moved under `Test.Integration.Simulate` to keep its reach test-only.

### 7.2 Where the existing type-erased OutboundRunner plumbing is preserved

`OutboundRunner.processEvent` (`core/service/Service/Integration/Types.hs:47-49`) keeps its existing signature `ActionContext -> EventStore Json.Value -> Event Json.Value -> Task Text (Array CommandPayload)`. The reason: downstream code in the dispatcher (`core/service/Service/Integration/Dispatcher.hs:793-800`) calls `runner.processEvent ctx eventStore event` and iterates the `Array CommandPayload` result to invoke `dispatchCommand`. Changing this signature would ripple through lifecycle runners; instead, the *body* of `processEvent` is what changes (it now goes through `Integration.emit`), not its shape.

### 7.3 File-line-precise migration targets

- `core/service/Service/Application/Integrations.hs:96` — replace `Integration.runAction ctx action` call.
- `core/service/Service/OutboundIntegration/TH.hs:154-158` — generated `handleEventImpl` body: no TH change needed; the user's `handleEvent` body itself is what migrates (but `handleEvent` returns `Integration.Outbound`, and the encoding of `Outbound` is what shifted — so the TH stays, the data type changes).
- `core/service/Integration.hs:215-225` — `Action`, `Outbound`, `ToAction`, `outbound`, `batch`, `none` deleted. `Outbound` replaced by a new shape (`newtype Outbound = Outbound (Array SomeDispatchCall)`).

---

## 8. Integration with Inbound Workers (ADR-0008)

The existing `Integration.Inbound` value-level worker (`core/service/Integration.hs:354`) is replaced by `InboundIntegration` typeclass instances.

### 8.1 Affected call sites (all verified via Grep)

- `core/service/Service/Application.hs:345` — `(config -> Integration.Inbound)` in `DeferredInboundReg`. Changed to `(config -> SomeInboundIntegration)` where `SomeInboundIntegration` is an existential wrapping `forall inbound. (InboundIntegration inbound, Typeable inbound) => Proxy inbound`.
- `core/service/Service/Application.hs:360,401,899,908` — `inboundIntegrations :: Array Integration.Inbound` becomes `Array SomeInboundIntegration`. All array-manipulation sites updated.
- `core/service/Service/Application.hs:1246` — `Integrations.startInboundWorkers app.inboundIntegrations combinedCommandEndpoints` gains a `Selection` argument and a `DispatchRegistry` argument.
- `core/service/Service/Application.hs:1413,1417,1450,1460,1464` — `withInbound` / `withInboundFromConfig` helpers rewritten to accept a type-indexed `InboundIntegration inbound` witness.
- `core/service/Service/Application/Integrations.hs:191-210` — `withInbound` helper, `(Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound)` signatures updated to the new existential.
- `core/service/Service/Application/Integrations.hs:263-295` — `startInboundWorkers` rewritten to consult `Selection`: if the inbound is named in a `Hybrid` fake list, or `Selection == Fake`, it calls `(runFake @inbound).drive emitCommand` instead of `runReal @inbound emitCommand`.
- `core/service/Integration/Timer.hs:102-109` — `periodicCartCreator :: Integration.Inbound` example and the `Integration.Timer.periodic` helper are rewritten to return an `InboundIntegration` instance. The module file keeps its current name; what it re-exports changes.
- `core/service/Integration/Command.hs` — same treatment (class instance replacing `Integration.inbound`).
- `core/service/Integration/Exit.hs` — same.

### 8.2 Application-side registration helper

`withInbound` migrates to:

```haskell
withInbound ::
  forall inbound.
  (InboundIntegration inbound, Typeable inbound) =>
  Proxy inbound ->
  Application ->
  Application
```

The `Proxy` carries the type witness. The user's registration site becomes:

```haskell
Application.new
  |> ...
  |> Application.withInbound (Proxy @StripeWebhook)
```

mirroring the `Application.withQuery @CartSummary` style already in use (`core/service/Service/Application.hs:747`).

---

## 9. Fixture Path Resolution — Concrete Algorithm

Path resolution lives in `Service.Integration.Fixture.resolvePath`. The algorithm uses Haskell-shaped pseudocode (signatures, real nhcore operator choices). Each failure branch maps to a `Task Text <result>` error.

```haskell
resolvePath ::
  Path ->       -- projectRoot (absolute, resolved at Application.run time)
  Text ->       -- integrationName (from NameOf)
  FixtureKey -> -- hex hash
  Task Text Path
resolvePath projectRoot integrationName key = do
  -- Step 1: validate the integration name.
  validatedName <- case validateIntegrationName integrationName of
    Ok name -> Task.yield name
    Err err -> Task.throw [fmt|Fixture path rejected: #{err}|]

  -- Step 2: assemble relative path using System.FilePath's </> (imported
  -- qualified as GhcFilePath in §4.7).
  let rootStr = Path.toLinkedList projectRoot
  let nameStr = Text.toLinkedList validatedName
  let keyStr  = Text.toLinkedList (FixtureKey.toText key)
  let assembled =
        rootStr
          GhcFilePath.</> "tests"
          GhcFilePath.</> "fixtures"
          GhcFilePath.</> nameStr
          GhcFilePath.</> (keyStr ++ ".json")

  -- Step 3: resolve symlinks and ".." via makeAbsolute.
  absolute <- GhcDirectory.makeAbsolute assembled
    |> Task.fromFailableIO @GhcException.IOError
    |> Task.mapError (\err -> [fmt|Fixture path resolve failed: #{show err}|])

  -- Step 4: canonicalise the root once, too, so the prefix check is apples to apples.
  rootAbs <- GhcDirectory.makeAbsolute rootStr
    |> Task.fromFailableIO @GhcException.IOError
    |> Task.mapError (\err -> [fmt|Project root resolve failed: #{show err}|])

  let fixturesRootAbs =
        rootAbs
          GhcFilePath.</> "tests"
          GhcFilePath.</> "fixtures"

  -- Step 5: prefix check against the canonicalised project-fixtures root.
  if not (fixturesRootAbs `GhcFilePath.isPrefixOf` absolute)
    then Task.throw
      [fmt|Fixture path escapes project root: #{absolute} is not a descendant of #{fixturesRootAbs}|]
    else Task.yield ()

  -- Step 6: further follow the actual filesystem symlink target and re-check.
  --   GhcDirectory.pathIsSymbolicLink plus GhcDirectory.getSymbolicLinkTarget.
  --   If it resolves outside fixturesRootAbs, treat as a miss on read path,
  --   error on write path. The read/write caller decides (see §3.7).
  isSymlink <- GhcDirectory.pathIsSymbolicLink absolute
    |> Task.fromFailableIO @GhcException.IOError
    |> Task.mapError (\err -> [fmt|Symlink check failed: #{show err}|])
  case isSymlink of
    True -> do
      target <- GhcDirectory.getSymbolicLinkTarget absolute
        |> Task.fromFailableIO @GhcException.IOError
        |> Task.mapError (\err -> [fmt|Symlink target read failed: #{show err}|])
      targetAbs <- GhcDirectory.makeAbsolute target
        |> Task.fromFailableIO @GhcException.IOError
        |> Task.mapError (\err -> [fmt|Symlink target resolve failed: #{show err}|])
      if not (fixturesRootAbs `GhcFilePath.isPrefixOf` targetAbs)
        then Task.throw
          [fmt|Fixture symlink target escapes project root: #{targetAbs}|]
        else Task.yield ()
    False -> Task.yield ()

  Task.yield (Path.fromLinkedList absolute |> Maybe.getOrDie)

validateIntegrationName :: Text -> Result Text Text
validateIntegrationName name =
  if Text.matchesRegex "^[A-Za-z0-9_]+$" name
    then Ok name
    else Err [fmt|Integration name '#{name}' must match [A-Za-z0-9_]+|]
```

Regex match uses the existing `Text.matchesRegex` helper (if absent, the implementer adds it as a one-liner over `regex-tdfa` — which is already pulled in by §5.3 for the test library). The read-path (`lookupResponse`) catches `FixtureOutsideRoot`, `FixtureNameInvalid`, and absent files as a *miss*, returning `Result FixtureMiss (Response request)`; the caller (fake-mode dispatch) treats a miss as "fall through to `runFake`". The write-path (`Test.Integration.Fixture.record`) treats any of these as hard `IntegrationError.ValidationFailure` and aborts.

---

## 10. Canonical JSON — Implementation Outline

### 10.1 Module

`Service.Integration.Canonical` (`core/service/Service/Integration/Canonical.hs`).

### 10.2 Public function signatures

```haskell
encode :: (Aeson.ToEncoding value) => value -> ByteString
hash   :: (Aeson.ToEncoding value) => value -> Text
version :: Int
```

### 10.3 `ToEncoding`

`ToEncoding` is the aeson class. It is **not currently re-exported** by the nhcore `Json` module (`core/json/Json.hs:1-23`). We do **not** modify `Json.hs` to re-export it — that would widen the Json-user API for a narrow use-case. Instead, the canonical encoder imports `Data.Aeson qualified as Aeson` directly and accepts `Aeson.ToEncoding value` as its constraint. Users who want to be hashable need `deriving Aeson.ToEncoding` (or, more commonly, a manual `toEncoding = genericToEncoding defaultOptions` on their request type). The integration authoring guide calls this out.

### 10.4 Implementation plan

The RFC 8785 encoder is **not** built on top of `encode`'s existing JSON output — aeson's default `encode` does not sort keys UTF-16-wise and does not format numbers per ECMA-262 §7.1.12.1. We walk the `Aeson.Encoding` structure ourselves. Helpers:

```haskell
-- Internal (no re-export).
encodeValue :: Aeson.Value -> Builder.Builder
encodeObject :: [(Text, Aeson.Value)] -> Builder.Builder
encodeArray :: Array Aeson.Value -> Builder.Builder
encodeNumber :: Scientific.Scientific -> Builder.Builder
encodeString :: Text -> Builder.Builder

sortObjectKeys :: [(Text, Aeson.Value)] -> [(Text, Aeson.Value)]
-- UTF-16 code-unit comparison: convert each key to its UTF-16 representation
-- (via Data.Text.Encoding.encodeUtf16LE from text 2.x — pinned by existing
-- nhcore deps) and sort on that bytestring.

formatNumber :: Scientific.Scientific -> Builder.Builder
-- Implements the ECMA-262 §7.1.12.1 ToString(number) rule by routing through
-- `scientific`'s `formatScientific` with `Fixed`/`Generic` selection based on
-- exponent magnitude. The "shortest round-trippable" requirement is satisfied
-- by scientific's built-in minimal-digit formatter (floatToShortest internal).

escapeString :: Text -> Builder.Builder
-- RFC 8785 §3.2.2.2: only escape " \ and control chars U+0000..U+001F; all
-- other Unicode code points pass through as UTF-8 bytes.
```

- UTF-16 key sorting: uses `Data.Text.Encoding.encodeUtf16LE` (from `text`, already a direct dep — `core/nhcore.cabal:80`) to produce the comparison key, then sorts the `[(Text, Value)]` list with the resulting `ByteString` as the Ord.
- Number serialisation: uses `scientific`'s `Data.Scientific.formatScientific FPExponent Nothing`-based path plus a small dispatch (based on magnitude) to match ECMA-262's switch between fixed and exponential forms. `scientific` is already a pinned dep (`>= 0.3 && < 0.4`, `core/nhcore.cabal:94`). If the fine-tuning of `formatScientific` proves insufficient for edge cases flagged by conformance vectors (e.g. `-0`, `5e-324`), we fall back to a hand-rolled ECMA-262 implementation in `Canonical.Number` — this module is listed as an other-module fallback in §5.1 and only implemented if conformance vectors fail.
- `NaN` / `±Infinity`: detected at `encodeNumber` entry via `Scientific.isNaN` / `isInfinite` equivalents (or pattern match on the exponent / coefficient), throwing `Canonical.InvalidNumber "NaN"` etc., which the caller lifts to `IntegrationError.ValidationFailure`.

### 10.5 Conformance test layout

RFC 8785 ships official test vectors at <https://github.com/cyberphone/json-canonicalization/tree/master/testdata>. We mirror the `input/` and `output/` trees into `core/service/test/Canonical/Vectors/` and add a golden-file harness in `core/test/Service/Integration/CanonicalSpec.hs` that walks the tree and, for each pair, asserts `encode (parsedInput) == expectedOutput`. The harness is test-only (`test-suite nhcore-test-core`). Layout:

```
core/service/test/Canonical/Vectors/
  input/
    arrays.json
    french.json
    structures.json
    unicode.json
    values.json
    weird.json
  output/
    arrays.json
    french.json
    structures.json
    unicode.json
    values.json
    weird.json
  README.md              -- source, license note
```

The vectors files are committed as-is (no preprocessing). CI fails if any input/output pair drifts.

---

## 11. Dispatcher Storage & Lookup

**Pick:** `TypeRep`-keyed `Map` populated once at startup (already motivated in §6.7).

### 11.1 Storage shape

```haskell
newtype DispatchRegistry = DispatchRegistry (Map TypeRep Unknown.Unknown)
```

- `Unknown.Unknown` (`core/core/Unknown.hs` — the existential wrapper already used in `OptionsParser` at `core/options-parser/Command.hs:48`) holds the closure in a type-erased slot.
- `TypeRep` comes from `Data.Typeable.typeRep (Proxy @request)` at registration time.

### 11.2 Population

The `Application.run` wire-up step (`Integrations.buildDispatchRegistry selection integrations`) iterates `integrations` and, for each, calls `DispatchRegistry.register` with a closure that already has `selection` baked in:

```haskell
registerFor ::
  forall request.
  (Integration request, Typeable request, GHC.KnownSymbol (NameOf request)) =>
  Selection ->
  Path ->  -- project root
  Proxy request ->
  DispatchRegistry ->
  DispatchRegistry
registerFor selection projectRoot _proxy reg =
  DispatchRegistry.register @request (closureFor selection projectRoot) reg
  where
    closureFor :: Selection -> Path -> request -> Task IntegrationError (Response request)
    closureFor Real _ req = runReal req
    closureFor Fake root req = fakeOrFixture @request root req
    closureFor (Hybrid names) root req =
      let name = Text.fromLinkedList (GHC.symbolVal (Proxy @(NameOf request)))
      in if FakeNameRegistry.member name (FakeNameRegistry.fromArray names)
           then fakeOrFixture @request root req
           else runReal req
```

### 11.3 Lookup

At emit time, `Integration.emit registry request` calls `DispatchRegistry.lookup @request registry`. If `Just closure`, the closure is invoked directly (and inlines thanks to `{-# INLINE emit #-}`). If `Nothing`, emit throws `PermanentFailure "Integration not registered"` (this is a programmer error — the `Application.withOutbound` site didn't register the type, so startup should have already thrown; we preserve the error path for completeness).

### 11.4 Precedent

This mirrors:

- `core/service/Service/Integration/Dispatcher.hs:252-262` — `ConcurrentMap StreamId EntityWorker`, a type-erased-by-StreamId worker map populated at spawn and looked up at dispatch.
- `core/service/Integration.hs:116-147` — `ImmutableProviderRegistry` (`newtype` around `Map Text ValidatedOAuth2ProviderConfig`) — a read-only type-safe registry built once at startup.

Both patterns set a precedent for "build at startup, look up at runtime". We extend the same idiom to `TypeRep` keys.

---

## 12. Test Layout

Every test file listed here gets its `Spec` exported and registered in the owning test-suite's `Main.hs`.

| Test file | Spec name | Role |
|-----------|-----------|------|
| `core/test/Service/Integration/AdapterSpec.hs` | `spec` | Class-shape tests: zero-state default `runFake` yields a QuickCheck-generated response; override path; class does not impose `Arbitrary` on instances that override `runFake`. |
| `core/test/Service/Integration/CanonicalSpec.hs` | `spec` | Drives every RFC 8785 conformance vector in `core/service/test/Canonical/Vectors/`. Fails on any byte drift. Additionally tests `NaN`/`Infinity` rejection and `Canonical.version == 1`. |
| `core/test/Service/Integration/FixtureSpec.hs` | `spec` | Fixture file lookup: hit, miss fall-through to `runFake`, hash-keyed filename correctness. Path-traversal attempt tests: `../etc/passwd`, symlink-out, Unicode homoglyphs — all produce `FixtureOutsideRoot`. |
| `core/test/Service/Integration/SelectionSpec.hs` | `spec` | CLI + env-var gate: `--integrations=fake` without env var ⇒ startup error with exact ADR §3 text; with env var ⇒ `Selection.Fake`; `hybrid --fake=Sendgrid --fake=Stripe` ⇒ `Hybrid ["Sendgrid","Stripe"]`; `--fake=Evil/../` ⇒ validation error. |
| `core/test/Service/Integration/DispatchRegistrySpec.hs` | `spec` | Register, lookup, re-register (same TypeRep overwrites), lookup-miss (`Nothing` path), round-trip preservation of the closure's `Response` type. |
| `core/test/Service/Integration/InboundSpec.hs` | `spec` | `controllableHandle` bounded-channel behaviour: default capacity 1024; backpressure when full (a producer's `inject` blocks until consumer `drive` drains); `simulate` cross-thread correctness; explicit `controllableHandleWith 10` honours override. |
| `core/test/Service/Integration/RecorderSpec.hs` | `spec` | `NEOHASKELL_RECORD_FIXTURES` unset ⇒ `record` is a no-op. Set ⇒ writes to `tests/fixtures/local/`. Entropy scan rejects a response containing `"sk_live_..."`, `"Bearer abc..."`, `"eyJhbGci..."`, 32-hex tokens, base64 blobs > 48 chars, high-Shannon strings. `AllowEntropyPath` hatches a field out of the scan. `promote` requires `NEOHASKELL_PROMOTE_FIXTURES=1`. |
| `core/test/Service/Integration/PropertySpec.hs` | `spec` | `fakeProperty` smoke test — generates 100 requests against a toy integration, asserts an invariant, fails with the `request + hash` output when the invariant breaks. |
| `core/test/Service/Integration/ContractSpec.hs` | `spec` | `contractTests @Toy` with a planted-drift response type (fake returns old shape; response type updated) ⇒ `FromJSON` fails ⇒ test fails. `NEOHASKELL_CONTRACT_SANDBOX=1` branch is exercised against a stub `runReal`. |

ADR-mapped cross-reference:

| ADR need | Covered by |
|---|---|
| Contract tests | `ContractSpec`, `Test.Integration.Contract` |
| Property tests | `PropertySpec`, `Test.Integration.Property` |
| Fixture-lookup tests | `FixtureSpec` |
| Canonical-JSON conformance | `CanonicalSpec` + vectors tree |
| Path-traversal safety | `FixtureSpec` |
| Env-var gate behaviour | `SelectionSpec` |
| Recorder entropy-scan | `RecorderSpec`, `Test.Integration.EntropyScan` |
| Bounded-channel inbound | `InboundSpec` |
| Dispatcher lookup | `DispatchRegistrySpec` |
| Class shape & default | `AdapterSpec` |

---

## 13. Migration Impact

Every existing file touched by the migration, with a one-line reason. Discovered via the Grep queries shown in the task preamble.

### 13.1 `ToAction` instance migrations

| File | Reason |
|------|--------|
| `integrations/Integration/Http/Internal.hs:51-56` | `instance ToAction (Request command)` rewritten as `instance Integration (Request command)` with `type Response = Response` and `runReal = executeRequest`. |
| `integrations/Integration/Oura/Internal.hs:148,154,160,166,172,178,184` | Seven `ToAction` instances (DailySleep, DailyActivity, DailyReadiness, HeartRate, Workout, Session, PersonalInfo) rewritten as `Integration` instances. Responses derive `Arbitrary` via `Generic`. |
| `integrations/Integration/Oura/SyncAll.hs:69-70` | `ToAction (SyncAll command)` rewritten; `runReal` keeps the 17-fetch concurrency pattern. |
| `integrations/Integration/Audio/Transcribe/Internal.hs:47-49` | `ToAction (Request command)` rewritten. |
| `integrations/Integration/Pdf/ExtractText/Internal.hs:47-49` | `ToAction (Request command)` rewritten. |
| `integrations/Integration/Ocr/Ai/Internal.hs:47-49` | `ToAction (Request command)` rewritten. |
| `integrations/Integration/Agent/Internal.hs:49-51` | `ToAction (Request command)` rewritten. |

### 13.2 Inbound-worker migrations

| File | Reason |
|------|--------|
| `core/service/Integration/Timer.hs:102-109` | `Integration.Inbound` / `Integration.inbound` replaced with an `InboundIntegration` instance. |
| `core/service/Integration/Command.hs` | Same treatment. |
| `core/service/Integration/Exit.hs` | Same treatment. |

### 13.3 Testbed and example-app migrations

| File | Reason |
|------|--------|
| `testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs` | Uses `Integration.outbound Sendgrid.Email {...}`; now emits via the new dispatcher. `handleEvent` body unchanged in intent; the shape of `Integration.Outbound` changes. |
| `testbed/src/Testbed/Cart/Integrations.hs` | Same. |
| `testbed/src/Testbed/Examples/PdfExtraction.hs` | Uses `Integration.outbound Pdf.ExtractText.Request {...}`. |
| `testbed/src/Testbed/Examples/OpenRouterIntegration.hs` | Uses `Integration.outbound` against OpenRouter request. |
| `testbed/src/Testbed/Examples/HttpIntegration.hs` | Uses `Integration.outbound` against `Http.Request`. |
| `testbed/src/Testbed/Examples/AiTranscription.hs` | Uses `Integration.outbound`. |

### 13.4 Application surface migrations

| File | Reason |
|------|--------|
| `core/service/Service/Application.hs:345` | `DeferredInboundReg` field `(config -> Integration.Inbound)` rewritten. |
| `core/service/Service/Application.hs:360` | `inboundIntegrations :: Array Integration.Inbound` rewritten. |
| `core/service/Service/Application.hs:401` | Initialiser field value. |
| `core/service/Service/Application.hs:762-764` | Hook for selection parsing (see §6.1). |
| `core/service/Service/Application.hs:899,908` | `resolvedInboundIntegrations` update. |
| `core/service/Service/Application.hs:1246` | `Integrations.startInboundWorkers` call. |
| `core/service/Service/Application.hs:1413,1417` | Evaluated-inbound path. |
| `core/service/Service/Application.hs:1450,1460,1464` | Deferred-inbound path. |
| `core/service/Service/Application/Integrations.hs:191-210` | `withInbound` / `withOutboundLifecycle` helper signature updates. |
| `core/service/Service/Application/Integrations.hs:78-102` | `createTypedOutboundRunner` now emits via shim. |
| `core/service/Service/Application/Integrations.hs:263-295` | `startInboundWorkers` consults `Selection`. |

### 13.5 Documentation migrations (not Haskell code, but on the critical path)

| File | Reason |
|------|--------|
| `website/src/content/docs/reference/integrations.mdx` | Updated — `ToAction`/`Inbound` sections replaced with `Integration` / `InboundIntegration` class docs and examples. |
| `website/src/content/docs/tutorial/05-transfers.mdx` | Example using `Integration.outbound` rewritten. |
| `website/src/content/docs/guides/integrating-existing-systems.mdx` | Example using `ToAction` rewritten. |
| `website/src/content/docs/coming-from/python.mdx` | Snippet update. |

### 13.6 Existing tests requiring rename/port

| File | Reason |
|------|--------|
| `core/test/IntegrationSpec.hs` | Tests of `Integration.runAction`, `Integration.batch`, `Integration.outbound` need porting to the new typeclass-based shape. `Integration.runAction` and `Integration.batch` survive as test-only re-exports under `Test.Integration.Simulate` for migration ease. |
| `core/test/OutboundIntegrationSpec.hs` | TH macro tests unaffected, but expected-type assertions that mention `Integration.Outbound`'s internals must update. |
| `core/test/Integration/DispatcherSpec.hs` | Uses `Integration.CommandPayload` outputs; unchanged by this ADR (the lower-level `OutboundRunner` shape is preserved per §7). |
| `core/test/Integration/RuntimeSpec.hs` | Migrates `Integration.inbound` config calls to the new class. |
| `core/test/Integration/TimerSpec.hs` | Timer worker tests consume the new `InboundIntegration` shape of `Integration/Timer.hs`. |
| `core/test/Integration/CommandSpec.hs`, `ContextSpec.hs`, `ExitSpec.hs`, `RegistrySpec.hs` | Scoped rewrites mirroring the `Integration/*.hs` source changes. |

### 13.7 Cabal manifests

| File | Reason |
|------|--------|
| `core/nhcore.cabal` | New `library testing` stanza, 11 new exposed modules, `regex-tdfa` added to `common_cfg`, `nhcore:testing` added to four test-suite `build-depends`. |
| `integrations/nhintegrations.cabal` | No structural change; will rebuild against the new `nhcore` surface. |

---

## 14. Open Questions

None. All design decisions are resolved in sections 1–13.
