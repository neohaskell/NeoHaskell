# Changelog

## 0.10.2 — 2026-09-22

### Fixed

- **Framework**: Concurrent commands that update the same entity now use the entity revision they actually read. If another command writes first, the stale command retries with fresh state instead of appending an outdated event. New-entity commands also prevent duplicate stream creation, while unconditional appends keep their existing behavior.

Existing applications do not need code or data migrations. To verify the fix locally, run `./dev test 'Retry Logic' nhcore-test-service` and confirm the command-handler retry examples pass for the in-memory and PostgreSQL backends. ([change](https://github.com/neohaskell/NeoHaskell/commit/cab923c0b098fdb82ece539380719c0757ec9057))

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.10.1...neo-v0.10.2)

## 0.10.1 — 2026-09-22

### Added

- **Framework**: Declare events, commands, entities, queries, and outbound integrations with
`deriveEvent`, `deriveCommand`, `deriveEntity`, `deriveQuery`, and
`deriveOutboundIntegration`, all available through `import Core`.

The new `deriveEntity ''CartEntity ''CartEvent` helper generates routine entity
instances and connects your `initialState`, `update`, and `getEventEntityId`
functions. Existing custom instances remain supported. The previous marker
names still work, so existing applications need no changes. ([change](https://github.com/neohaskell/NeoHaskell/commit/4ba756552d2b5ba20c734ba9ae29fa31dab3a11e))

### Fixed

- **CLI**: Applications can use the framework's declaration helpers without adding language
pragmas to their source files. Neo now enables the required deriving strategy in
the project settings it generates for application, library, and test components.

After updating Neo, run `neo build` and `neo test` from your application directory.
Neo refreshes the generated settings; no manual Cabal configuration is needed. ([change](https://github.com/neohaskell/NeoHaskell/commit/4ba756552d2b5ba20c734ba9ae29fa31dab3a11e))

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/neo-v0.10.0...neo-v0.10.1)

## 0.10.0 — 2026-09-15

### Breaking changes

- **CLI**: The Rust Neo CLI joins NeoHaskell, its integrations and starter under one
platform version. It creates a complete starter project, builds and tests it,
and includes the browser IDE. Projects made with the old 0.9 Haskell CLI need
a configuration and launcher update; `neo shell` is replaced by `nix develop`.
The Rust CLI and IDE already shipped separately as `neo-v0.1.x`: users with
that project layout can keep it while applying any framework API migrations. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))
- **Framework**: Apps upgrading from 0.9.0 need to update event-store calls and several core
helpers. Text and array appends now take the value being extended last;
mutable variables run inside `Task`; durable-channel writes accept batches;
and JSON constraint names and subprocess error types have changed. The guide
below preserves existing ordering, error handling and event identities. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))

### Added

- **Framework**: Build an app around its recorded history: define commands such as “add an item,”
  events such as “item added,” and queries that build the lists users see. The new
  service framework connects these parts, checks their types, and supplies the
  following capabilities. Existing 0.9 apps can adopt them after applying the
  core/event-store migration guide.

  - **Commands and saved state.** Declare entities, events, commands and queries
    with the framework's derivation helpers instead of writing their supporting
    instances by hand. Command execution checks access, rejects invalid decisions
    and retries conflicting updates. Entity fetching rebuilds state from events;
    optional snapshots avoid replaying the same old history on every fetch.
    `Uuid.generateV5` and `Decider.generateDeterministicUuid` let an app derive a
    repeatable ID from a namespace and a natural key, such as an external order ID.
    Verify accepted, rejected and repeated commands with the app's domain tests.

  - **Storage that survives restarts.** Use the PostgreSQL event store for shared
    database storage, or SimpleEventStore's optional JSONL files for local
    persistence. PostgreSQL subscriptions catch up after a listener reconnect;
    fixes prevent missing events during that catch-up and release subscription
    connections on unsubscribe. PostgreSQL settings include explicit pool sizes
    and TLS modes. Configure storage and restart a test instance to check that its
    history recovers and PostgreSQL subscriptions resume. Database event listeners
    require a direct,
    session-preserving connection.

  - **Lists, filters and pages.** Queries maintain read models: saved views of
    events used to answer requests. They can use memory or PostgreSQL storage.
    HTTP responses include `items`, `total`, `hasMore` and `effectiveLimit`, so a
    screen can request one page at a time. The default page contains at most 100
    items, with a maximum of 1,000 per request. Results and counts respect query
    authorization. NeoQL supplies field access and equality filtering. Test
    first/next/empty pages and access restrictions in your app before connecting
    the UI.

  - **Sign-in and access rules.** JWT authentication validates signed access
    tokens; OAuth2 support connects external accounts, stores tokens and refreshes
    them. Typed configuration loads fields from flags, environment variables and
    `.env` files, reports missing or invalid values, and supports secret fields.
    Commands can declare public or authenticated access. Multi-tenant commands
    and the `tenantOnly` query filter let apps separate each customer's data when
    explicitly configured. Test both allowed and denied requests, including users
    from different tenants; adding sign-in alone does not select your access rules.

  - **HTTP, command-line and agent access.** Expose service commands and queries
    through HTTP, an app-specific CLI, or MCP (the protocol coding assistants use
    to call tools). Internal commands can serve integrations without becoming
    public endpoints. Generated JSON schemas and OpenAPI descriptions reflect
    command inputs and paginated query responses. WebTransport supports CORS for
    browser clients. Select the transports your app needs and exercise their
    requests, including invalid inputs and denied access.

  - **Uploads and downloads.** Apps can validate uploaded file sizes and types,
    retain an opaque file reference, confirm or delete files, and control download
    access. Local blob storage keeps bytes; the optional PostgreSQL file-state
    store keeps lifecycle information. Deduplication reuses identical content for
    the same owner and restores a missing blob on re-upload. Configure both stores
    for your deployment, then test upload, authorized download, rejection and
    deletion with real files.

  - **Startup and diagnostics.** HTTP can start accepting health checks while
    queries rebuild from history. With readiness enabled, `/ready` stays at 503
    until the read models are usable; route traffic after it reaches 200.
    PostgreSQL query checkpoints support resuming rebuilds. Structured logging
    adds command and event context, and line-buffered output reaches container
    logs promptly. Follow the [deployment guide](https://github.com/neohaskell/NeoHaskell/blob/d2201cc555d55cc558d751ff67d04569a42eda18/website/src/content/docs/guides/deployment.mdx)
    for separate startup, liveness and readiness probes; test a cold restart,
    rather than only an already-warm process.

  - **Everyday data and concurrency helpers.** Fixed-point `Decimal` provides
    money-oriented arithmetic and formatting; `Crypto` signs and verifies
    HMAC-SHA256 messages with an opaque key type. Arrays gain trimming, searching,
    chunking and zipping helpers; Bytes gains binary conversions, slicing and
    Base64 support. Map and Maybe gain convenience operations, and `Text.escapeHtml`
    escapes text for HTML. Concurrent maps, streams and atomic variables support
    shared work; asynchronous tasks gain cancellation and racing. `Parser` and
    `Layout` help parse and format text, with comment/function syntax primitives.
    These primitives do not constitute a complete new language compiler. Adopt
    the helpers where needed and retain boundary tests for your data formats,
    rounding, authentication and cancellation behavior. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))
- **IDE**: View and edit your app's event model in the browser with `neo ide`. The IDE is
  bundled into the Neo CLI, opens locally at `127.0.0.1:2323` by default, and
  saves the diagram in `event-model.json`. It can show commands, events, queries
  and their connections, validate references, and synchronize information from
  source files. This brings the IDE already available in the independent Neo
  CLI series into the coordinated NeoHaskell release.

  From the project root, run `neo ide` and open its printed URL. Review autosaved
  changes before committing the model. You can keep working in your usual source
  editor; adopting the diagram is optional.

  Use `neo validate` to check an existing model without opening the browser;
  expect exit code 0 for a valid file. `neo inspect domains` and
  `neo inspect wiring` show what Neo can discover from your source. When you
  intend to update the model from code, run `neo inspect sync` and review the
  resulting diff: that command writes to `event-model.json`. A missing or invalid
  model makes validation fail; it is not treated as a successful check. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))
- **Integrations**: Connect app events to external services through the new `nhintegrations`
  package. Configure an integration's request and the commands to run on success
  or failure; the framework handles dispatch. These integrations are additions
  since 0.9.0, so there are no old integration imports to rename in a 0.9 app.

  - **HTTP and email:** call external HTTP endpoints with `Integration.Http`,
    send transactional email with `Integration.Brevo`, or use Azure Communication
    Services through `Integration.Acs`.
  - **AI conversations and tools:** use `Integration.OpenRouter` or
    `Integration.AzureAI` for chat completions. `Integration.Agent` lets a model
    call typed application commands as tools. Register only the commands and
    permissions you intend the agent to have.
  - **Documents and recordings:** `Integration.Pdf.ExtractText` extracts text
    from digital PDFs with `pdftotext`; `Integration.Ocr.Ai` extracts text from
    documents/images through a selected multimodal model. `Integration.Audio.Transcribe`
    supports WAV, MP3, M4A and OGG recordings. AI extraction/transcription sends
    content to the selected provider through OpenRouter. Configure the MIME type
    and model for the actual input; large-file chunking and streaming transcription
    are not supplied. For long transcription requests, make the integration
    dispatcher's timeout at least as long as the request timeout.
  - **Oura:** connect an Oura account with OAuth2 and access typed sleep, activity,
    readiness and other Oura API v2 data through `Integration.Oura`.

  Add `nhintegrations` to your app's dependencies, configure the chosen service's
  credentials and required local tools, and register its outbound handler. Use
  an isolated test account or a fake integration to check the success and failure
  commands before sending real emails or changing external data. Also test token
  refresh where applicable. Provider accounts and usage are separate from a
  Codex subscription. The release process itself requires no paid AI API. ([change](https://github.com/neohaskell/NeoHaskell/commit/1fcad50a1e945c24cfc20008219642bfeae8e173))

### Migration from 0.9.0

#### CLI

### Choose the path matching your project

If your app already has `neo.json` with `neo-version`, a `launcher/Launcher.hs`
and the Rust CLI layout, keep that layout. Updating the CLI does not
automatically change an existing app's framework pin. Compare its
`neo-version` with `neohaskell.source_revision` in this release's
`neo-compatibility.json`; updating that pin is a framework upgrade and needs
the relevant API migrations and tests.

For projects created with the **0.9 Haskell CLI**, use the following steps.
They preserve the app's behavior; a simple command-line program need not become
a web service.

### Prepare a reference project

Download the native binary for your OS/CPU from the
[0.10.0 release assets](https://github.com/neohaskell/NeoHaskell/releases/tag/neo-v0.10.0),
along with `SHA256SUMS` and `neo-compatibility.json`:

| Computer | Binary |
| --- | --- |
| Mac, Apple Silicon | `neo-aarch64-apple-darwin` |
| Mac, Intel | `neo-x86_64-apple-darwin` |
| Linux, ARM64 | `neo-aarch64-unknown-linux-gnu` |
| Linux, x86-64 | `neo-x86_64-unknown-linux-gnu` |

Run `shasum -a 256 <downloaded-file>` (or `sha256sum` on Linux) and compare its
hash with that file's entry in `SHA256SUMS`. Make the verified binary executable
with `chmod +x <downloaded-file>`, then install it as `neo` in a directory on
your PATH. Check `neo --version` reports 0.10.0 before migrating. With Nix and Git available, create a
reference in a separate temporary directory, using your app's existing package
name so generated names match:

```sh
neo --ci new my-app
```

Use `neo --ci new my-app --library` for a library; it has no executable launcher.
Replace `my-app` with your own package name. The starter is embedded in the
binary; resolving or building dependencies can still require network access.
Keep your original project and Git history, and bring over the needed scaffold
files on an upgrade branch rather than replacing the app with the example.

### Update configuration and build files

Keep `name`, `version`, `description`, `author` and `license`
from your existing `neo.json`; review each dependency as described below. The application's `version` remains its own
version. Add `neo-version` using the full 40-character revision generated by
the new CLI, and set `type` to `executable` or `library` as appropriate.

Dependency values now have meaning: the 0.9 generator used the package names
and ignored the values. For a Hackage dependency, use a `hackage:` key and a
supported npm-style version range, for example:

```json
"dependencies": {
  "hackage:aeson": ">=2.0.0 <3.0.0"
}
```

Choose the range your app needs; this example is not a required Aeson upgrade.
A bare key uses the NeoPackages registry. Git/local dependencies use values
such as `github:owner/repo#ref`, `git:https://host/repo.git#ref`, or
`file:../local-lib`. Preserve the selected source and intended versions; old
arbitrary values or Cabal-specific constraints are not automatically valid.

The old optional `overrideNeohaskell` setting is no longer read. Record why
it was used before removing it. For a normal upstream dependency, replace it
with the release's `neo-version`; if it pointed to a local checkout or custom
fork, resolve that build arrangement with the owner before running the
regeneration commands below. `neo-version` alone cannot represent a different
repository or local path; an unresolved override means migration is incomplete.

`neo build`, `neo test` and `neo run` regenerate `<name>.cabal`, `cabal.project`
and `flake.nix`. Put supported dependency settings in `neo.json`. Save any
custom flags, source directories, native dependencies or Nix overrides from
the old files and check whether the generated configuration represents them.
Unsupported custom build settings need a deliberate solution before migration
can be considered complete.

### Preserve the executable entrypoint and tests

The old scaffold called `<PascalName>.run :: Task Text ()` through
`.launcher/Main.hs`. The new executable entrypoint is
`launcher/Launcher.hs`, containing `module Main`; the generated Cabal file uses `launcher/` for the
executable and `src/` for library modules. The old `.launcher/` path is no longer
used. For a simple program, the new launcher can
continue to run the same task:

```haskell
module Main where

import Core
import MyApp qualified
import Task qualified

main :: IO ()
main = MyApp.run |> Task.runOrPanic
```

Replace `MyApp` with your existing module name. For an app adopting the service
framework, follow the new starter's `src/App.hs` / `app :: Application` and
`Application.run` setup instead. Preserve the app's actual startup behavior,
arguments and side effects. Do not copy the starter's service `src/App.hs` over
a simple app's existing source.

Keep application source in `src/`, merge existing tests into the new `tests/`
setup, and inspect `.envrc`, `.env.example`, Docker configuration and editor
settings before adopting them. Do not replace real configuration with starter
example values or commit secrets. The scaffold installs a Git lock hook;
`neo lock` helps protect existing event definitions from accidental edits.
Review `.locked-files` and the hook before recording intentional migrations.
Do not lock incompatible code and then skip the check just to make a build pass.

Replace scripts or editor tasks calling `neo shell` with `nix develop` in the
project directory. Optional `neo skills setup --dry-run --tool codex` previews
available agent guidance; installing skills is separate from migrating code.

### Verify

From the upgraded application root, run:

```sh
neo --ci build
neo --ci test
```

Expect a successful build and passing app tests. For an executable, also run
`neo --ci run` with development configuration and check its actual behavior:
for example, its expected terminal output or a real HTTP request. Stop the app
once the smoke check is complete. Libraries need their API tests, not `neo run`.

Confirm `neo.json`'s `neo-version`, the regenerated Nix source revision and
Cabal source tags agree with the intended release. Inspect the diff for lost
source, dependencies, test assertions and custom configuration. If the app has
an event model, `neo validate` should pass; do not treat a missing model as a
reason to create unrelated product features.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate CLI from NeoHaskell 0.9.0 to 0.10.0.
Upgrade this application's NeoHaskell tooling from the 0.9 Haskell CLI to the
0.10 Rust Neo CLI, preserving its behavior, Git history, sources, tests and
configuration. Work on a branch and inspect the current layout first.

If neo.json already has neo-version and the app already uses the Rust CLI
launcher/build layout (including users of neo-v0.1.x), keep that layout. A CLI
update does not change the existing framework pin. Compare it with the release's
neo-compatibility.json neohaskell.source_revision; apply framework migrations
and tests before intentionally changing it.

For an old 0.9 layout, create a reference project in a separate temporary
parent directory using `neo --ci new <existing-package-name>`; add --library
only for a library. Keep the original repository. Preserve neo.json name,
application version, description, author and license; add the
reference's immutable 40-character neo-version and the correct type. Review
all dependencies: 0.9 ignored their values. Current Hackage entries use
"hackage:name": "<npm-style-range>"; bare keys select NeoPackages. Git/local
values use git:<url>#ref, github:<owner>/<repo>#ref, or file:<path>. Convert old
values to preserve intended sources/versions, not just their spelling. The old
overrideNeohaskell field is no longer read: document any custom fork/local
checkout and get its build mapping resolved before removing it or running regeneration. Never substitute
main for the release's compatible revision.

Preserve src/ and tests. The executable now uses launcher/Launcher.hs with
module Main. It can retain the old module's `run :: Task Text ()` by calling
`MyApp.run |> Task.runOrPanic` (replace MyApp with the actual module). Do not
convert a simple program to a service unnecessarily. Apps adopting services can
follow the starter's App.app/Application.run pattern. Libraries have no launcher.
The CLI regenerates <name>.cabal, cabal.project and flake.nix; put supported
dependency configuration in neo.json and report any custom build settings that
cannot be represented. Review .envrc, environment examples, Docker/editor files,
.locked-files and Git hooks. Preserve actual environment values securely and
never commit secrets. Replace neo shell in scripts with nix develop.

Run neo --ci build and neo --ci test. For executables, run neo --ci run with
safe development configuration, verify the app's actual output or endpoint and
stop it afterward. For libraries, run API tests without neo run. Verify emitted
Nix/Cabal revisions agree with neo-version and the release compatibility file.
If event-model.json already exists, run neo validate. Report changed files,
commands/results, unresolved build overrides and behavior requiring the owner's
judgment. Do not delete tests, bypass lock checks to hide failures, deploy,
change production data or publish a release.
```

</details>

#### Framework

Apply only the sections matching APIs your app uses. Commit or back up the app
before starting, and test against a copy of any persistent data.

### Preserve the order of appended text and arrays

These calls still compile but produce the opposite order if left unchanged:

| 0.9 code | Replacement with the same result |
| --- | --- |
| `Text.append "butter" "fly"` | `"butter" \|> Text.append "fly"` |
| `Array.append left right` | `left \|> Array.append right` |

The first row must still produce `"butterfly"`. The second must put every
item from `left` before every item from `right`. For direct calls, reverse the
arguments instead: `Text.append suffix subject` or `Array.append right left`.
Generic `Collection.append` on arrays uses the same reversed argument order.
The `Appendable` `(++)` operator is unchanged. Review partial applications and
folds individually: a blanket swap can reverse
an entire accumulated list. Include empty inputs and distinct values in tests.

### Move mutable variables into Task

`Var.new`, `Var.get` and `Var.set` now return `Task error value`, replacing
`IO value`. Their names and argument order stay the same. Inside an existing
Task, remove an outer `Task.fromIO` around a Var operation:

```haskell
-- Before
counter <- Task.fromIO (Var.new 0)
-- After
counter <- Var.new 0
```

If the caller is an `IO` entrypoint, move the variable operations into a Task
and run it once at that boundary. A task that cannot fail can use
`Task.runNoErrors`; a fallible task needs the application's existing error
handling. Do not wrap a Task in `Task.fromIO`, discard its errors, or change
shared-variable synchronization while migrating.

### Write one-item batches to durable channels

For existing code that writes one value, keep that behavior by wrapping it
with `Array.wrap`:

| 0.9 code | Replacement |
| --- | --- |
| `DurableChannel.write value channel` | `DurableChannel.write (Array.wrap value) channel` |
| `DurableChannel.checkAndWrite predicate value channel` | `DurableChannel.checkAndWrite predicate (Array.wrap value) channel` |
| `DurableChannel.writeWithIndex makeValue channel` | `DurableChannel.writeWithIndex (\index -> Array.wrap (makeValue index)) channel` |

Reads still return individual values. `writeWithIndex` returns the batch's
starting index. Test write/read order, a predicate that rejects a write, and
index values; adopting larger batches is a separate behavior change.

### Rename JSON constraints and handle subprocess failures

Replace `Json.Decodable value` with `Json.FromJSON value` and
`Json.Encodable value` with `Json.ToJSON value`, including explicit import
lists. Existing JSON encoding/decoding functions retain their names. Keep
fixtures that check the shape of saved or exchanged JSON.

`Subprocess.open` and `Subprocess.openInherit` now return
`Task Subprocess.Error Completion`. If your caller uses `Task Text`, translate
the error at the call boundary, for example:

```haskell
completion <- Subprocess.open executable arguments directory |> Task.mapError toText
```

`executable` is Text, `arguments` is an Array of Text, and `directory` is a
Path. For an app-specific error type, map to its appropriate constructor. Check the
completion's `exitCode` as before: a process exiting unsuccessfully is different
from being unable to launch it (`ProcessError`). Test both a missing executable and a nonzero exit.

### Update direct EventStore users

The new event store saves typed event content, supports batches and streams
reads without loading the whole history at once. If you called the 0.9
`Service.EventStore` directly, update both writes and readers:

| 0.9 API | Current API / required decision |
| --- | --- |
| `EntityId` wrapping a UUID | `EntityName` wrapping text; choose a stable name for each entity kind |
| `StreamId` wrapping a UUID | Text-backed `StreamId`; `toStreamId oldUuid` preserves a UUID as text |
| `InsertionEvent` and `appendToStream` | `InsertionPayload`, `Insertion` and `store.insert` |
| `EventStore` | `EventStore eventType`; `InMemory.new` returns `EventStore Json.Value` inside its Task |
| Plain `Event` with top-level `id`, positions | `Event eventType`; read `metadata.eventId`, `metadata.localPosition`, `metadata.globalPosition` (positions are optional) |
| `Array Event` from reads | `Stream (ReadStreamMessage eventType)` or `Stream (ReadAllMessage eventType)` |
| Subscription callback `Task Error Unit` | Callback `Task Text Unit`; map app errors explicitly |
| `ConcurrencyConflict ...` | `InsertionError ConsistencyCheckFailed` |
| `StreamNotFound streamId` | `StreamNotFound entityName streamId` |
| `Limit Int`, `StreamPosition Int` | `Limit Int64`, `StreamPosition Int64` |

Keep existing UUIDs and the mapping from entity kinds to streams. `EntityName`
is a kind such as `"Cart"`, not a freshly generated ID per write. Import `Service.Event.StreamId` qualified as `StreamId` for
`StreamId.toStreamId`, or use the `toStreamId` method exported by `Service.Event`.
The old event
record contained identifiers and positions but no domain payload: decide what
`eventType` your app will store instead of inventing missing historical content.
`EventStore.castEventStore` adapts a raw `EventStore Json.Value` to a type with
`Json.FromJSON` and `Json.ToJSON` instances.

The old `localPosition` was the expected **next** position. For the first
insertion use `StreamCreation`; when the expected next position is `n > 0`,
use `InsertAfter (StreamPosition (n - 1))`. Also set the first insertion's
`metadata.localPosition` to `Just (StreamPosition n)`. Keep subsequent batch
positions consecutive. Derive `n` from the stream you actually read. Simple checks the
next position; PostgreSQL rejects conflicting stored positions but does not
validate an arbitrary position ahead of the stream tail. Do not invent or
skip positions. Test the stale-write case on the backend your app uses.

For example, in a Task using qualified `Service.Event` as `Event` and
`Service.Event.EventMetadata` as `EventMetadata`, the first write becomes:

```haskell
metadata <- EventMetadata.new
let insertion = Event.Insertion
      { id = existingEventId
      , event = yourEvent
      , metadata = metadata
          { eventId = existingEventId
          , localPosition = Just (Event.StreamPosition 0)
          }
      }
let payload = Event.InsertionPayload
      { streamId = migratedStreamId
      , entityName = Event.EntityName "Cart"
      , insertionType = Event.StreamCreation
      , insertions = Array.wrap insertion
      }
success <- store.insert payload
```

Here `existingEventId` is the UUID you previously supplied, `yourEvent` is the
app's chosen payload, and `migratedStreamId` preserves the old stream identity.
`insert` returns `InsertionSuccess`, with `localPosition` and
`globalPosition`, rather than an Event. Persistence uses `metadata.eventId`;
setting `Insertion.id` alone does not preserve the stored event UUID. The convenience `payloadFromEvents`
generates new IDs and defaults to `AnyStreamState`, which does **not** preserve
an old expected-position check; do not substitute it blindly.

Preserve handling of the outer read `Task Error` before consuming its stream.
For finite reads, `Stream.toArray` collects messages, then
`EventStore.collectStreamEvents` / `collectAllEvents` extracts events. Inspect
`ToxicStreamEvent` / `ToxicAllEvent` (content that could not decode) and
`StreamTerminated` / `Terminated` before filtering: simply extracting events
can hide read failures. Large histories should use `Stream.consume` with
explicit handling of event, checkpoint and failure messages. Update any
custom EventStore implementation for these protocols and its new `close` and
`truncateStream` operations; call `close` during shutdown.

`InMemory.new` remains available and non-persistent. To retain events across
restarts, configure `SimpleEventStore` with a valid `Path` and
`persistent = True`, or configure the PostgreSQL store. This is a storage
choice, not an automatic conversion: the old in-memory store has no disk data
to recover after shutdown. If your app supplied its own storage, export and
validate its identifiers, positions and content before changing formats; this
release provides no general converter for custom stores.

### Verify

After also applying the CLI guide if needed, run `neo build` and `neo test`
from the app root (or its existing Cabal build/test commands if it does not use
Neo). Expect a successful build and passing app tests. Add focused checks for
append order, Var read/write, channel ordering and rejected writes, JSON
round-trips, and subprocess failures where those APIs are used.

If your app uses the `Command` options-parser facade, also test its flags,
environment inputs, help output and invalid-input errors: its parser backend
changed, even though the public facade names remain.

For EventStore users, test first insertion, the next insertion, two competing
writers, a rejected stale write, forward/backward reads, subscription catch-up
and shutdown. Confirm IDs and ordering remain unchanged, and that errors cannot
silently drop events. For persistent stores, restart against a **copy** of data
and verify the same history can be read. Report unresolved storage mappings
before deploying the upgraded app.

<details>
<summary>Copy this prompt to your coding agent</summary>

```text
Migrate Framework from NeoHaskell 0.9.0 to 0.10.0.
Upgrade this application's NeoHaskell 0.9 core/EventStore usage to 0.10. Preserve
behavior, tests, user data, UUIDs and event ordering. Start on a branch; identify
which changes apply before editing.

1. Rewrite old Text.append first second and Array.append first second to
   first |> Text.append second and first |> Array.append second. Review partial
   applications, Collection.append on Array, and folds for order. Appendable
   (++) is unchanged. Test distinct
   values and empty inputs; "butter" followed by "fly" must remain "butterfly".
2. Var.new/get/set now return Task instead of IO. Remove Task.fromIO wrappers
   around them in Task code. At an IO boundary, run a composed Task using the
   app's error policy (Task.runNoErrors only when the error type is Never).
3. DurableChannel.write/checkAndWrite accept Array values. Preserve single
   writes with Array.wrap. writeWithIndex callbacks must return a singleton
   Array; reads remain individual values. Test order, indexes and rejection.
4. Rename Json.Decodable/Encodable constraints and imports to FromJSON/ToJSON.
   Preserve JSON round-trip fixtures. Subprocess.open/openInherit now return
   Task Subprocess.Error Completion: map errors to the caller's type and retain
   exitCode checks. Test missing executables and nonzero exits.
5. For direct EventStore users, replace EntityId with a stable EntityName per
   entity kind and convert old UUID StreamIds with StreamId.toStreamId. Use typed
   Event eventType, InsertionPayload/Insertion and store.insert. Preserve the
   old insertion UUID in Insertion.id and metadata.eventId. Choose actual domain
   content with the owner when old records lack it; never invent historical data.
   Old expected-next position 0 uses StreamCreation; n > 0 uses InsertAfter
   (StreamPosition (n - 1)). Set metadata.localPosition = Just (StreamPosition n)
   and consecutive positions for batches. Derive n from actual stream state,
   never an invented future position; PostgreSQL rejects position collisions
   but does not validate gaps ahead of the tail. Positions and Limit use Int64.
   payloadFromEvents generates new IDs and defaults to AnyStreamState; using it
   unchanged would drop the old concurrency check. insert returns positions.
   Persistence uses metadata.eventId, not Insertion.id alone.
   Read Event IDs/positions through metadata; positions are Maybe values.
   Reads now return Stream messages. Preserve outer Task error handling,
   then handle toxic/terminated messages before
   collectStreamEvents/collectAllEvents; Stream.toArray is only for bounded
   reads. Subscription callbacks return Task Text Unit. Replace old concurrency
   errors with InsertionError ConsistencyCheckFailed and add EntityName to
   StreamNotFound matches. Implement close/truncateStream for custom stores.
6. Run the application's build/tests (neo build and neo test after any CLI
   migration). Exercise stale/conflicting writes, ordering, subscription catch-up
   and shutdown. Use a copy of persistent data for restart verification. The old
   InMemory backend has no persistent data; custom storage has no automatic
   migration. Do not delete data, weaken tests or silently skip decoding errors.

Report changed files, tests and exact outcomes, unresolved API or data mappings,
and any assumptions needing the app owner's decision. Do not deploy or publish.
```

</details>

[Compare changes](https://github.com/neohaskell/NeoHaskell/compare/0.9.0...neo-v0.10.0)

## Legacy history

# Changelog

Entries are **generated from contract-delta specs** (`docs/changes/*.md`) by
`./dev changelog` — do not hand-write them; regenerate instead. A change is
**breaking** iff its spec's `diff signatures` delta removes or changes a
signature line; a breaking entry carries a mandatory migration note (from the
spec's `## User impact`). CI gate: `changelog --check` in `.github/workflows/checks.yml`.

**Release promotion:** at release time, rename the `## [Unreleased]` heading to
`## [X.Y.Z] — YYYY-MM-DD` and add a fresh empty `## [Unreleased]` above it; a
breaking entry in the section forces a major/minor bump per semver. (No release
has been cut yet — everything accrues under Unreleased until the first tag.)

## Legacy development notes

### 008-harden-pipeline-evidence-and-regression-smoke — Change 008: Harden pipeline evidence and regression smoke

No public application API breaks. Contributors get earlier, deterministic failures instead of false-green specs, red tests, stale localization data, or incomparable timing claims. The new benchmark is local/nightly rather than PR-blocking because shared-runner wall-clock noise is not deterministic.

API delta:

- `+ Test.Service.Command.Core: AddItemToCartAfterItemCount :: Uuid -> Uuid -> Int -> AddItemToCartAfterItemCount`
- `+ Test.Service.Command.Core: data AddItemToCartAfterItemCount`
- `+ Test.Service.EventStore.Regression: assertBehavior :: Text -> Bool -> Task Text Unit`
- `+ Test.Service.EventStore.Regression: awaitInsertions :: Int -> InsertBarrier -> Task error Unit`
- `+ Test.Service.EventStore.Regression: barrierBeforeInsert :: InsertBarrier -> EventStore event -> EventStore event`
- `+ Test.Service.EventStore.Regression: data InsertBarrier`
- `+ Test.Service.EventStore.Regression: failFirstWithConsistencyConflict :: EventStore event -> Task error (EventStore event)`
- `+ Test.Service.EventStore.Regression: knownBad :: Text -> Text -> Task error Bool`
- `+ Test.Service.EventStore.Regression: newInsertBarrier :: Task error InsertBarrier`
- `+ Test.Service.EventStore.Regression: recordFetchedRevisions :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (Maybe StreamPosition)))`
- `+ Test.Service.EventStore.Regression: recordFetches :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (EntityFetchResult state)))`
- `+ Test.Service.EventStore.Regression: recordInsertions :: EventStore event -> Task error (EventStore event, Task error (Array (InsertionPayload event)))`
- `+ Test.Service.EventStore.Regression: releaseInsertions :: Int -> InsertBarrier -> Task error Unit`
- `+ Test.Service.EventStore.Regression: requireInsertionType :: InsertionType -> EventStore event -> EventStore event`
- `+ Test.Service.EventStore.Regression: seedStream :: EventStore event -> EntityName -> StreamId -> Array event -> Task Text Unit`

### 007-make-cold-start-health-constant-time — Change 007: Make cold-start health constant-time without losing replayed events

**Runtime:** container liveness no longer scales with event-store size. Operators
can probe `/health` for process liveness and `/ready` for traffic readiness
without widening a grace period as the store grows. Replay remains ordered and
complete when live events overlap startup; fixing the bind delay must not trade
a loud crash-loop for silent projection loss.

**Performance:** rebuild changes from one full-log pass per query plus
per-operation pool construction to one paged, entity-filtered pass over a
reused Postgres pool. Entity snapshots prevent repeated full-stream fetches.
Progress is visible through the ADR-0059 field names rather than a silent
multi-minute gap.

**Public Haskell surface:** additive only. `Service.Query.Registry` exposes
`registeredEntityNames :: QueryRegistry -> Array EntityName`; existing callers
need no migration.

**CI:** `.github/workflows/test.yml` exports `POSTGRES_AVAILABLE=true` for the
Postgres-backed suites so the concurrency and pool regressions execute on every
substantive PR.

**Deployment documentation:** restore the deployment guide and lead with a
`startupProbe`/readiness configuration, including explicit
`periodSeconds × failureThreshold` arithmetic and the distinction between
`/health` and `/ready`.

**Deliberately deferred:** production checkpoint-store wiring and query-state
migration remain tracked by #854/#855/#666. SIGTERM cancellation remains #662;
outbound-integration recovery is #856; the missing `X-Query-Status` contract is
#664; Neon scale-to-zero support is #857. None is required to make port binding
constant-time and in-process replay/live overlap gap-free.

API delta:

- `+ Service.Query.Registry: registeredEntityNames :: QueryRegistry -> Array EntityName`

### 006-deterministic-uuid-v5 — Change 006: Add deterministic UUID v5 generation to `Uuid` and the `Decision` monad

**Not breaking.** Three added signatures, no removals, no behavior change to any
existing function. `Decision`'s constructor set is unchanged, so `runDecision`
and the command executor are untouched — existing commands cannot regress.

**New capability for users.** Natural-key entity identity becomes expressible in
the framework:

```haskell
getEntityId :: RegisterProject -> Maybe Uuid
getEntityId command =
  Uuid.generateV5 projectNamespace command.repoPath |> Just
```

so the same normalized path always routes to the same stream, and `decide` sees
`Just entity` on a repeat and can `Decider.reject "already registered"`. Inside
`decide`, other derived ids come from
`Decider.generateDeterministicUuid namespace key`, which reads alongside
`Decider.generateUuid` without a `pure`/`Task.yield` lift (which the dialect
hook bans at the call site anyway).

**Security constraint, carried in the haddock of both functions.** A v5 UUID is
**not** a secret — anyone who knows the namespace and the name reproduces it
exactly, and names drawn from a small space can be brute-forced from a known
namespace. It must never be used for capability tokens, session ids,
password-reset components, or anything else that is unguessable by design;
`Uuid.generate` (random v4) stays the correct choice there. The division of
labour is now explicit: *random for secrets and fresh identity, deterministic for
derived identity.*

**No default namespace.** The issue floated a `uuidFromText :: Text -> Decision Uuid`
convenience with a built-in namespace; it is deliberately not shipped. A
framework-wide default namespace is a global collision domain, and since the
whole premise of the primitive is that the same input *is* the same stream, such
a collision is a data-integrity bug across unrelated aggregates. Callers build a
namespace with `Uuid.fromText`. RFC 4122's predefined namespaces (DNS/URL/OID/X500)
are likewise not re-exported yet — rule of three, and no consumer has asked.

**Testbed effect.** A new demo command `Testbed.Cart.Commands.RegisterCartByKey`
(registered in `Testbed.Cart.Service`) exercises the feature end-to-end, per the
`new-command-machinery` extension point's rule that framework write-side features
get a demo command plus hurl coverage. It is additive: no existing testbed
command, query, or hurl file changes, and no existing expectation is touched.

**Relationship to the abandoned `feat/uuid-v5-decision` branch.** The issue asks
that the pre-existing branch be merged. It is not merged as-is: alongside the
~130 relevant lines it carries ~5000 lines of unrelated tooling
(`.atomicorch/**`, `docs/designs/**`), duplicate ADR trees (`docs/adr/` and
`docs/decisions/`), and an ADR numbered **0055**, which `main` has since assigned
to *declarative integrations with fakes*. Its `Uuid.generateV5` also reaches for
`Data.Text.Encoding` and `Data.ByteString` directly rather than the dialect's
`Text.toBytes`. This change re-lands the wanted API on a clean branch through the
spec gate, with the ADR renumbered to 0073 and the criteria above; the old branch
should be closed rather than merged.

API delta:

- `+ Uuid: generateV5 :: Uuid -> Text -> Uuid`
- `+ Bytes: unpack :: Bytes -> [Word8]`
- `+ Decider: generateDeterministicUuid :: Uuid -> Text -> Decision Uuid`

### 005-thread-query-name-through-query-object-store — Change 005: Thread the real query name through the QueryObjectStore so multiple queries per entity stop colliding  **[BREAKING]**

**Breaking (source-level, in-repo callers updated in this PR).** Three exported
signatures gain a query-name input:

- `createDefinitionWithStore` — its store-factory argument becomes
  `Text -> Task Text (QueryObjectStore query)`. Migration: a factory `f` that
  ignored the name becomes `\_ -> f`; a factory that needs it receives the
  query's `NameOf` as `Text`.
- `QueryObjectStore.Core.createQueryObjectStore` / `Postgres.newFromConfig` —
  each takes the query name as a trailing `Text`. Migration: pass the query
  name (`newFromConfig cfg "my-query"`).

The two in-repo callers — `Application.withQuery` (config-backed path) and
`Definition.createDefinition` (in-memory convenience) — are updated here.
External services that call `createDefinitionWithStore` with a **custom** store
factory must adapt the factory shape; the changelog carries the migration note
(generated from the removed signature lines).

**Runtime behavior.** Before: any app with ≥2 queries over one entity, running
on the Postgres QueryObjectStore, corrupts state across queries and fails their
rebuilds — `withQueryObjectStore` with a Postgres config is unusable for
real multi-query services. After: each query's rows are keyed by its own name,
so distinct queries over the same entity persist independently. In-memory stores
are unaffected — each `InMemory.new` already allocates an independent map, so its
`createQueryObjectStore` ignores the name.

**Testbed:** no acceptance-test change — the collision needs a Postgres backend
with ≥2 queries per entity, which the default testbed app does not wire; covered
at the integration level. The existing `PostgresSpec` single-store tests keep
running (their `mkStore` helper supplies a fixed default query name), so the
refactor's regression surface stays green.

**Wiring coverage (review follow-up):** the store-level isolation criteria
(C1/C2) prove that *manually named* Postgres stores do not collide, but not that
the application actually threads the right name into the store. C4 closes that
gap: it drives `createDefinitionWithStore` with a spy store factory and asserts
the factory is handed `NameOf query`, not the `"__trait__"` sentinel — a fast
`unit` test (no Postgres) that exercises the exact wiring hop the fix adds.

**Checkpoint coexistence (implementation follow-up, C5):** before the fix the
trait's per-instance rows lived under `"__trait__"` while the checkpoint marker
(`Subscriber.rebuildFrom` via `CheckpointStore`, keyed by the reserved nil UUID)
lived under the real `query_name` — two different partitions. Threading the real
name unifies them under one `query_name`, isolated only by `instance_uuid` (real
vs nil). `get`/`atomicUpdate` are unaffected (they use real instance UUIDs), and
`resumeFromCheckpoint`/`deleteStaleHash` stay correct (they filter by
`query_hash`, which trait rows leave empty; `deleteStaleHash` only fires before a
full replay-from-0 that rebuilds those rows). The one place that needed a guard
is `getAll`, which now excludes the nil-UUID marker so a checkpointed query's
`GET /queries/{name}` never surfaces the marker's placeholder state — proven by
C5.

API delta:

- `- Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Task Text (QueryObjectStore query)`
- `+ Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Text -> Task Text (QueryObjectStore query)`
- `- Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError (QueryObjectStore query)`
- `+ Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Text -> Task QueryObjectStoreError (QueryObjectStore query)`
- `- Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => Task Text (QueryObjectStore query) -> QueryDefinition`
- `+ Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => (Text -> Task Text (QueryObjectStore query)) -> QueryDefinition`

### 004-crypto-hmac-sign-verify — Change 004: Add Crypto module with HMAC-SHA256 signWith/verifyWith

None breaking. New public module `Crypto` and new `Bytes.getRandom`
primitive (secure random bytes, mirroring `Int.getRandom`); no existing
signatures change. The `Bytes` newtype now lives in the hidden internal
module `Bytes.Internal` purely to break an import cycle; it is not
importable by applications. The public `Bytes` API is unchanged (`Bytes
(..)` is re-exported as before). `Auth.OAuth2.StateToken` keeps its private
`HmacKey` for now — migrating it onto `Crypto.HmacKey` is a possible
follow-up refactor, deliberately out of scope here. Signature wire format
is lowercase hex (the common webhook header convention, e.g. GitHub/Stripe
style); `verifyWith` is case-insensitive on input.

API delta:

- `+ Crypto: data HmacKey`
- `+ Crypto: hmacKeyFromText :: Text -> Result Text HmacKey`
- `+ Crypto: hmacKeyFromBytes :: Bytes -> Result Text HmacKey`
- `+ Crypto: generateHmacKey :: Task err HmacKey`
- `+ Crypto: signWith :: HmacKey -> Bytes -> Text`
- `+ Crypto: verifyWith :: HmacKey -> Text -> Bytes -> Bool`
- `+ Bytes: getRandom :: Int -> Task w Bytes`

### 003-maintainer-codemap-regeneration — Change 003: Maintainer-triggered codemap regeneration onto a contributor PR

Not breaking. No public signature or wire-format change. New capability for
maintainers only: a manually-dispatched workflow on `main`. Contributors see
their PR branch receive one `chore: regenerate codemap` fast-forward commit after
Nick approves the protected environment; a no-op (codemap already current) leaves
the branch untouched and the run succeeds. Every unsupported or unsafe condition
(maintainer edits disabled, org-owned fork, metadata race, symlink under
`codemap/`, out-of-allowlist manifest/diff, non-fast-forward) fails with an
actionable Actions summary and mutates nothing — **no fallback PR is ever
created**. Testbed: no acceptance-test change — this is CI/tooling with no
HTTP-observable behavior. One-time maintainer setup is **mandatory and
load-bearing**: the `codemap-publish` Environment with **required reviewer Nick**
AND **deployment branches = `main` only**, plus the `CODEMAP_PUBLISH_TOKEN` secret
— a maintainer classic `public_repo` PAT (broad public-repo blast radius
documented; dedicated bot identity recommended; expiry ≤90d; revoke-on-exposure)
— documented in ADR-0070 and the workflow header. Without any of these the
workflow fails closed at `publish`.

### 002-task-control-flow-dialect-rules — Change 002: Enforce Task control-flow dialect — `|> discard`, `Task.when`, `Task.unless`

Not breaking. No public signature or wire-format change — the migrated `if …
pass` blocks and their `Task.when`/`Task.unless` replacements are behaviourally
identical (`Task.when c a` runs `a` iff `c`, `Task.unless c a` runs `a` iff not
`c`, each otherwise doing nothing — exactly like the `if`/`pass` forms). New Task
code is nudged toward the dialect idioms at edit
time (rule 1) and at `./dev lint`/CI (rules 2–3). Existing non-dialect parser
and `Q`-monad code is deliberately preserved via added-lines grandfathering and
a scoped ignore. Testbed: no acceptance-test change — this is a source-dialect
and tooling change with no HTTP-observable behaviour.

### 001-fileupload-dedup-blob-existence-check — Change 001: Verify the blob still exists before returning a dedup match on file upload

Not breaking. No signature or wire-format change; `UploadResponse` still omits
`blobKey` from JSON. Behavior only changes on the failure path: an upload that
previously returned a reference to a missing blob (poisoning the content hash
forever) now re-stores the content and returns a valid reference. The re-stored
bytes are the caller's own uploaded content, matched by the same owner-scoped
content hash, so there is no cross-owner exposure. Testbed: no acceptance-test
change — blob loss cannot be induced over HTTP; covered at the integration
level.

Side effect of making the reproduction executable: `ContentDedupSpec` is listed
in the cabal `other-modules` but was never registered in
`core/test-service/Main.hs`, so its dedup coverage compiled but never ran. This
change registers it, so the regression tests **and** the existing dedup suite
now execute.
