# Neo CLI — User Flow Chapters

## Phases

### Pre-MVP: Build & Run

Goal: A developer can create, build, and run a NeoHaskell project without touching Nix or Cabal directly.

| Command | Chapter | Status |
|---------|---------|--------|
| `neo init` | 1 | ✅ Designed |
| `neo build` | 2 | ✅ Designed |
| `neo run` | 3 | ✅ Designed |

**Pre-MVP constraints:**
- No `.nh` transpiler yet — projects use `.hs` files with NeoHaskell conventions (nhcore library, pipe style, qualified imports)
- `neo init` generates a standard Cabal project with a Nix flake
- `neo build` and `neo run` wrap `nix develop` + `cabal build/run` — Jess never types either
- The `neo` binary is a Haskell CLI using nhcore primitives. The Rust `installer/` only handles Nix installation.
- Generated file extensions are `.hs` everywhere the full vision says `.nh`

### Post-MVP

Remaining commands, in rough priority order. Designed as pre-MVP stabilizes.

| Command | Chapter |
|---------|---------|
| `neo test` | 4 |
| `neo add` | 5 |
| `neo remove` | 6 |
| `neo repl` | 7 |
| `neo clean` | 8 |
| `neo info` | 9 |
| `neo update` | 10 |

---

## CLI Infrastructure (applies to all commands)

This section defines the behavior of `neo` itself — before any command runs. It covers the bare invocation, global flags, error handling, output conventions, and project detection. Every command in this spec inherits these rules.

---

### `neo` (bare invocation)

Running `neo` with no arguments shows a friendly welcome message. Not an error. Not a usage dump. A welcome.

```
$ neo

  Welcome to NeoHaskell.

  neo init my-app    Create a new project
  neo run            Start your app
  neo build          Build for production

  Run neo --help for all commands.
```

Design notes:
- Tone: a friendly colleague, not a man page
- Shows the three commands Jess will use 95% of the time — not the full list
- No "Error:" prefix, no exit code 1 — this is not a failure state
- `neo --help` is the escape hatch for the full list
- Output goes to **stderr**

---

### `neo --help`

Every command supports `--help`. Help text is minimal and example-driven.

**Global help:**

```
$ neo --help

  Usage: neo <command>

  Commands:
    init     Create a new project
    build    Build for production
    run      Start the app
    test     Run the tests
    add      Add a dependency
    remove   Remove a dependency
    repl     Start an interactive session
    clean    Remove build artifacts
    info     Show project and toolchain info
    update   Update neo

  Run neo <command> --help for details.
```

**Per-command help:**

```
$ neo init --help

  Create a new NeoHaskell project.

  Usage: neo init <project-name>

  Example:
    neo init my-app
    cd my-app
    neo run
```

```
$ neo build --help

  Build the project.

  Usage: neo build [--verbose]

  Flags:
    --verbose    Show full compiler output

  Example:
    neo build
    neo build --verbose
```

```
$ neo run --help

  Build and run the project.

  Usage: neo run [--verbose]

  Flags:
    --verbose    Show full compiler output during build

  Example:
    neo run
    neo run --verbose
```

Design notes:
- Help text shows one example, not every edge case
- Descriptions are present tense, imperative mood
- No manpage-style option tables — this is the full help
- `--help` output goes to **stderr**

---

### `neo --version`

```
$ neo --version
neo 0.1.0
```

One line. No platform info, no build hash, no toolchain details. Just the version.

Toolchain details (GHC version, OS, architecture) live in `neo info` — a separate command for when you actually need them.

Output goes to **stdout** — `neo --version` is machine-readable.

---

### Unknown command handling

When Jess types a command that doesn't exist, `neo` tells her what went wrong and — if it looks like a typo — suggests what she probably meant.

**Exact typo (edit distance ≤ 2):**

```
$ neo initt

  Unknown command: initt. Did you mean: init?

  Run neo --help to see all commands.
```

**No close match:**

```
$ neo deploy

  Unknown command: deploy.

  Run neo --help to see all commands.
```

**Multiple close matches (rare):**

```
$ neo re

  Unknown command: re. Did you mean one of: repl, remove?

  Run neo --help to see all commands.
```

Design notes:
- Typo correction uses edit distance (Levenshtein ≤ 2). Prefix matches also qualify.
- Suggestion is offered, never forced — Jess decides
- Exit code 1
- Output goes to **stderr**

---

### Unknown flag handling

When a flag isn't recognized, `neo` names the flag and tells Jess what the command actually accepts.

```
$ neo init --no-git

  Unknown flag: --no-git
  neo init takes no flags.

  Run neo init --help for usage.
```

```
$ neo build --watch

  Unknown flag: --watch
  neo build accepts: --verbose

  Run neo build --help for usage.
```

```
$ neo run --release

  Unknown flag: --release
  neo run accepts: --verbose

  Run neo run --help for usage.
```

Design notes:
- Always names the offending flag — Jess knows exactly what she typed
- If the command takes no flags, says so directly
- If the command takes flags, lists them — short enough to fit in one line
- Exit code 1
- Output goes to **stderr**

---

### Unimplemented command handling

Post-MVP commands (`test`, `add`, `remove`, `repl`, `clean`, `info`, `update`) are recognized but not yet implemented. They show a friendly "coming soon" message — not an error.

```
$ neo test

  Tests are coming in a future release.
  Follow along: https://neohaskell.org
```

```
$ neo add json

  Package management is coming in a future release.
  Follow along: https://neohaskell.org
```

```
$ neo repl

  The interactive session is coming in a future release.
  Follow along: https://neohaskell.org
```

Design notes:
- These are NOT unknown commands — they're known, just not ready
- Tone is warm, not apologetic. "Coming in a future release" not "Not yet implemented"
- The URL is the same for all — one place to follow progress
- Exit code 0 — this is informational, not a failure
- Output goes to **stderr**

**Full list of unimplemented commands and their messages:**

| Command | Message |
|---------|---------|
| `neo test` | `Tests are coming in a future release.` |
| `neo add` | `Package management is coming in a future release.` |
| `neo remove` | `Package management is coming in a future release.` |
| `neo repl` | `The interactive session is coming in a future release.` |
| `neo clean` | `Clean is coming in a future release.` |
| `neo info` | `Project info is coming in a future release.` |
| `neo update` | `Self-update is coming in a future release.` |

---

### Project detection

Commands that operate on an existing project (`neo build`, `neo run`, and all future project-scoped commands) find the project root by walking up the directory tree looking for `neo.json`.

This means they work from any subdirectory inside the project — Jess doesn't need to `cd` to the root.

```
$ pwd
/home/jess/my-counter/src/Counter

$ neo build
  ⠋ Building my-counter...
  ✓ Built (2.3s)
```

**Walk algorithm:**
1. Check current directory for `neo.json`
2. If not found, check parent directory
3. Repeat until filesystem root (`/`)
4. If not found anywhere: show error and exit

**Not in a project:**

```
$ neo build

  Not a NeoHaskell project.
  Run neo init <name> to create one.
```

Design notes:
- Same behavior as `git` — works from any subdirectory
- `neo init` is the only command that does NOT require an existing `neo.json`
- The project name shown in output (e.g., `Building my-counter...`) comes from `neo.json`, not the directory name
- Exit code 1 when project not found

---

### Output conventions

**stderr vs stdout**

| Stream | What goes there |
|--------|-----------------|
| stderr | All human-readable messages: spinners, ✓/✗, errors, hints, welcome text, help text |
| stdout | Machine-readable data: app output (from `neo run`), `neo --version` |

The rule: if a human reads it, it's stderr. If a script reads it, it's stdout.

This means `neo run` can be piped safely — the build status goes to stderr, the app's output goes to stdout.

```sh
# This works — build noise stays out of the pipe
neo run | grep 'count:'
```

**TTY detection**

When stderr is a TTY (interactive terminal):
- Spinners are shown (`⠋ Building...`)
- Color is used (green ✓, red ✗)
- Progress is animated

When stderr is NOT a TTY (piped, redirected, CI):
- Spinners are replaced with static text (`Building my-counter...`)
- No ANSI color codes
- No cursor movement
- Output is line-buffered and safe to capture

CI example (no TTY):
```
Building my-counter...
Built (2.3s)
```

Interactive example (TTY):
```
  ⠋ Building my-counter...
  ✓ Built (2.3s)
```

**NO_COLOR**

When the `NO_COLOR` environment variable is set (any value), all ANSI formatting is stripped. Spinners still animate, but without color. This follows the [no-color.org](https://no-color.org) convention.

```sh
NO_COLOR=1 neo build   # Monochrome output, spinners still work
```

---

### The `--verbose` flag

`--verbose` is available on `neo build` and `neo run`. It is the only flag either command accepts.

**What `--verbose` does:**

Without `--verbose`, `neo` shows a clean summary — spinner during build, ✓/✗ on completion, errors with source context. The toolchain's internal output is hidden.

With `--verbose`, the full compiler output streams to stderr in real time. Every line the compiler produces is shown, unfiltered.

**Without `--verbose` (default):**

```
$ neo build

  ⠋ Building my-counter...
  ✓ Built (4.1s)
```

**With `--verbose`:**

```
$ neo build --verbose

  Building my-counter...
  Resolving dependencies...
  Configuring my-counter-0.1.0...
  Preprocessing library for my-counter-0.1.0...
  Building library for my-counter-0.1.0...
  [1 of 4] Compiling Config
  [2 of 4] Compiling Counter.Core
  [3 of 4] Compiling Counter.Entity
  [4 of 4] Compiling Main
  ✓ Built (4.1s)
```

**With `--verbose` on failure:**

```
$ neo build --verbose

  Building my-counter...
  Resolving dependencies...
  Configuring my-counter-0.1.0...
  Preprocessing library for my-counter-0.1.0...
  Building library for my-counter-0.1.0...
  [1 of 4] Compiling Config
  [2 of 4] Compiling Counter.Core
  [3 of 4] Compiling Counter.Entity

  src/Counter/Entity.hs:12:5: error:
      Expected type: Int
      Actual type: String

     12 |   value = "five"
          |           ^^^^^^

  ✗ Build failed
  1 error found.
```

**The contract:**

| Behavior | Default | `--verbose` |
|----------|---------|-------------|
| Spinner during build | ✓ | replaced by streaming output |
| Compiler progress lines | hidden | shown |
| Dependency resolution lines | hidden | shown |
| Error messages | shown (cleaned) | shown (raw) |
| Build time on completion | ✓ | ✓ |
| ✓/✗ summary | ✓ | ✓ |

Design notes:
- `--verbose` is for debugging — when the build is failing in a way that the cleaned output doesn't explain
- It does NOT change what succeeds or fails — only what is shown
- In `--verbose` mode, the spinner is replaced by streaming output (can't animate while streaming)
- All `--verbose` output goes to **stderr**
- `neo run --verbose` applies `--verbose` to the build phase only — app output is always unmodified on stdout

---

### Toolchain management (internal)

The `neo` CLI manages the build toolchain so Jess never has to think about it. This section documents the internal behavior — it is never surfaced in user-facing messages.

- `neo` detects whether it's already inside a managed shell environment
- If inside: runs build commands directly
- If outside: bootstraps the environment automatically
- Jess never types any build tool commands — `neo` is the only command she needs

If the toolchain is not installed:

```
$ neo build

  neo needs to set up its build tools. This takes a few minutes the first time.
  Setting up toolchain...
  ✓ Ready

  ⠋ Building my-counter...
  ✓ Built (2.3s)
```

If setup fails:

```
$ neo build

  Couldn't set up the build tools.
  Check your internet connection and try again.
  If the problem persists: https://neohaskell.org/help
```

Design notes:
- The words "Nix", "Cabal", "GHC", "flake" never appear in user-facing messages
- These are implementation details — Jess doesn't need to know them
- Error messages point to `https://neohaskell.org/help`, not to Nix or Cabal documentation

## Chapter 1: neo init

Creates a new NeoHaskell project with a working Counter example.

### Pre-MVP note

In the pre-MVP, generated source files use `.hs` (standard Haskell) instead of `.nh`. The project structure, naming conventions, and NeoHaskell style (pipes, qualified imports, nhcore types) are identical — only the file extension differs. When the transpiler ships, `neo init` switches to `.nh` and the rest follows.

Additionally, `neo init` generates Cabal and Nix files that the full vision hides inside `.neo/`:
- `<project-name>.cabal` — Cabal package description (generated from `neo.json`, not hand-edited)
- `flake.nix` — Nix flake for reproducible builds
- `flake.lock` — Nix lock file (generated by Nix on first build)

### Usage

```
neo init <project-name>
```

No flags. No options. No interactive prompts. (`--help` and `--version` are handled globally — see §CLI Infrastructure below.)

### Flow

1. Validate project name
2. Create project directory
3. Generate all project files (including Counter example)
4. Initialize git repository
5. Print success with next steps

### Name Validation

Rules:
- Starts with a lowercase letter
- Contains only lowercase letters, digits, and hyphens
- 1–64 characters
- No consecutive hyphens (`my--project`)
- Does not start or end with a hyphen

```
Valid:    my-project, counter, app2, hello-world
Invalid:  My-Project, -start, end-, my--project, 123abc, _foo
```

### CLI Output

```
$ neo init my-counter

  ⠋ Creating project...
  ⠙ Initializing git...
  ✓ my-counter is ready

  cd my-counter
  neo run          # start your app
```

Design notes:
- Spinner during generation — silence for 3+ seconds feels broken. Two phases: project files, then git init.
- `✓` — one symbol. Success has a signal.
- `my-counter is ready` — present tense, project-as-subject, no trailing slash, no passive "Created"
- No "Next steps:" header — the commands ARE the next steps
- `# start your app` — Jess knows what `neo run` does without leaving the terminal
- All output goes to **stderr** — stdout stays clean for scripting/piping
- Spinner and `✓` are suppressed when not a TTY (CI, pipes). Respects `NO_COLOR` env var.

### Generated Structure

```
my-counter/
├── neo.json
├── my-counter.cabal          # Pre-MVP: visible. Full vision: inside .neo/
├── flake.nix                 # Pre-MVP: visible. Full vision: inside .neo/
├── README.md
├── AGENTS.md
├── .gitignore
├── .git/
├── src/
│   ├── Main.hs               # .nh in full vision
│   ├── App.hs
│   ├── Config.hs
│   └── Counter/
│       ├── Core.hs
│       ├── Entity.hs
│       ├── Service.hs
│       ├── Commands/
│       │   └── Increment.hs
│       ├── Events/
│       │   └── Increment.hs
│       ├── Queries/
│       │   └── CurrentCount.hs
│       └── Integrations.hs
└── tests/
    ├── Main.hs
    ├── Counter/
    │   ├── EntitySpec.hs
    │   └── Commands/
    │       └── IncrementSpec.hs
    ├── commands/
    │   └── increment.hurl
    ├── queries/
    │   └── current-count.hurl
    └── scenarios/
        └── counter-workflow.hurl
```

No `neo.lock` — created later when dependencies are added.

### Generated Files

**neo.json**

```json
{
  "name": "my-counter",
  "version": "0.1.0",
  "dependencies": {}
}
```

Minimal. `main` is always `src/Main.hs` by convention — no field needed.

**README.md** — Jess's first map of the project. Tone: a senior dev left her a note.

    # my-counter
    
    A counter app built with NeoHaskell. Run it, break it, make it yours.
    
    ## Run it
    
    ```sh
    neo run
    ```
    
    That's it. Your app is running.
    
    ## How it works
    
    NeoHaskell apps are built around a simple idea:
    **commands change things, queries read things.**
    
    Your project has a working counter that shows both:
    
    | File                                   | What it does                     |
    |----------------------------------------|----------------------------------|
    | `src/Counter/Core.hs`                  | Shared types and helpers         |
    | `src/Counter/Entity.hs`                | The counter — holds the count    |
    | `src/Counter/Commands/Increment.hs`    | The "add 1" action               |
    | `src/Counter/Events/Increment.hs`      | Records that count changed       |
    | `src/Counter/Queries/CurrentCount.hs`  | Reads the current count          |
    | `src/Counter/Integrations.hs`          | Auto-increments every second     |
    
    You don't need to understand all of this yet.
    Open `src/Main.hs` and read from the top — it's meant to be readable.
    
    ## Try this first
    
    The fastest way to learn is to change something:
    
    1. Open `src/Counter/Integrations.hs`
    2. Change the timer interval from `1` second to `5` seconds
    3. Run `neo run` again — watch the counter slow down
    
    That's the loop. Change code, run it, see what happens.
    The compiler will catch your mistakes before you do.
    
    ## Commands
    
    | Command          | What it does                 |
    |------------------|------------------------------|
    | `neo run`        | Start the app                |
    | `neo test`       | Run the tests                |
    | `neo build`      | Build for production         |
    | `neo add <name>` | Add a dependency             |
    | `neo info`       | Show project info            |
    
    ## Go further
    
    - Docs: https://neohaskell.org/docs
    - Community: https://neohaskell.org/community
    
    If something is confusing, that's a bug in our docs — tell us.

Design notes:
- Leads with `neo run` — one command, immediate feedback
- 2-column table — plain English only. Jargon (Entity, Command, Event) is earned later, not front-loaded.
- "Try this first" gives a concrete experiment she can do in 2 minutes
- "commands change things, queries read things" teaches the mental model in one line
- Closing line reframes confusion as the tool's problem, not hers
- Deliberately omits: installation, Event Modeling theory, architecture, contribution guidelines, badges
**.gitignore**

```
.neo/
dist-newstyle/
flake.lock
```

`.neo/` is the build output directory (future). `dist-newstyle/` is Cabal's build cache (pre-MVP). `flake.lock` is generated by Nix and not hand-edited.

**src/Main.hs** — Entry point. Thin launcher that runs the app.
- Calls `Application.run app` — that's it
- All wiring lives in App.hs, not here

**src/App.hs** — Application composition. Wires everything together.
- Registers config, transport, services, queries, integrations
- `Application.new |> Application.withConfig @CounterConfig |> Application.withService Counter.Service.service |> ...`
- This is the "fat" file — all the plumbing in one place

**src/Config.hs** — Project configuration via declarative DSL.
- Defines `CounterConfig` with fields for HTTP port, etc.
- Each field: type, default value, env var, optional CLI flag
- `defineConfig "CounterConfig" [ Config.field @Int "httpPort" |> Config.defaultsTo 8080 |> Config.envVar "HTTP_PORT" ]`

**src/Counter/Core.hs** — Domain types and helpers shared across the Counter module.
- Types like `CounterId` that commands, events, and queries all reference
- Helper functions used by multiple files in the module
- This is NOT the entity definition — it's the shared vocabulary

**src/Counter/Entity.hs** — Counter entity definition, event ADT, and update function.
- State: `{ value : Int }` (starts at 0)
- Event ADT: `type CounterEvent = Increment Increment.Event` — one branch per event module, wrapping its event type
- Update function: `update : CounterEvent -> Counter -> Counter` — pattern matches on all events, applies state changes
- Imports event modules qualified (e.g., `import Counter.Events.Increment qualified as Increment`)
- This is the Entity — the transaction boundary and single source of truth for state transitions

**src/Counter/Service.hs** — Command registration for the Counter entity.
- `service = Service.new |> Service.command @Increment`
- Registers all commands so the application knows about them

**src/Counter/Commands/Increment.hs** — Increment command.
- Command: `Increment { amount : Int }`
- Decide function: validates and produces `Incremented` event
- Validation: `amount > 0`

**src/Counter/Events/Increment.hs** — Event produced by the Increment command.
- Event: `Incremented { amount : Int, newValue : Int }`
- This demonstrates Pattern 1 (State Change): Command → Event

**src/Counter/Queries/CurrentCount.hs** — CurrentCount query.
- Input: (none — reads the counter directly)
- Output: `{ value : Int }`
- Fed by: `Incremented` events
- This demonstrates Pattern 2 (View): Event → Query → UI

**src/Counter/Integrations.hs** — Outbound and inbound integrations for the Counter.
- Outbound: reacts to `Incremented` events — logs the new count to console
- Inbound: timer that fires `Increment { amount = 1 }` every second
- This demonstrates Pattern 3 (Automation): Event → Integration and Pattern 4 (External Input): Timer → Command

(Full Counter Event Model defined in AGENTS.md §Example)

**AGENTS.md** — AI assistant instructions for the project.
- Explains the Event Modeling variant used in NeoHaskell (Entity, Command, Event, Query, Integration)
- Maps Event Modeling concepts to NeoHaskell file structure (`src/<Entity>/Entity.hs`, `Core.hs`, `Service.hs`, `Commands/`, `Events/`, `Queries/`, `Integrations.hs`)
- Points AI agents to the right places: where to find entity definitions, how commands produce events, how queries project state
- Documents project conventions so any AI coding assistant understands the architecture immediately
- Content is the same for every project (not project-specific) — it teaches the NeoHaskell way

**tests/Main.hs** — Test entry point. Auto-discovers all `*Spec.hs` files.
- One line: `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}`
- No manual test registration needed — add a `*Spec.hs` file and it's picked up

**tests/Counter/EntitySpec.hs** — Unit tests for the Counter entity.
- Tests the `update` function: applying `Incremented` event to state
- Verifies initial state starts at 0
- Verifies increment updates the value correctly
- Structure: `describe "Counter" do ... it "increments value" \_ -> do ...`

**tests/Counter/Commands/IncrementSpec.hs** — Unit tests for the Increment command.
- Tests the `decide` function: valid input produces `Incremented` event
- Tests validation: rejects non-positive amounts
- Uses piped assertions: `result |> shouldBe expected`

**tests/commands/increment.hurl** — Integration test for the Increment command.
- `POST http://localhost:8080/commands/increment`
- Verifies response has `entityId` and correct structure
- Tests the full stack: HTTP → command → event → response

**tests/queries/current-count.hurl** — Integration test for the CurrentCount query.
- `GET http://localhost:8080/queries/current-count`
- Verifies response is a JSON collection with expected shape

**tests/scenarios/counter-workflow.hurl** — End-to-end scenario.
- Step 1: POST increment command, capture `counter_id`
- Step 2: GET current count with `retry` (eventual consistency)
- Step 3: Verify the count matches the increment
- Demonstrates captures, template variables, and retry patterns

_Future: NeoHaskell will ship a Given-When-Then (GWT) framework for Event Modeling tests — Given events, When command, Then expected events. This will be the primary way to test domain logic, replacing manual unit tests for commands._

### Errors

Errors are conversations, not verdicts. Each tells Jess what went wrong and what to do right now.

| Scenario | Message |
|----------|---------|
| No name given | `What should we call it? Try: neo init my-project` |
| Directory already exists | `my-counter already exists. Pick a different name, or cd into it.` |
| Invalid name (wrong casing) | `Project names must be lowercase. Did you mean: neo init my-counter?` |
| Invalid name (starts with digit) | `Project names must start with a letter. Try: neo init my-app` |
| Invalid name (leading/trailing hyphen) | `Project names can't start or end with a hyphen. Try: neo init my-app` |
| Invalid name (consecutive hyphens) | `Project names can't have consecutive hyphens. Try: neo init my-app` |
| Invalid name (invalid characters) | `Project names can only contain lowercase letters, digits, and hyphens. Try: neo init my-app` |
| Invalid name (too long) | `That name is too long (max 64 characters). Try a shorter one.` |
| Extra arguments | `neo init takes one argument — the project name. Try: neo init my-app` |
| Permission denied | `Couldn't create my-counter — permission denied. Check that you have write access to this directory.` |
| Git not installed | `Git is required. Install it from https://git-scm.com, then try again.` |
| Path is a file | `my-counter is already a file. Pick a different name.` |

Pattern: what's wrong → what to do. Suggest the fix, don't explain the grammar.

### Decisions

| Decision | Rationale |
|----------|-----------|
| No `neo init .` (current directory) | Creating a project = creating a directory. The name IS the directory. |
| No `--no-git` | Git is free. Always initialize. No reason not to. |
| No `--template` | Every project starts the same. One path. |
| No interactive prompts | Scriptable. One command, complete result. |
| Counter always included | It's the "Hello World" of NeoHaskell — demonstrates Entity, Command, Event, Query, Integration in one working example. |
| tests/ always included | TDD is the NeoHaskell way. Unit tests + integration tests from day one. |
| No `neo.lock` at init | No dependencies yet. Lock file appears on first dependency resolution. |
| AGENTS.md always included | Teaches AI assistants the NeoHaskell way — Event Modeling patterns, file structure conventions, where to find what. Same content for every project. |
| `neo add` is packages only | Domain scaffolding (`neo scaffold entity`, `neo scaffold command`) is planned for v2. |
| `.cabal` and `flake.nix` visible (pre-MVP) | Full vision hides these in `.neo/`. Pre-MVP exposes them because the transpiler and build abstraction aren't ready yet. Jess shouldn't edit them, but they won't confuse her — `.gitignore` and README don't mention them. |

---

## Chapter 2: neo build

Compiles the project. Handles the toolchain so Jess doesn't have to.

### Usage

```
neo build [--verbose]
```

| Flag | Description |
|------|-------------|
| `--verbose` | Show raw compiler output prefixed with `[build]`. For debugging toolchain issues. |

### Flow

1. Find project root (walk up directories for `neo.json`)
2. Read project name from `neo.json`
3. Overwrite generated toolchain files (`flake.nix`, `*.cabal`) from project config — these are ALWAYS regenerated, never hand-edited
4. Detect environment (inside Nix shell or not)
5. If first build (no `.nix` cache or `dist-newstyle/`): run first-build flow with streaming progress
6. Run `cabal build <project-name>` (via `nix develop --command` if outside Nix shell)
7. Capture compiler output, post-process on completion
8. Report success or failure

If any toolchain file is missing or corrupt (flake.nix, cabal file, hix config), neo silently deletes and regenerates it before building. Neo never asks Jess to fix toolchain files.

### CLI Output

**Success (normal build):**

```
$ neo build

  Building my-counter...
  ✓ Built (2.3s)
```

**Success (first build — streaming progress):**

The first build downloads the compiler and all dependencies. This can take several minutes. Instead of a silent spinner, neo streams progress so Jess knows it's working:

```
$ neo build

  Setting up NeoHaskell for the first time.
  This takes a few minutes — it won't happen again.

  Downloading compiler...     ━━━━━━━━━━━━━━━━━━━━ 100%  1.2 GB
  Downloading dependencies... ━━━━━━━━━━━━━━━━━━━━ 100%  847 MB
  Compiling dependencies...   [23/47]
  Building my-counter...      [1/3]

  ✓ Ready. First build took 3m 42s.
    Future builds will be much faster.
```

Design notes for first build:
- Progress bars show download size and completion — Jess can see it's actually doing something
- Compilation shows `[n/total]` module count — gives a sense of progress without exposing module names
- The final message reassures: this is a one-time cost
- If TTY: progress bars animate in-place. If not TTY: one line per stage (see CI mode below)

**Failure (compilation error):**

```
$ neo build

  Building my-counter...
  ✗ Build failed

  ── src/Counter/Entity.hs · line 12 ──────────────────────────

     Expected type: Int
     Actual type: String

     12 │   value = "five"
        │           ^^^^^^

  1 error found.
```

**Failure (multiple errors):**

```
$ neo build

  Building my-counter...
  ✗ Build failed

  ── src/Counter/Entity.hs · line 12 ──────────────────────────

     Expected type: Int
     Actual type: String

     12 │   value = "five"
        │           ^^^^^^

  ── src/Counter/Commands/Increment.hs · line 8 ───────────────

     Variable not in scope: amout
     Perhaps you meant: amount

      8 │ decide cmd entity = amout cmd
        │                     ^^^^^

  2 errors found.
```

**Internal error (auto-recovered):**

When a build error comes from neo's own libraries (not Jess's code), neo retries silently. If it recovers:

```
$ neo build

  Building my-counter...
  ↻ Retrying (internal error, not your code)
  ✓ Built (4.1s)
```

If auto-recovery fails:

```
$ neo build

  Building my-counter...
  ✗ Build failed due to an internal error in neo's libraries.

  Run neo build --verbose for details.
  If the problem persists: https://neohaskell.org/help
```

**Verbose mode (`--verbose`):**

```
$ neo build --verbose

  [build] Configuring my-counter-0.1.0...
  [build] Preprocessing library for my-counter-0.1.0...
  [build] Building library for my-counter-0.1.0...
  [build] [1 of 3] Compiling Counter.Entity
  [build] [2 of 3] Compiling Counter.Commands.Increment
  [build]
  [build] /src/Counter/Entity.hs:12:5: error:
  [build]     • Expected type: Int
  [build]     • Actual type: String
  [build]     |
  [build]  12 |   value = "five"
  [build]     |           ^^^^^^
  ✗ Build failed

  1 error found.
```

Design notes for `--verbose`:
- Every line of raw compiler/toolchain output is prefixed with `[build]`
- Error summary still appears at the end (without prefix)
- Useful for debugging when neo's error post-processing hides relevant context

**Warnings:**

```
$ neo build

  Building my-counter...
  ✓ Built (2.3s)

  2 warnings:

  ── src/Counter/Entity.hs · line 5 ──────────────────────────

     Unused import: Data.Text

  ── src/Counter/Commands/Increment.hs · line 3 ───────────────

     Unused variable: oldValue
```

Design notes for warnings:
- Warnings appear AFTER the success line — build succeeded, these are informational
- Capped at 5 warnings displayed. If more: `... and 3 more warnings. Run neo build --verbose to see all.`
- No caret/source context for warnings (keep them compact)
- Warnings never block the build

**Non-TTY / CI mode:**

When stdout is not a TTY (piped, CI, redirected), neo uses a plain format:

```
$ neo build 2>&1 | cat

neo: Building my-counter...
neo: Built (2.3s)
```

```
$ neo build 2>&1 | cat   # failure

neo: Building my-counter...
neo: Build failed
neo: src/Counter/Entity.hs:12:5: Expected type: Int, Actual type: String
neo: 1 error found.
```

Design notes for CI mode:
- No spinner, no progress bars, no color, no box-drawing characters
- Each line prefixed with `neo:` for grep-ability in CI logs
- Errors are single-line (no multi-line source context)
- Timestamps omitted (CI systems add their own)

### Error post-processing (pre-MVP)

GHC errors are captured and post-processed before display:

| Step | What it does |
|------|-------------|
| 1. Strip Cabal wrapper noise | Remove `Building library for...`, `Configuring...`, `Resolving dependencies...` |
| 2. Rewrite file paths | Strip build-dir prefix, show paths relative to project root |
| 3. Add horizontal rule headers | `── src/File.hs · line N ──────` before each error block |
| 4. Replace box-drawing | Use `│` for line gutters consistently |
| 5. Rewrite jargon (see table) | Replace GHC-specific terms with plain language |
| 6. Filter internal errors | Errors in neo's own libraries → auto-retry or show internal error message |
| 7. Count and summarize | `N error(s) found.` / `N warning(s):` at the end |

**Jargon rewrite table (pre-MVP):**

| GHC says | Neo says |
|----------|----------|
| `Not in scope` | `Variable not found` |
| `No instance for` | `Missing implementation for` |
| `Couldn't match type` | `Type mismatch` |
| `Ambiguous occurrence` | `Multiple definitions found for` |
| `Illegal qualified name` | `Invalid module reference` |
| `Parse error on input` | `Unexpected syntax` |

Future: full Elm-style error rewriting with NeoHaskell-specific hints. Pre-MVP does this minimal set.

### Edge cases

| Scenario | Behavior |
|----------|----------|
| Empty project (no .hs files) | `Nothing to build — no source files found in src/.` |
| Disk full | `Build stopped — not enough disk space. Free some space and try again.` |
| Network error (first build) | `Couldn't download dependencies — check your internet connection and try again.` |
| Ctrl+C during build | Build process killed immediately. No partial output. Next line: `Stopped` |
| Ctrl+C during first build | Same as above. Next `neo build` resumes where it left off (Nix cache is incremental). |
| Build tools not found | `Neo's build tools aren't installed. Re-run the Neo installer: https://neohaskell.org/install` |

### Errors

| Scenario | Message | `--verbose` |
|----------|---------|-------------|
| Not in a project | `Not a NeoHaskell project. Run neo init <name> to create one.` | Same |
| Build tools not installed | `Neo's build tools aren't installed. Re-run the Neo installer: https://neohaskell.org/install` | Same |
| Toolchain setup failed | `Something went wrong setting up the build environment, and neo couldn't fix it automatically.`<br>`Try: neo clean, then neo build.`<br>`If that doesn't help: https://neohaskell.org/help` | + raw error output |
| Internal compilation error | `Build failed due to an internal error in neo's libraries.`<br>`Run neo build --verbose for details.` | + full compiler output |
| Network error (first build) | `Couldn't download dependencies — check your internet connection and try again.` | Same |
| neo.json damaged | `neo.json is missing or damaged, and neo couldn't repair it.`<br>`Try: neo init <name> to create a fresh project, then copy your src/ files.` | Same |
| Compilation error (user code) | Formatted error with source context (see output above) | + raw GHC output |

### Decisions

| Decision | Rationale |
|----------|-----------|
| Streaming log for first build (not spinner) | A spinner for 3-5 minutes is a trust-killer. Progress bars show Jess the system is working, and set correct time expectations. |
| `--verbose` flag | Normal mode hides toolchain noise. When something goes wrong internally, Jess (or a helper) needs to see what the compiler actually said. |
| No `--release` / `--optimize` | Pre-MVP has one build mode. Optimization profiles come later. |
| No `--watch` | File watching is significant scope. Ships separately. |
| Auto-detect Nix shell | If Jess is already in `nix develop`, don't nest another. Just run Cabal. |
| Show build time | Builds happen often. Jess should notice regressions. |
| Always overwrite toolchain files | `flake.nix` and `.cabal` are generated from `neo.json`. Neo owns them. If they're corrupt or hand-edited, neo silently regenerates. |
| Auto-retry internal errors | If an error is in neo's libraries (not Jess's code), try `neo clean` + rebuild automatically before reporting failure. |
| Strip Cabal noise | Jess doesn't need `Resolving dependencies...` and `Configuring my-counter-0.1.0...`. She needs her errors. |
| Rewrite GHC jargon (minimal set) | Full error rewriting comes with the transpiler. Pre-MVP does a small rewrite table for the worst offenders. |
| Cap warnings at 5 | A wall of warnings buries the signal. Show the first 5, tell Jess how to see the rest. |
| Errors use horizontal rule headers | `── file · line ──────` separates errors visually without requiring color. Works in every terminal. |
| Filter internal compiler errors | Errors from neo's own libraries are not Jess's problem. Hide them by default, show with `--verbose`. |

---

## Chapter 3: neo run

Builds and runs the project. The command Jess uses most.

### Usage

```
neo run [--verbose]
```

| Flag | Description |
|------|-------------|
| `--verbose` | Show full compiler output instead of the spinner summary |

No other flags. No `-- <args>`. No `--watch`. No `--no-build`.

### Flow

1. Find project root (walk up directories for `neo.json`)
2. Read project name from `neo.json`
3. Detect environment (inside Nix shell or not)
4. Build the project (identical to `neo build`)
   - If build fails → print errors to stderr, exit 1
5. Launch the executable
6. Become transparent: child stdout → neo stdout, child stderr → neo stderr
7. On Ctrl+C → send SIGINT to child, wait for it to exit, print `Stopped` to stderr
8. Exit with the child's exit code

### stdout / stderr contract

This is the most important thing to get right. Two phases, two rules.

**Phase 1 — Before the app starts (build phase):**
Everything neo emits goes to **stderr**. This includes the spinner, `✓ Built`, `✗ Build failed`,
compiler errors, and the blank separator line. Stdout is silent.

**Phase 2 — After the app starts (run phase):**
Neo becomes transparent. The child process's stdout flows directly to neo's stdout.
The child process's stderr flows directly to neo's stderr.
Neo adds nothing. Neo removes nothing. Neo does not buffer.

**Ctrl+C / shutdown:**
`Stopped` is printed to **stderr** after the child exits.

```
neo stderr:   ⠋ Building my-counter...
neo stderr:   ✓ Built (2.3s)
neo stderr:   (blank line)
child stdout: Counter running on http://localhost:8080
child stdout: Current count: 0
child stdout: Current count: 1
neo stderr:  Stopped
```

This means `neo run | grep 'count'` works exactly as if the app were running directly.
Piping, redirection, and log aggregators all see clean app output.

### Output examples

**Success — first run (build takes time):**

```
$ neo run

  ⠋ Building my-counter...
  ✓ Built (2.3s)

Counter running on http://localhost:8080
Current count: 0
Current count: 1
Current count: 2
^C
Stopped
```

The build output (spinner, `✓ Built`) is on stderr. The app output (`Counter running...`,
`Current count: ...`) is on stdout. `Stopped` is on stderr.

**Success — no changes since last build:**

```
$ neo run

  ✓ Built (0.1s)

Counter running on http://localhost:8080
Current count: 0
```

The spinner is skipped when the build is already up to date. The `✓ Built (0.1s)` line
still appears — Jess should always know a build check happened.

**Success — first build ever (toolchain setup):**

```
$ neo run

  ⠋ Setting up toolchain (first build)...
  ⠙ Building my-counter...
  ✓ Built (1m 12s)

Counter running on http://localhost:8080
Current count: 0
```

First build downloads the entire toolchain. Could be minutes. The spinner message
changes to set expectations. Jess should not think it's frozen.

**Success — with `--verbose`:**

```
$ neo run --verbose

  ⠋ Building my-counter...
  Build profile: -w ghc-9.8.4 -O1
  [1 of 3] Compiling Counter.Entity
  [2 of 3] Compiling Counter.Commands.Increment
  [3 of 3] Compiling Main
  ✓ Built (2.3s)

Counter running on http://localhost:8080
Current count: 0
```

Verbose mode streams compiler output line-by-line to stderr as it arrives.
The spinner is replaced by streaming log lines. App output still passes through unchanged.

**Build failure:**

```
$ neo run

  ⠋ Building my-counter...
  ✗ Build failed

  src/Counter/Entity.hs:12:5: error:
      Expected type: Int
      Actual type: String

     12 |   value = "five"
        |           ^^^^^^

  1 error found.
```

App never starts. All output is on stderr. Exit code is 1. Stdout is empty.

**App exits with error (runtime crash):**

```
$ neo run

  ✓ Built (0.1s)

Counter running on http://localhost:8080
thread blocked indefinitely in an STM transaction
CallStack (from HasCallStack):
  error, called at src/Counter/Service.hs:42:5
```

```
$ echo $?
1
```

Neo passes the crash output through unchanged. Neo exits with whatever code the app returned.
No wrapping, no "your app crashed" message — the app's own output is the message.

**Ctrl+C while app is running:**

```
$ neo run

  ✓ Built (0.1s)

Counter running on http://localhost:8080
Current count: 0
Current count: 1
^C
Stopped
```

```
$ echo $?
0
```

`^C` is the terminal echoing the keypress — neo doesn't print it.
Neo sends SIGINT to the child process and waits for it to exit.
Once the child exits, neo prints `Stopped` to stderr (no period, no exclamation mark).
Neo exits with the child's exit code.

If the app handles SIGINT gracefully and exits 0, neo exits 0.
If the app exits non-zero on SIGINT, neo exits with that code.

**Ctrl+C during build (before app starts):**

```
$ neo run

  ⠋ Building my-counter...
  ^C
  Stopped
```

Build is cancelled. Exit code is 1. App never starts.

### Errors

| Scenario | Output | Exit code |
|----------|--------|-----------|
| Not in a project | `Not a NeoHaskell project. Run neo init <name> to create one.` (stderr) | 1 |
| Build failure | Compiler errors to stderr (same as `neo build`) | 1 |
| Build tools not available | `Neo's build tools aren't installed. Re-run the installer: https://neohaskell.org/install` (stderr) | 1 |
| Executable not found after build | `Build succeeded but neo couldn't find the executable. This is likely a bug. Run neo build --verbose for details.` (stderr) | 1 |
| Runtime crash | App output passes through unchanged | App's exit code |
| Port already in use | App output passes through unchanged | App's exit code |

### Decisions

| Decision | Rationale |
|----------|-----------|
| Always build first | Safety. Stale binaries cause confusion. Cabal skips unchanged modules — unchanged code is near-instant. |
| No `--no-build` | If unchanged, build is instant. No flag needed. Jess should never run stale code. |
| No `-- <args>` (pre-MVP) | App arguments come later. Pre-MVP runs with defaults. |
| No `--watch` / `--reload` | Hot reload is a major feature. Ships separately when ready. |
| `--verbose` available | Mirrors `neo build --verbose`. Jess needs an escape hatch when something is wrong with the build. |
| Exit code mirrors child | `neo run` is a thin wrapper. Scripts that check exit codes should see the app's code, not neo's. |
| `Stopped` not `Stopped.` | No period. It's a status word, not a sentence. Consistent with how terminals report signals. |
| App output unmodified | Neo is transparent once the app starts. No prefixing, no buffering, no wrapping. Piping must work. |
| Build output to stderr | Keeps stdout clean for the app. `neo run | grep foo` should grep app output, not build messages. |
| Neo never mentions Nix or Cabal | Jess doesn't know what those are. The toolchain is neo's problem. |
| Blank line between build and app output | Visual breath. Jess can see where neo's output ends and her app's begins. |

---

## Post-MVP Commands

The following commands are planned but not yet designed. They will be specified as the pre-MVP (init, build, run) stabilizes.

---

## Chapter 4: neo test

_To be defined_

---

## Chapter 5: neo add

Adds a package dependency to the project. Domain scaffolding (`neo scaffold entity`, `neo scaffold command`) is v2.

_To be defined_

---

## Chapter 6: neo remove

_To be defined_

---

## Chapter 7: neo repl

_To be defined_

---

## Chapter 8: neo clean

_To be defined_

---

## Chapter 9: neo info

Shows project and toolchain information. The command Jess runs when something is weird
or when filing a bug report.

### Usage

```
neo info
```

No flags. No options.

### Purpose

Two use cases, one command:

1. **"Am I set up correctly?"** — Jess just installed neo and wants to confirm everything
   is working before she starts a project.
2. **"Something is wrong — help me debug."** — Jess is filing a GitHub issue and needs
   to paste her environment details.

`neo info` is the single source of truth for toolchain details. It is NOT in `--version`
(which stays minimal: `neo 0.1.0`). It is NOT scattered across error messages.

### Flow

1. Detect whether inside a NeoHaskell project (walk up for `neo.json`)
2. If inside a project: show project section + toolchain section
3. If outside a project: show toolchain section only (no error — this is valid)
4. Detect toolchain versions by running version checks
5. Print all output to stdout

### stdout / stderr contract

`neo info` is a read-only inspection command. All output goes to **stdout**.
It is designed to be copy-pasted and piped. No spinners. No color required.
Respects `NO_COLOR`.

### Output examples

**Inside a project (normal case):**

```
$ neo info

Project
  name     my-counter
  version  0.1.0
  config   /home/jess/my-counter/neo.json

Toolchain
  neo      0.1.0
  language 9.8.4

```

"language" is the NeoHaskell language version — not "GHC", not "Haskell".
Jess doesn't need to know what GHC is. The version number is what matters for bug reports.

**Outside a project:**

```
$ neo info

Toolchain
  neo      0.1.0
  language 9.8.4

```

No error. No "not in a project" warning. Just the toolchain. This is valid — Jess might
run `neo info` before creating her first project.

**Toolchain not fully set up (language runtime missing):**

```
$ neo info

Toolchain
  neo      0.1.0
  language not found — run: neo build (to set up the toolchain)

```

The language runtime is installed on first build (via Nix). Before that, it's not present.
The message tells Jess what to do, not what's wrong internally.

**Full output (post-MVP, when more toolchain components exist):**

```
$ neo info

Project
  name         my-counter
  version      0.1.0
  config       /home/jess/my-counter/neo.json
  dependencies 3

Toolchain
  neo          0.1.0
  language     9.8.4
  build-tool   3.10.3.0
  platform     linux-x86_64

```

"build-tool" is the build tool version — not "Cabal". "platform" is the OS/arch.
These are useful for bug reports. Jess doesn't need to know the internal names.

### Field reference

| Field | What it shows | User-facing label | Notes |
|-------|--------------|-------------------|-------|
| Project name | `name` from `neo.json` | `name` | Only shown inside a project |
| Project version | `version` from `neo.json` | `version` | Only shown inside a project |
| Config path | Resolved absolute path to `neo.json` | `config` | Tells Jess which project she's in |
| Dependency count | Number of entries in `dependencies` | `dependencies` | Post-MVP |
| Neo version | The `neo` binary version | `neo` | Always shown |
| Language version | GHC version | `language` | Always shown; "not found" if toolchain not set up |
| Build tool version | Cabal version | `build-tool` | Post-MVP |
| Platform | OS + architecture | `platform` | Post-MVP |

**Vocabulary rule:** Neo never uses the words "GHC", "Cabal", or "Nix" in user-facing output.
These are implementation details. The user-facing vocabulary is: `neo`, `language`, `build-tool`, `platform`.

### Errors

| Scenario | Output | Exit code |
|----------|--------|-----------|
| `neo.json` found but malformed | `neo.json is not valid JSON. Check the file for syntax errors.` (stderr) | 1 |
| `neo.json` found but missing required fields | `neo.json is missing required field: name.` (stderr) | 1 |
| Toolchain version check fails | Show `not found` for that field, continue | 0 |

### Decisions

| Decision | Rationale |
|----------|-----------|
| All output to stdout | `neo info` is data, not a status message. It should be pipeable and redirectable. |
| No spinners | This is a read command. It should be instant. No async work. |
| No color required | Output is designed to be copy-pasted into a GitHub issue. Color would be noise. |
| "language" not "GHC" | Jess doesn't know what GHC is. The version number is what matters. |
| "build-tool" not "Cabal" | Same principle. Neo owns the vocabulary. |
| Config path shown | Jess might have multiple projects. The path confirms which `neo.json` is active. |
| No error outside a project | `neo info` is valid anywhere. Toolchain info is always useful. |
| Toolchain section always shown | Even outside a project, Jess needs to know her toolchain is set up. |
| `not found` not an error | Missing language runtime is expected before first build. Don't alarm Jess. |
| Post-MVP fields noted | `dependencies`, `build-tool`, `platform` are useful but not critical for pre-MVP. Spec the shape now. |
| `--version` stays minimal | `neo --version` → `neo 0.1.0`. Toolchain details live here, not there. |
---

## Chapter 10: neo update

_To be defined_

---

_Last updated: 2026-02-27_
