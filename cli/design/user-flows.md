# Neo CLI — User Flow Chapters

## CLI Infrastructure (applies to all commands)

### --help

Every command supports `--help`. Help text is minimal and example-driven.

```
$ neo init --help
Create a new NeoHaskell project

Usage: neo init <project-name>

Example:
  neo init my-app
  cd my-app && neo run
```

```
$ neo --help
Usage: neo <command>

Commands:
  init    Create a new project
  run     Start the app
  build   Build for production
  test    Run the tests
  add     Add a dependency
  info    Show project info
  clean   Remove build artifacts
  repl    Start interactive session

Run neo <command> --help for details.
```

Design notes:
- Help text shows one example, not every flag
- Descriptions are present tense, imperative mood
- No manpage-style option tables — this is the full help

### --version

```
$ neo --version
neo 0.1.0
```

Minimal. No platform info, no build hash. Just the version.

### Output conventions

- **stderr** for all human-readable messages (spinners, ✓, errors, hints)
- **stdout** for machine-readable data only (JSON output, file lists when piped)
- **TTY detection**: spinners and color when interactive, plain text when piped or CI
- **NO_COLOR**: respected — strips all ANSI formatting when set
- **Unknown flags**: clean error — `Unknown flag: --no-git. neo init takes no flags.`


## Chapter 1: neo init

Creates a new NeoHaskell project with a working Counter example.

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
├── README.md
├── AGENTS.md
├── .gitignore
├── .git/
├── src/
│   ├── Main.nh
│   ├── App.nh
│   ├── Config.nh
│   └── Counter/
│       ├── Core.nh
│       ├── Entity.nh
│       ├── Service.nh
│       ├── Commands/
│       │   └── Increment.nh
│       ├── Events/
│       │   └── Increment.nh
│       ├── Queries/
│       │   └── CurrentCount.nh
│       └── Integrations.nh
└── tests/
    ├── Main.nh
    ├── Counter/
    │   ├── EntitySpec.nh
    │   └── Commands/
    │       └── IncrementSpec.nh
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

Minimal. `main` is always `src/Main.nh` by convention — no field needed.

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
    | `src/Counter/Core.nh`                  | Shared types and helpers         |
    | `src/Counter/Entity.nh`                | The counter — holds the count    |
    | `src/Counter/Commands/Increment.nh`    | The "add 1" action               |
    | `src/Counter/Events/Increment.nh`      | Records that count changed       |
    | `src/Counter/Queries/CurrentCount.nh`  | Reads the current count          |
    | `src/Counter/Integrations.nh`          | Auto-increments every second     |
    
    You don't need to understand all of this yet.
    Open `src/Main.nh` and read from the top — it's meant to be readable.
    
    ## Try this first
    
    The fastest way to learn is to change something:
    
    1. Open `src/Counter/Integrations.nh`
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
```

`.neo/` is the build output directory. That's the only thing to ignore.

**src/Main.nh** — Entry point. Thin launcher that runs the app.
- Calls `Application.run app` — that’s it
- All wiring lives in App.nh, not here

**src/App.nh** — Application composition. Wires everything together.
- Registers config, transport, services, queries, integrations
- `Application.new |> Application.withConfig @CounterConfig |> Application.withService Counter.Service.service |> ...`
- This is the “fat” file — all the plumbing in one place

**src/Config.nh** — Project configuration via declarative DSL.
- Defines `CounterConfig` with fields for HTTP port, etc.
- Each field: type, default value, env var, optional CLI flag
- `defineConfig "CounterConfig" [ Config.field @Int "httpPort" |> Config.defaultsTo 8080 |> Config.envVar "HTTP_PORT" ]`

**src/Counter/Core.nh** — Domain types and helpers shared across the Counter module.
- Types like `CounterId` that commands, events, and queries all reference
- Helper functions used by multiple files in the module
- This is NOT the entity definition — it’s the shared vocabulary

**src/Counter/Entity.nh** — Counter entity definition, event ADT, and update function.
- State: `{ value : Int }` (starts at 0)
- Event ADT: `type CounterEvent = Increment Increment.Event` — one branch per event module, wrapping its event type
- Update function: `update : CounterEvent -> Counter -> Counter` — pattern matches on all events, applies state changes
- Imports event modules qualified (e.g., `import Counter.Events.Increment qualified as Increment`)
- This is the Entity — the transaction boundary and single source of truth for state transitions

**src/Counter/Service.nh** — Command registration for the Counter entity.
- `service = Service.new |> Service.command @Increment`
- Registers all commands so the application knows about them

**src/Counter/Commands/Increment.nh** — Increment command.
- Command: `Increment { amount : Int }`
- Decide function: validates and produces `Incremented` event
- Validation: `amount > 0`

**src/Counter/Events/Increment.nh** — Event produced by the Increment command.
- Event: `Incremented { amount : Int, newValue : Int }`
- This demonstrates Pattern 1 (State Change): Command → Event

**src/Counter/Queries/CurrentCount.nh** — CurrentCount query.
- Input: (none — reads the counter directly)
- Output: `{ value : Int }`
- Fed by: `Incremented` events
- This demonstrates Pattern 2 (View): Event → Query → UI

**src/Counter/Integrations.nh** — Outbound and inbound integrations for the Counter.
- Outbound: reacts to `Incremented` events — logs the new count to console
- Inbound: timer that fires `Increment { amount = 1 }` every second
- This demonstrates Pattern 3 (Automation): Event → Integration and Pattern 4 (External Input): Timer → Command

(Full Counter Event Model defined in AGENTS.md §Example)

**AGENTS.md** — AI assistant instructions for the project.
- Explains the Event Modeling variant used in NeoHaskell (Entity, Command, Event, Query, Integration)
- Maps Event Modeling concepts to NeoHaskell file structure (`src/<Entity>/Entity.nh`, `Core.nh`, `Service.nh`, `Commands/`, `Events/`, `Queries/`, `Integrations.nh`)
- Points AI agents to the right places: where to find entity definitions, how commands produce events, how queries project state
- Documents project conventions so any AI coding assistant understands the architecture immediately
- Content is the same for every project (not project-specific) — it teaches the NeoHaskell way

**tests/Main.nh** — Test entry point. Auto-discovers all `*Spec.nh` files.
- One line: `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}`
- No manual test registration needed — add a `*Spec.nh` file and it's picked up

**tests/Counter/EntitySpec.nh** — Unit tests for the Counter entity.
- Tests the `update` function: applying `Incremented` event to state
- Verifies initial state starts at 0
- Verifies increment updates the value correctly
- Structure: `describe "Counter" do ... it "increments value" \_ -> do ...`

**tests/Counter/Commands/IncrementSpec.nh** — Unit tests for the Increment command.
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
| Invalid name (casing) | `Did you mean: neo init my-counter?` (auto-suggests kebab-case correction) |
| Extra arguments | `neo init takes one argument — the project name. Try: neo init my-counter` |
| Permission denied | `Couldn't create my-counter — permission denied. Check the parent directory permissions.` |
| Git not installed | `Git is required — install it from https://git-scm.com then try again.` |
| Path is a file, not a directory | `my-counter is a file, not a directory. Pick a different name.` |

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
---

## Chapter 2: neo add

Adds a package dependency to the project. Domain scaffolding (`neo scaffold entity`, `neo scaffold command`) is v2.

_To be defined_

---

## Chapter 3: neo remove

_To be defined_

---

## Chapter 4: neo build

_To be defined_

---

## Chapter 5: neo run

_To be defined_

---

## Chapter 6: neo test

_To be defined_

---

## Chapter 7: neo repl

_To be defined_

---

## Chapter 8: neo clean

_To be defined_

---

## Chapter 9: neo info

_To be defined_

---

## Chapter 10: neo update

_To be defined_

---

_Last updated: 2026-02-25_
