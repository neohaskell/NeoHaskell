# Haskell CLI App Scaffold with Event-Sourcing & CQRS: Best Practices Research

**Date**: March 12, 2026  
**Scope**: External best practices + NeoHaskell project conventions  
**Focus**: Minimal MVP scaffolds with hardcoded integrations and command-emitting flows

---

## EXECUTIVE SUMMARY

Building a Haskell CLI scaffold for event-sourced applications requires careful attention to:

1. **Command-line parsing architecture** (optparse-applicative patterns)
2. **Exit code semantics** (POSIX standards + graceful degradation)
3. **Event-sourcing decision functions** (pure, testable, deterministic)
4. **CQRS command/query separation** (clear boundaries)
5. **CLI UX edge cases** (TTY detection, NO_COLOR, error messaging)
6. **Acceptance testing patterns** (Hurl for blackbox testing)

This document synthesizes external research with NeoHaskell's existing patterns (testbed, style guide, user flows).

---

## PART 1: EXTERNAL BEST PRACTICES

### 1.1 Haskell CLI Architecture (optparse-applicative)

**Standard Pattern** (from FP Complete, Medium, Discourse):

```haskell
-- 1. Define command/option ADT
data Command
  = Init { projectName :: String }
  | Run { configPath :: FilePath }
  | Build { release :: Bool }
  deriving (Show)

data GlobalOptions = GlobalOptions
  { verbose :: Bool
  , configFile :: Maybe FilePath
  , command :: Command
  }
  deriving (Show)

-- 2. Build parser with subcommands
commandParser :: Parser Command
commandParser = hsubparser
  ( command "init" (info initParser (progDesc "Create a new project"))
  <> command "run" (info runParser (progDesc "Start the app"))
  <> command "build" (info buildParser (progDesc "Build for production"))
  )

initParser :: Parser Command
initParser = Init
  <$> argument str (metavar "PROJECT_NAME")

runParser :: Parser Command
runParser = Run
  <$> strOption
      ( long "config"
      <> short 'c'
      <> metavar "FILE"
      <> value "config.json"
      <> help "Configuration file"
      )

-- 3. Global options wrapper
globalParser :: Parser GlobalOptions
globalParser = GlobalOptions
  <$> switch (long "verbose" <> short 'v' <> help "Verbose output")
  <*> optional (strOption (long "config-file" <> metavar "FILE"))
  <*> commandParser

-- 4. Main entry point
main :: IO ()
main = do
  opts <- execParser (info (globalParser <**> helper) fullDesc)
  case command opts of
    Init name -> handleInit opts name
    Run cfg -> handleRun opts cfg
    Build release -> handleBuild opts release
```

**Key Principles**:
- **Applicative composition** over monadic chains (cleaner, more declarative)
- **Subparser for commands** (each command has own option set)
- **Global options before subcommand** (parsed first, available to all)
- **Helper + version** automatically included via `<**> helper`
- **Metavar for documentation** (shows in `--help`)

**Naming Convention**:
- Parser functions: `<commandName>Parser` (e.g., `initParser`, `runParser`)
- Option builders: `<optionName>Option` or inline with `long`/`short`
- Main dispatcher: `handle<CommandName>` (e.g., `handleInit`)

---

### 1.2 Exit Codes & Error Handling (POSIX Standards)

**Standard Exit Codes** (from sysexits.h, FP Complete, Stack Overflow):

```haskell
import System.Exit

-- POSIX standard exit codes
exitSuccess :: ExitCode  -- 0: Success
exitFailure :: ExitCode  -- 1: Generic error

-- BSD sysexits.h conventions (use exit-codes package)
-- EX_USAGE (64): Command line usage error
-- EX_DATAERR (65): Data format error
-- EX_NOINPUT (66): Cannot open input
-- EX_NOUSER (67): Addressee unknown
-- EX_NOHOST (68): Host name unknown
-- EX_UNAVAILABLE (69): Service unavailable
-- EX_SOFTWARE (70): Internal software error
-- EX_OSERR (71): System error
-- EX_OSFILE (72): Critical OS file missing
-- EX_CANTCREAT (73): Can't create output file
-- EX_IOERR (74): Input/output error
-- EX_TEMPFAIL (75): Temporary failure
-- EX_PROTOCOL (76): Remote protocol error
-- EX_NOPERM (77): Permission denied
-- EX_CONFIG (78): Configuration error

-- Pattern: Catch errors, log to stderr, exit with code
main :: IO ()
main = do
  result <- try (execParser opts) :: IO (Either SomeException GlobalOptions)
  case result of
    Left err -> do
      hPutStrLn stderr [fmt|Error: #{err}|]
      exitWith (ExitFailure 64)  -- EX_USAGE
    Right opts -> do
      result <- try (runCommand opts) :: IO (Either AppError ())
      case result of
        Left err -> do
          hPutStrLn stderr [fmt|Fatal: #{err}|]
          exitWith (ExitFailure 70)  -- EX_SOFTWARE
        Right () -> exitSuccess
```

**Critical Race Condition** (from FP Complete):
- With concurrency, multiple threads can call `exitWith` simultaneously
- Last one to call wins (non-deterministic exit code)
- **Solution**: Use `bracket` or `finally` to ensure cleanup, then single exit point

```haskell
-- WRONG: Race condition with concurrent exits
main = concurrently
  (exitWith (ExitFailure 41))
  (exitWith (ExitFailure 42))
-- Result: Non-deterministic, could be 41 or 42

-- CORRECT: Single exit point
main = do
  result <- try (runConcurrently ...) :: IO (Either SomeException ())
  case result of
    Left err -> exitWith (ExitFailure 70)
    Right () -> exitSuccess
```

**Error Message Convention**:
- **stderr for human messages** (errors, warnings, spinners, progress)
- **stdout for machine-readable data** (JSON, file lists when piped)
- **TTY detection**: Use `hIsTerminalDevice stdout` to suppress spinners in CI/pipes
- **NO_COLOR support**: Check `lookupEnv "NO_COLOR"` and strip ANSI codes

---

### 1.3 Event-Sourcing Decision Functions (Functional Core)

**Standard Pattern** (from thinkbeforecoding, Eventuous, Wolverine):

```haskell
-- Pure decision function: Command + State -> Events (or Error)
type Decide command state event error
  = command -> state -> Either error [event]

-- Example: Increment command on Counter
data IncrementCommand = Increment { amount :: Int }
data CounterState = CounterState { value :: Int }
data CounterEvent = Incremented { amount :: Int, newValue :: Int }
data CounterError = InvalidAmount | CounterNotFound

decide :: Decide IncrementCommand CounterState CounterEvent CounterError
decide (Increment amount) state
  | amount <= 0 = Left InvalidAmount
  | otherwise = Right [Incremented amount (state.value + amount)]

-- Evolution function: State + Event -> State (deterministic replay)
type Evolve state event = state -> event -> state

evolve :: Evolve CounterState CounterEvent
evolve state (Incremented _ newValue) = state { value = newValue }

-- Command handler: Fetch state, decide, evolve, persist
handleCommand :: CommandHandler
handleCommand cmd entityId = do
  state <- eventStore.loadState entityId
  case decide cmd state of
    Left err -> Task.throw (CommandRejected err)
    Right events -> do
      eventStore.append entityId events
      Task.yield events
```

**Key Principles**:
- **Decide is pure**: No I/O, no side effects, deterministic
- **Evolve is pure**: Replaying events always produces same state
- **Separate concerns**: Decide (business logic) vs. Evolve (state transition)
- **Validation in Decide**: All business rules checked before events generated
- **Events are immutable**: Must contain all fields needed for replay (no external lookups)

**Naming Convention**:
- Command type: `<Action>Command` (e.g., `IncrementCommand`, `CreateCartCommand`)
- Event type: `<Action>Event` or `<Action>` (e.g., `Incremented`, `CartCreated`)
- Decide function: `decide` (unqualified, or `<Entity>.decide` when qualified)
- Evolve function: `evolve` (unqualified, or `<Entity>.evolve` when qualified)
- Error type: `<Entity>Error` (e.g., `CounterError`, `CartError`)

---

### 1.4 CQRS Command/Query Separation

**Standard Pattern** (from Azure, Eventuous, Wolverine):

```haskell
-- Commands: Write side (change state)
data Command
  = CreateCart { customerId :: CustomerId }
  | AddItem { cartId :: CartId, productId :: ProductId, qty :: Int }
  | RemoveItem { cartId :: CartId, productId :: ProductId }

-- Queries: Read side (no state change)
data Query result
  = GetCartSummary CartId (CartSummary -> result)
  | GetStockLevel ProductId (StockLevel -> result)

-- Command handler: Fetch aggregate, decide, persist events
handleCommand :: Command -> Task AppError [Event]
handleCommand cmd = do
  case cmd of
    CreateCart customerId -> do
      cartId <- Uuid.generate
      let events = [CartCreated customerId cartId]
      eventStore.append cartId events
      Task.yield events
    AddItem cartId productId qty -> do
      state <- eventStore.loadState cartId
      case decide (AddItem productId qty) state of
        Left err -> Task.throw err
        Right events -> do
          eventStore.append cartId events
          Task.yield events

-- Query handler: Read from projection (no event store access)
handleQuery :: Query result -> Task AppError result
handleQuery (GetCartSummary cartId callback) = do
  summary <- cartProjection.get cartId
  Task.yield (callback summary)
handleQuery (GetStockLevel productId callback) = do
  level <- stockProjection.get productId
  Task.yield (callback level)

-- Key rule: Queries NEVER call Commands
-- Cross-entity communication: Event -> Integration -> Command
-- Example: When CartItemAdded event fires, Integration calls ReserveStock command
```

**Key Principles**:
- **Commands are imperative**: "CreateCart", "AddItem" (verb phrases)
- **Queries are declarative**: "GetCartSummary", "GetStockLevel" (noun phrases)
- **Commands produce events**: Always return `[Event]` or error
- **Queries read projections**: Never touch event store directly
- **Cross-entity via Integration**: Event → Integration → Command (never Command → Command)

---

### 1.5 CLI UX Edge Cases

**TTY Detection & Output Formatting** (from NeoHaskell design, POSIX standards):

```haskell
import System.IO (hIsTerminalDevice, stdout, stderr)
import System.Environment (lookupEnv)

-- Detect if output is interactive
isInteractive :: IO Bool
isInteractive = do
  isTty <- hIsTerminalDevice stdout
  noColor <- lookupEnv "NO_COLOR"
  pure (isTty && isNothing noColor)

-- Spinner (only in TTY)
showSpinner :: String -> IO ()
showSpinner msg = do
  interactive <- isInteractive
  if interactive
    then putStr [fmt|⠋ #{msg}|]  -- Animated spinner
    else putStrLn msg

-- Success message (with symbol in TTY)
showSuccess :: String -> IO ()
showSuccess msg = do
  interactive <- isInteractive
  if interactive
    then hPutStrLn stderr [fmt|✓ #{msg}|]
    else hPutStrLn stderr msg

-- Error message (always to stderr)
showError :: String -> IO ()
showError msg = hPutStrLn stderr [fmt|Error: #{msg}|]

-- JSON output (always to stdout, never colored)
showJson :: ToJSON a => a -> IO ()
showJson = putStrLn . Json.encode
```

**Error Message Patterns** (from NeoHaskell design):

```haskell
-- Pattern: What went wrong → What to do
-- GOOD: Actionable, specific
"my-counter already exists. Pick a different name, or cd into it."
"Did you mean: neo init my-counter?"  -- Auto-suggest
"Git is required — install it from https://git-scm.com then try again."

-- BAD: Vague, blames user
"Invalid input"
"Something went wrong"
"Usage: neo init [OPTIONS] PROJECT_NAME"  -- Too formal

-- BAD: Jargon without explanation
"Aggregate root validation failed"
"Event stream consistency check failed"
```

**Argument Validation** (from NeoHaskell design):

```haskell
-- Project name validation
validateProjectName :: String -> Either String String
validateProjectName name
  | null name = Left "What should we call it? Try: neo init my-project"
  | not (Char.isLower (head name)) = Left [fmt|Did you mean: neo init #{Text.toLower name}?|]
  | not (all (\c -> Char.isLower c || Char.isDigit c || c == '-') name) = 
      Left "Project name must contain only lowercase letters, digits, and hyphens"
  | Text.length name > 64 = Left "Project name must be 64 characters or less"
  | otherwise = Right name

-- File/directory checks
checkDirectoryExists :: FilePath -> Task AppError ()
checkDirectoryExists path = do
  exists <- Directory.doesDirectoryExist path
  if exists
    then Task.throw (DirectoryAlreadyExists path)
    else Task.yield ()

-- Permission checks
checkWritePermission :: FilePath -> Task AppError ()
checkWritePermission path = do
  result <- try (File.writeText path "") :: IO (Either IOException ())
  case result of
    Left _ -> Task.throw (PermissionDenied path)
    Right () -> File.deleteFile path >> Task.yield ()
```

---

## PART 2: NEOHASKELL PROJECT CONVENTIONS

### 2.1 NeoHaskell Style Guide Application to CLI

**Pipe Operator for Command Chains**:

```haskell
-- CORRECT: Left-to-right command flow
processCommand opts = opts
  |> validateOptions
  |> loadConfig
  |> executeCommand
  |> formatOutput

-- CORRECT: Pipeline with intermediate steps
initProject name = name
  |> validateProjectName
  |> createDirectory
  |> generateFiles
  |> initializeGit
  |> printSuccess
```

**Do-Blocks for Bindings** (even in pure code):

```haskell
-- CORRECT: do-block for pure code
parseArgs args = do
  let opts = parseOptions args
  let cmd = opts.command
  let config = loadConfig opts.configFile
  (opts, cmd, config)

-- CORRECT: do-block in Task (monadic)
handleInit projectName = do
  validated <- validateProjectName projectName
  let dir = createDirectory validated
  gitInit <- initializeGit dir
  Task.yield gitInit
```

**Case Expressions for Pattern Matching**:

```haskell
-- CORRECT: case on constructors
handleCommand cmd = case cmd of
  Init name -> handleInit name
  Run cfg -> handleRun cfg
  Build release -> handleBuild release

-- CORRECT: nested case
processResult result = case result of
  Ok value -> case value of
    Success msg -> printSuccess msg
    Warning msg -> printWarning msg
  Err err -> printError err
```

**If-Then-Else for Bool Conditionals**:

```haskell
-- CORRECT: if-then-else for Bool
showSpinner msg =
  if isInteractive
    then putStr [fmt|⠋ #{msg}|]
    else putStrLn msg

-- CORRECT: Task.when for monadic conditionals
Task.when shouldVerbose do
  Log.info [fmt|Processing #{entityId}|]
```

**String Interpolation with fmt**:

```haskell
-- CORRECT: fmt quasi-quoter
errorMsg = [fmt|Failed to create #{projectName}: #{error}|]
logMsg = [fmt|Initializing git in #{directory}|]

-- WRONG: String concatenation
errorMsg = "Failed to create " <> projectName <> ": " <> error
```

**Result Over Either**:

```haskell
-- CORRECT: Result type
validateName :: String -> Result ValidationError String
validateName name =
  if Text.isEmpty name
    then Err EmptyName
    else Ok name

-- CORRECT: Result helpers
result |> Result.isOk |> shouldBe True
result |> Result.withDefault fallback
```

**Task Over IO**:

```haskell
-- CORRECT: Task with yield
readConfig :: Task ConfigError Config
readConfig = do
  contents <- File.readText "config.json"
  case Json.decode contents of
    Ok config -> Task.yield config
    Err err -> Task.throw (ParseError err)

-- WRONG: IO with pure/return
readConfig :: IO Config
readConfig = do
  contents <- readFile "config.json"
  pure (parseConfig contents)
```

---

### 2.2 NeoHaskell CLI Design Principles (from user-flows.md)

**Principle of Least Astonishment**:
- Use conventions from user's world (JSON config, Git, GitHub, VS Code)
- Command names do what they sound like (`neo run` runs, `neo build` builds)
- No surprising side effects
- Error messages confirm what user tried and say what went wrong

**Principle of Least Effort**:
- One command to create project, one to run it, one to build it
- No flags when there's one obvious default
- No configuration files to write before getting started
- Standard library handles common tasks
- Error messages tell what to do, not just what failed

**Principle of Developer Happiness**:
- Clear documentation with code examples
- Generated example is first teacher (must be understandable)
- Errors never blame user or use jargon
- CLI feels responsive and respectful of time
- Working code on first run

---

### 2.3 NeoHaskell Event Modeling for CLI

**The 4 Patterns** (from AGENTS.md):

```
Pattern 1: State Change
[User Input] → [Command] → [Event]
User intent causes a recorded fact.

Pattern 2: View
[Event] → [Query] → [UI Output]
Facts project into queryable views.

Pattern 3: Automation (Policy)
[Event] → [Integration] → [Command]
A fact triggers automated action.

Pattern 4: External Input
[External System] → [Integration] → [Command]
External trigger causes internal action.
```

**Example: `neo init` Command**

```markdown
# Entity: Project

## State
- projectId: ProjectId
- name: String
- version: String
- dependencies: Map String Version

## Commands → Events

### Command: CreateProject
- **Fields:** { name: String }
- **Validation:**
  - name matches pattern [a-z][a-z0-9-]{0,62}
  - directory doesn't exist
- **Errors:** [InvalidName, DirectoryExists]

### Event: ProjectCreated
- **Fields:** { projectId: ProjectId, name: String, timestamp: UTCTime }
- **Produced by:** CreateProject command

## Integrations

### Inbound: InitCommand
- **Trigger:** User runs `neo init my-project`
- **Action:** Emit CreateProject { name = "my-project" } command

### Outbound: GenerateScaffold
- **Trigger:** ProjectCreated event
- **Action:** Generate files (src/Main.nh, tests/, etc.)

### Outbound: InitializeGit
- **Trigger:** ProjectCreated event
- **Action:** Run `git init` in project directory
```

---

### 2.4 NeoHaskell Testbed Patterns (Hurl Acceptance Tests)

**Command Test Pattern** (from testbed/tests/commands/):

```hurl
# tests/commands/create-cart.hurl
POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
[Asserts]
jsonpath "$.entityId" matches /^[0-9a-f-]{36}$/
jsonpath "$.events[0].type" == "CartCreated"

[Captures]
cart_id: jsonpath "$.entityId"
```

**Query Test Pattern** (from testbed/tests/queries/):

```hurl
# tests/queries/cart-summary.hurl
GET http://localhost:8080/queries/cart-summary?cartId={{cart_id}}

HTTP/1.1 200
[Asserts]
jsonpath "$" isCollection
jsonpath "$[0].cartId" == "{{cart_id}}"
```

**Scenario Test Pattern** (from testbed/tests/scenarios/):

```hurl
# tests/scenarios/create-and-query.hurl
# Step 1: Create cart
POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
[Captures]
cart_id: jsonpath "$.entityId"

# Step 2: Query with retry (eventual consistency)
GET http://localhost:8080/queries/cart-summary?cartId={{cart_id}}
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$[0].cartId" == "{{cart_id}}"
```

**Key Patterns**:
- **Retry for async**: Use `retry: N` + `retry-interval: ms` for integration effects
- **Captures**: `[Captures]` to pass IDs between requests
- **JSONPath filters**: `$[?(@.field == 'value')]` for array element matching
- **Assertions**: `matches`, `isCollection`, `==` for verification

---

## PART 3: NAMING & PACKAGE CONVENTIONS

### 3.1 CLI Package Structure

**Recommended Layout**:

```
cli/
├── design/                    # Design docs (user flows, AGENTS.md)
│   ├── user-flows.md         # Chapter-by-chapter CLI spec
│   ├── AGENTS.md             # AI assistant instructions
│   └── syntax.md             # NeoHaskell syntax spec
├── src/                       # Implementation (when ready)
│   ├── Main.hs               # Entry point
│   ├── Neo/
│   │   ├── CLI/
│   │   │   ├── Parser.hs     # optparse-applicative setup
│   │   │   ├── Commands/
│   │   │   │   ├── Init.hs   # neo init command
│   │   │   │   ├── Run.hs    # neo run command
│   │   │   │   └── Build.hs  # neo build command
│   │   │   ├── Output.hs     # Spinner, colors, formatting
│   │   │   └── Error.hs      # Error messages, exit codes
│   │   └── Project/
│   │       ├── Core.hs       # Project types
│   │       ├── Scaffold.hs   # File generation
│   │       └── Validation.hs # Name/path validation
├── tests/
│   ├── commands/
│   │   ├── init.hurl         # neo init acceptance tests
│   │   ├── run.hurl          # neo run acceptance tests
│   │   └── build.hurl        # neo build acceptance tests
│   └── scenarios/
│       └── full-workflow.hurl # End-to-end: init → run → build
└── scripts/
    └── run-tests.sh          # Auto-start CLI, run Hurl tests
```

### 3.2 Naming Conventions

**Command Types**:
```haskell
data Command
  = Init InitOptions
  | Run RunOptions
  | Build BuildOptions
  | Add AddOptions
  | Remove RemoveOptions
  | Test TestOptions
  | Clean
  | Repl
  | Info
  deriving (Show)

data InitOptions = InitOptions
  { projectName :: String
  }
  deriving (Show)

data RunOptions = RunOptions
  { configPath :: Maybe FilePath
  , verbose :: Bool
  }
  deriving (Show)
```

**Parser Functions**:
```haskell
commandParser :: Parser Command
initParser :: Parser InitOptions
runParser :: Parser RunOptions
globalOptionsParser :: Parser GlobalOptions
```

**Handler Functions**:
```haskell
handleInit :: GlobalOptions -> InitOptions -> Task AppError ()
handleRun :: GlobalOptions -> RunOptions -> Task AppError ()
handleBuild :: GlobalOptions -> BuildOptions -> Task AppError ()
```

**Output Functions**:
```haskell
showSpinner :: String -> Task AppError ()
showSuccess :: String -> Task AppError ()
showError :: String -> Task AppError ()
showJson :: ToJSON a => a -> Task AppError ()
```

**Validation Functions**:
```haskell
validateProjectName :: String -> Result ValidationError String
validateFilePath :: FilePath -> Result ValidationError FilePath
validatePort :: Int -> Result ValidationError Int
```

---

## PART 4: ACCEPTANCE CRITERIA PATTERNS

### 4.1 Command Acceptance Criteria Template

**For `neo init` command**:

```markdown
## Acceptance Criteria: neo init

### Happy Path
- [ ] `neo init my-project` creates directory `my-project/`
- [ ] Generated files include: neo.json, README.md, AGENTS.md, .gitignore, src/, tests/
- [ ] `src/Main.nh` is thin launcher (calls Application.run)
- [ ] `src/App.nh` wires all services, queries, integrations
- [ ] `src/Counter/` example is fully functional
- [ ] `tests/` includes unit tests + Hurl integration tests
- [ ] `git init` is run automatically
- [ ] Output shows spinner during generation, ✓ on success
- [ ] Next steps printed: `cd my-project && neo run`

### Validation
- [ ] Rejects names starting with uppercase: `neo init MyProject` → suggests `my-project`
- [ ] Rejects names with consecutive hyphens: `neo init my--project` → error
- [ ] Rejects names starting/ending with hyphen: `neo init -my-project` → error
- [ ] Rejects names > 64 chars → error
- [ ] Rejects names with special chars: `neo init my_project` → error

### Error Cases
- [ ] No name given: `neo init` → "What should we call it? Try: neo init my-project"
- [ ] Directory exists: `neo init existing-dir` → "existing-dir already exists. Pick a different name, or cd into it."
- [ ] Permission denied: → "Couldn't create my-project — permission denied. Check the parent directory permissions."
- [ ] Git not installed: → "Git is required — install it from https://git-scm.com then try again."
- [ ] Extra arguments: `neo init my-project extra` → "neo init takes one argument — the project name. Try: neo init my-counter"

### Output Format
- [ ] Spinner shows during generation (TTY only)
- [ ] Success message: `✓ my-counter is ready` (TTY only)
- [ ] All output to stderr (stdout stays clean)
- [ ] NO_COLOR env var respected (no ANSI codes)
- [ ] Non-TTY (CI/pipes): plain text, no spinner

### Exit Codes
- [ ] Success: exit 0
- [ ] Usage error (invalid name): exit 64 (EX_USAGE)
- [ ] Directory exists: exit 73 (EX_CANTCREAT)
- [ ] Permission denied: exit 77 (EX_NOPERM)
- [ ] Git not found: exit 69 (EX_UNAVAILABLE)
```

### 4.2 Event-Sourcing Acceptance Criteria Template

**For Command Handler**:

```markdown
## Acceptance Criteria: CreateCart Command Handler

### Happy Path
- [ ] `CreateCart { customerId }` command accepted
- [ ] Generates `CartCreated { cartId, customerId, timestamp }` event
- [ ] Event persisted to event store
- [ ] Response includes `cartId` for subsequent operations
- [ ] Event timestamp is current UTC time

### Validation
- [ ] Rejects if customerId is empty → `CustomerNotFound` error
- [ ] Rejects if customerId doesn't exist in system → `CustomerNotFound` error
- [ ] Returns error (not exception) for validation failures

### Idempotency
- [ ] Same command with same cartId produces same event
- [ ] Replaying events produces same state
- [ ] No side effects during decide phase (pure function)

### Concurrency
- [ ] Multiple concurrent CreateCart commands produce different cartIds
- [ ] Event store handles concurrent appends correctly
- [ ] No race conditions in state transitions

### Integration
- [ ] CartCreated event triggers ReserveStock integration
- [ ] Integration emits ReserveStock command for each item
- [ ] Integration failures don't prevent event persistence
```

### 4.3 CLI UX Acceptance Criteria Template

**For Error Messages**:

```markdown
## Acceptance Criteria: Error Messages

### Format
- [ ] Error messages start with "Error:" or specific error type
- [ ] Message is actionable (tells user what to do)
- [ ] Message is specific (not generic "something went wrong")
- [ ] Message uses user's terminology (not jargon)
- [ ] Message suggests fix when possible

### Examples
- [ ] ✓ "my-counter already exists. Pick a different name, or cd into it."
- [ ] ✓ "Did you mean: neo init my-counter?"
- [ ] ✓ "Git is required — install it from https://git-scm.com then try again."
- [ ] ✗ "Invalid input"
- [ ] ✗ "Something went wrong"
- [ ] ✗ "Aggregate root validation failed"

### Output
- [ ] All errors to stderr (not stdout)
- [ ] Exit code matches error type (64 for usage, 70 for software, etc.)
- [ ] NO_COLOR respected (no ANSI codes if set)
- [ ] TTY detection respected (no spinners in CI)
```

---

## PART 5: EDGE CASES & GOTCHAS

### 5.1 Process Exit Edge Cases

**Race Condition with Concurrent Exits**:
```haskell
-- WRONG: Multiple threads calling exitWith
main = concurrently
  (exitWith (ExitFailure 41))
  (exitWith (ExitFailure 42))
-- Result: Non-deterministic, could be 41 or 42

-- CORRECT: Single exit point
main = do
  result <- try (runConcurrently ...) :: IO (Either SomeException ())
  case result of
    Left err -> exitWith (ExitFailure 70)
    Right () -> exitSuccess
```

**Cleanup Before Exit**:
```haskell
-- WRONG: Exit without cleanup
main = do
  handle <- openFile "temp.txt" WriteMode
  exitWith ExitSuccess  -- File handle not closed!

-- CORRECT: Use bracket for cleanup
main = do
  bracket
    (openFile "temp.txt" WriteMode)
    hClose
    (\handle -> do
      hPutStrLn handle "data"
      exitSuccess)
```

**Buffering Issues**:
```haskell
-- WRONG: Output lost if not flushed before exit
main = do
  putStr "Processing..."
  exitSuccess  -- "Processing..." might not appear

-- CORRECT: Flush before exit
main = do
  putStr "Processing..."
  hFlush stdout
  exitSuccess
```

### 5.2 TTY Detection Edge Cases

**Piped Output**:
```bash
# TTY detection should return False
$ neo init my-project | tee output.log
# Expected: No spinner, plain text output

# Piped to another command
$ neo init my-project | grep "ready"
# Expected: No spinner, plain text output
```

**CI Environment**:
```bash
# GitHub Actions, GitLab CI, etc. are not TTY
$ neo init my-project
# Expected: No spinner, plain text output
# Exit code still correct
```

**NO_COLOR Environment Variable**:
```bash
# Should strip all ANSI codes
$ NO_COLOR=1 neo init my-project
# Expected: No colors, no ANSI codes
# Spinner still suppressed
```

### 5.3 File System Edge Cases

**Directory Already Exists**:
```bash
$ neo init my-project
$ neo init my-project  # Second time
# Expected: "my-project already exists. Pick a different name, or cd into it."
# Exit code: 73 (EX_CANTCREAT)
```

**Permission Denied**:
```bash
$ mkdir /root/my-project  # As non-root
# Expected: "Couldn't create /root/my-project — permission denied. Check the parent directory permissions."
# Exit code: 77 (EX_NOPERM)
```

**Path Traversal**:
```bash
$ neo init ../../../etc/passwd
# Expected: Validation error (path contains ..)
# Exit code: 64 (EX_USAGE)
```

### 5.4 Event-Sourcing Edge Cases

**Concurrent Command Execution**:
```haskell
-- WRONG: Race condition in decide
decide cmd state = do
  currentValue <- readMVar stateVar  -- Reads mutable state!
  let newValue = currentValue + 1
  [Event newValue]

-- CORRECT: Pure decide, no mutable state
decide (Increment amount) state =
  [Incremented (state.value + amount)]
```

**Event Replay Consistency**:
```haskell
-- WRONG: Decide depends on external state
decide cmd state = do
  currentTime <- getCurrentTime  -- Non-deterministic!
  [Event currentTime]

-- CORRECT: Decide is pure, time passed as parameter
decide cmd state currentTime =
  [Event currentTime]
```

**Missing Event Fields**:
```haskell
-- WRONG: Event doesn't contain all fields needed for replay
data CartCreated = CartCreated { cartId :: CartId }
-- Problem: Can't replay without customerId

-- CORRECT: Event contains all fields
data CartCreated = CartCreated
  { cartId :: CartId
  , customerId :: CustomerId
  , timestamp :: UTCTime
  }
```

---

## PART 6: INTEGRATION WITH NEOHASKELL TESTBED

### 6.1 Testbed as Reference Implementation

The NeoHaskell testbed (`testbed/src/`) demonstrates:

1. **App composition** (`App.hs`):
   - `Application.new |> Application.withConfig |> Application.withService |> Application.withQuery`
   - Config-dependent wiring (PostgreSQL, file uploads)
   - Service registration pattern

2. **Entity + Events** (`Cart/Core.hs`):
   - Entity definition with state
   - Event ADT with one branch per event module
   - Update function for replay

3. **Command handlers** (`Cart/Commands/CreateCart.hs`):
   - Decide function (pure, testable)
   - Validation logic
   - Error handling

4. **Query projections** (`Cart/Queries/CartSummary.hs`):
   - Read model definition
   - Projection from events

5. **Integrations** (`Cart/Integrations.hs`):
   - Outbound: Event → Effect/Command
   - Inbound: Timer → Command

6. **Acceptance tests** (`tests/`):
   - Command tests (Hurl)
   - Query tests (Hurl)
   - Scenario tests (multi-step workflows)

### 6.2 CLI Scaffold Should Follow Same Patterns

**Proposed CLI Implementation Structure**:

```haskell
-- cli/src/Neo/CLI/Parser.hs
-- Follows optparse-applicative pattern from external research

-- cli/src/Neo/CLI/Commands/Init.hs
-- Follows decide/evolve pattern from testbed

-- cli/src/Neo/Project/Scaffold.hs
-- Generates files following testbed structure

-- cli/tests/commands/init.hurl
-- Follows Hurl pattern from testbed
```

---

## PART 7: SUMMARY TABLE

| Aspect | External Best Practice | NeoHaskell Convention | CLI Scaffold Application |
|--------|------------------------|----------------------|--------------------------|
| **CLI Parsing** | optparse-applicative with subcommands | N/A (new) | Use applicative composition, global options before subcommand |
| **Exit Codes** | POSIX sysexits.h (64, 70, 73, 77) | N/A (new) | Map validation errors to specific exit codes |
| **Error Messages** | Actionable, specific, suggest fix | Least Astonishment principle | "What went wrong → What to do" format |
| **Output** | stderr for human, stdout for machine | Least Effort principle | TTY detection, NO_COLOR support |
| **Decision Functions** | Pure, deterministic, testable | N/A (new) | Separate decide (business logic) from evolve (state transition) |
| **Command/Query** | CQRS separation, no cross-command calls | N/A (new) | Commands emit events, queries read projections |
| **Naming** | `<Action>Command`, `<Action>Event`, `decide`, `evolve` | Qualified imports, descriptive names | `InitCommand`, `ProjectCreated`, `handleInit` |
| **Testing** | Hurl for blackbox acceptance tests | Testbed pattern | Command tests, query tests, scenario tests |
| **Pipes** | N/A | `\|>` operator over `$` | Use pipes for command chains |
| **Do-Blocks** | N/A | `do let` for all bindings | Use do-blocks even in pure code |
| **Case Expressions** | N/A | Case for constructors, if-then-else for Bool | Pattern match on command types with case |
| **String Interpolation** | N/A | `[fmt\|...\|]` with `#{}` | Use fmt for all error/log messages |
| **Result/Task** | N/A | `Result` over `Either`, `Task` over `IO` | Return `Task AppError ()` from handlers |

---

## REFERENCES

### External Research
- **optparse-applicative**: FP Complete, Medium (Stephan Schiffels), Discourse
- **Exit Codes**: POSIX sysexits.h, FP Complete (Michael Snoyman), Stack Overflow
- **Event-Sourcing**: thinkbeforecoding, Eventuous, Wolverine, Azure Architecture Center
- **CQRS**: Azure, Eventuous, Wolverine
- **CLI UX**: NeoHaskell design docs, POSIX standards

### NeoHaskell Project
- **Style Guide**: `.opencode/skills/neohaskell-style-guide/`
- **User Flows**: `cli/design/user-flows.md`
- **AGENTS.md**: `cli/design/AGENTS.md`
- **Testbed**: `testbed/src/`, `testbed/tests/`
- **AGENTS.md (root)**: `AGENTS.md`

---

**Document Version**: 1.0  
**Last Updated**: March 12, 2026  
**Status**: Research Complete, Ready for Implementation Planning
