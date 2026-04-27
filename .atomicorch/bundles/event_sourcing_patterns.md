---
id: event_sourcing_patterns
target_max_tokens: 6000
---

# NeoHaskell Event Sourcing Patterns

NeoHaskell uses an event-sourcing architecture built on nhcore primitives. All state changes are expressed as Commands that produce Events, and Entities are reconstructed by replaying those events.

## New Command Pattern

### Step 1: Define the command data type

```haskell
module Service.Command.CreateUser where

import Core.Json qualified as Json
import Core.Command qualified as Command

data CreateUser = CreateUser
  { userId :: UserId
  , email :: Email
  , name :: Text
  }
  deriving (Show, Eq, Generic)
```

### Step 2: Add JSON instances

```haskell
instance Json.FromJson CreateUser
instance Json.ToJson CreateUser
```

Both instances are derived via `Generic` — do not write them manually unless you need custom field mapping.

### Step 3: Register with the command macro

```haskell
Command.declareCommand ''CreateUser
```

This TH macro generates the `CommandOf` type instance and registers the command in the dispatch table. It must appear after the data type and Json instances, in the same file.

### Step 4: Write the decide function

The `decide` function takes the current entity state and the command, returning a list of events or an error:

```haskell
decide :: UserState -> CreateUser -> Result CreateUserError (Array UserEvent)
decide state cmd = do
  let validated = cmd |> validateCreateUser
  case validated of
    Result.Err err -> Result.err err
    Result.Ok input ->
      Result.ok [UserCreated { userId = input.userId, email = input.email, name = input.name }]
```

The `decide` function must be pure. No Task, no IO.

### Complete Command Example

```haskell
module Service.Command.CreateUser
  ( CreateUser (..)
  , decide
  ) where

import Core.Command qualified as Command
import Core.Json qualified as Json
import Service.Entity.User qualified as User

data CreateUser = CreateUser
  { userId :: UserId
  , email :: Email
  , name :: Text
  }
  deriving (Show, Eq, Generic)

instance Json.FromJson CreateUser
instance Json.ToJson CreateUser

Command.declareCommand ''CreateUser

decide :: User.State -> CreateUser -> Result User.CreateError (Array User.Event)
decide state cmd = case User.validateCreate state cmd of
  Result.Err err -> Result.err err
  Result.Ok _ ->
    Result.ok
      [ User.Created
          { userId = cmd.userId
          , email = cmd.email
          , name = cmd.name
          }
      ]
```

---

## New Entity Pattern

### Step 1: Define the entity data type

```haskell
module Service.Entity.User where

data User = User
  { userId :: UserId
  , email :: Email
  , name :: Text
  , status :: UserStatus
  }
  deriving (Show, Eq, Generic)
```

### Step 2: Define event types

All events for an entity live in the same module:

```haskell
data Event
  = Created { userId :: UserId, email :: Email, name :: Text }
  | EmailChanged { userId :: UserId, newEmail :: Email }
  | Deactivated { userId :: UserId }
  deriving (Show, Eq, Generic)

instance Json.FromJson Event
instance Json.ToJson Event
```

### Step 3: Define EntityOf and EventOf type instances

```haskell
type instance EntityOf User = User
type instance EventOf User = Event
```

These instances connect the entity type to its event type for the eventstore machinery.

### Step 4: Write the apply function

`apply` folds an event into entity state:

```haskell
type State = Option User

apply :: State -> Event -> State
apply _ (Created { userId, email, name }) =
  Option.some (User { userId, email, name, status = Active })
apply state (EmailChanged { newEmail }) =
  state |> Option.map (\user -> user { email = newEmail })
apply state (Deactivated _) =
  state |> Option.map (\user -> user { status = Inactive })
```

### Complete Entity Example

```haskell
module Service.Entity.User
  ( User (..)
  , State
  , Event (..)
  , apply
  , initialState
  ) where

import Core.Json qualified as Json
import Core.Entity qualified as Entity

data User = User
  { userId :: UserId
  , email :: Email
  , name :: Text
  , status :: UserStatus
  }
  deriving (Show, Eq, Generic)

data Event
  = Created { userId :: UserId, email :: Email, name :: Text }
  | EmailChanged { userId :: UserId, newEmail :: Email }
  | Deactivated { userId :: UserId }
  deriving (Show, Eq, Generic)

instance Json.FromJson Event
instance Json.ToJson Event

type instance Entity.EntityOf User = User
type instance Entity.EventOf User = Event

type State = Option User

initialState :: State
initialState = Option.none

apply :: State -> Event -> State
apply _ (Created { userId, email, name }) =
  Option.some (User { userId, email, name, status = Active })
apply state (EmailChanged { newEmail }) =
  state |> Option.map (\user -> user { email = newEmail })
apply state (Deactivated _) =
  state |> Option.map (\user -> user { status = Inactive })
```

---

## Common Pitfalls

### Pitfall 1: Orphan instances

Never define `Json.FromJson` or `Json.ToJson` instances for a type in a module other than where the type is defined. This creates orphan instances that cause compile errors when two modules are imported together.

```haskell
-- ANTI: orphan instance in Service.Command.CreateUser
instance Json.FromJson UserId  -- UserId is defined elsewhere!
```

**Fix:** Add the instance to the module where `UserId` is defined, or use `newtype` wrapping.

### Pitfall 2: Template Haskell staging

`Command.declareCommand ''CreateUser` must appear **after** the data type definition and its Json instances in the source file. TH splices cannot reference names that appear later in the same file.

```haskell
-- ANTI: TH splice before Json instances
Command.declareCommand ''CreateUser  -- WRONG: Json instances not yet defined

instance Json.FromJson CreateUser
instance Json.ToJson CreateUser
```

**Fix:** Always order: data type → Json instances → TH splice.

### Pitfall 3: Missing cabal entries

Every new module must be added to `exposed-modules` (for library modules) or `other-modules` (for test modules) in the `.cabal` file. GHC will silently ignore missing modules during build but fail at link time.

```cabal
-- In the library stanza, add:
exposed-modules:
  Service.Command.CreateUser
  Service.Entity.User
```

### Pitfall 4: NoImplicitPrelude

All modules must include the `NoImplicitPrelude` extension (set project-wide in `ghc-options` or `default-extensions`). Never import `Prelude` explicitly — import nhcore modules instead.

```haskell
-- ANTI: explicit Prelude import
import Prelude (map, filter, show)
```

---

## Eventstore API Reference

```haskell
-- Persist events
EventStore.append
  :: EntityId
  -> ExpectedVersion
  -> Array Event
  -> Task EventStoreError Unit

-- Replay entity state
EventStore.replay
  :: EntityId
  -> (State -> Event -> State)
  -> State
  -> Task EventStoreError State

-- Load single event stream
EventStore.load
  :: EntityId
  -> Task EventStoreError (Array Event)
```

All eventstore operations return `Task EventStoreError value` — never `IO`.
