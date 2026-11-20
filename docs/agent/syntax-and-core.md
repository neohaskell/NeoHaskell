# Syntax and Core

NeoHaskell application modules use Haskell with the generated package's language extensions and the `Core` facade. `Core` exports common types, pipeline operators, decisions, and framework classes.

## A complete module

The data shape here is an `Int` input and a `Bool` check. This module has no application wiring or external effects.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module AgentGuidance.Syntax where

import Core

increment :: Int -> Int
increment value = value + 1

checkedValue :: Bool
checkedValue = increment 2 == 3
```

`Core` re-exports the numeric and equality operations used here.

## Application conventions

Use `import Core` for the common vocabulary. Import a type explicitly and its operations qualified when a module owns the operations. For a command, import `Core`, `Decider qualified`, and your domain types.

Use `value |> operation` for a transformation pipeline. Record fields use `record.field`. Visible type application uses `@Type`. Generated projects enable `BlockArguments`, `OverloadedRecordDot`, `OverloadedStrings`, `TypeApplications`, and `NoImplicitPrelude`. A standalone module outside a generated project needs its own extensions.

Use `case` to inspect constructors and `if` for Boolean conditions. Define inputs by name. Prefer `Result error value` for a pure result and `Task error value` for effects. `Task.yield` constructs a successful task. A task runs only when a caller executes it.

`Decision event` describes acceptance, rejection, and supported decision capabilities. It is not a network effect.
