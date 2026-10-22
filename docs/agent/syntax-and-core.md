# Syntax and Core

NeoHaskell application modules use Haskell with the generated package's language extensions and the `Core` facade. `Core` exports common types, pipeline operators, decisions, and framework classes. See Core in `core/core/Core.hs` and the consumer Cabal template in `neo/assets/templates/project.cabal.j2`.

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

`Core` re-exports the numeric and equality operations used here through Basics in `core/core/Basics.hs`. The module is suitable for a compiler check against `nhcore`.

## Application conventions

Use `import Core` for the common vocabulary. Import a type explicitly and its operations qualified when a module owns the operations. The starter command in `neo/starter/src/Starter/Counter/Commands/IncrementCounter.hs` shows `Core`, `Decider qualified`, and explicit domain imports.

Use `value |> operation` for a transformation pipeline. Record fields use `record.field`. Visible type application uses `@Type`. `BlockArguments`, `OverloadedRecordDot`, `OverloadedStrings`, `TypeApplications`, and `NoImplicitPrelude` come from the generated extension list in `neo/assets/templates/project.cabal.j2`. A standalone module outside that package needs its own extensions.

Use `case` to inspect constructors and `if` for Boolean conditions. Define inputs by name. Prefer `Result error value` for a pure result and `Task error value` for effects. `Task.yield` constructs a successful task. A task runs only when a caller executes it. See Result in `core/core/Result.hs`, Task in `core/core/Task.hs`, and the starter reducer in `neo/starter/src/Starter/Counter/Entity.hs`.

`Decision event` describes acceptance, rejection, and supported decision capabilities. It is not a network effect. See Decider in `core/service/Decider.hs` and [commands](commands.md).
