---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Apps that call `Service.CommandExecutor.execute` directly must pass execution dependencies and a command source. Command failures now carry a typed reason and a diagnostic message. Commands sent through a NeoHaskell service endpoint keep their existing response shape.

## Migration

Replace the six positional arguments with one dependency record, one source, and the command. Use `FromRequest requestContext` for a direct command. Use `FromEvent requestContext sourceEventMetadata` when an event caused the command. The event metadata supplies attribution; `requestContext` still supplies the authority used for access checks.

```haskell
let dependencies = CommandExecutor.ExecutionDependencies
	{ eventStore = store
	, entityFetcher = fetcher
	, entityName
	}
let source = CommandExecutor.FromRequest requestContext
result <- CommandExecutor.execute dependencies source command
```

`execute` now returns `Task CommandExecutionFailure ExecutionResult`. Handle a failure constructor when your app needs its category. If the surrounding function still returns `Task Text`, convert the error at that boundary.

```haskell
import Service.CommandExecutor.CommandExecutionFailure qualified as CommandExecutionFailure

result <- CommandExecutor.execute dependencies source command
	|> Task.mapError CommandExecutionFailure.message
```

The former `CommandExecutor event` record was unused and has been removed. Remove any direct references to it.

### Verify

Run `cabal build` and the tests that call `execute`. Check that direct and event-triggered commands keep their actor and correlation metadata, and that rejected commands still return their existing response JSON.

## Agent prompt

```text
Migrate direct Service.CommandExecutor.execute calls to the three-argument API. Replace execute store fetcher entityName maybeSource requestContext command with an ExecutionDependencies record containing eventStore, entityFetcher, and entityName, then pass FromRequest requestContext when maybeSource is Nothing or FromEvent requestContext metadata when it is Just metadata. Keep the command last. The return type is Task CommandExecutionFailure ExecutionResult. Inspect failure constructors where the caller needs a category. At a Task Text boundary, use Task.mapError CommandExecutionFailure.message. Remove references to the unused CommandExecutor event record. Preserve authorization from RequestContext, event attribution, existing response JSON, and per-call store and fetcher substitutions. Run cabal build and the affected tests. Report changed files, test results, and any call that needs a manual migration choice.
```
