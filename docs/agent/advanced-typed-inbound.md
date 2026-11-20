# Typed inbound workers

Use `Integration.TypedInboundConfig` when one timer, webhook, or other inbound worker can emit different command types. Give `workerImpl` a signature of `Config -> Integration.TypedInbound '[ '(FirstCommand, FirstEntity), '(SecondCommand, SecondEntity)]`. Build it with `Integration.typedInbound` and call the supplied `emit` callback with each command value. The compiler rejects an emitted command missing from the list and a target that differs from `EntityOf Command`. Each command needs `Json.ToJSON` and `NameOf`.

Declare a handler type in the worker module and put `inboundIntegration ''Handler` after `workerImpl`. Declare it in an Event Model inbound Slice with `inboundIntegration @Handler`, followed by each `triggersCommand @Command` and `onTargetEntity @Entity`. The model registers the worker. Startup loads configuration before constructing a config-dependent handler and checks that every declared command has a receiving service before starting inbound workers. The worker keeps its interval, request verification, secrets, and input-to-command mapping in its own module.

Existing direct `Application` programs can keep `Integration.inbound`, `Timer.every`, and `Application.withInbound`. Those workers do not declare a complete output set. `InboundOutputsOf Handler` is the typed output relation for typed handlers; there is no `CommandOf Handler` API to migrate. A model-backed executable uses the typed handler Slice and needs no separate application registration.

Run `neo build` to check the declared outputs and `neo test --ci` to run the project's tests. See [Testing](testing.md) for the project test workflow.
