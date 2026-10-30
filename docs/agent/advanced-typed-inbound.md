# Typed inbound workers

Use `Integration.TypedInboundConfig` when one timer, webhook, or other inbound worker can emit different command types. Give `workerImpl` a signature of `Config -> Integration.TypedInbound '[ '(FirstCommand, FirstEntity), '(SecondCommand, SecondEntity)]`. Build it with `Integration.typedInbound` and call the supplied `emit` callback with each command value. The compiler rejects an emitted command missing from the list and a target that differs from `EntityOf Command`. Each command needs `Json.ToJSON` and `NameOf`.

Declare a handler type in the worker module and put `inboundIntegration ''Handler` after `workerImpl`. Register it with `Application.withInboundHandler @Handler`. Application loads its configuration before it constructs a config-dependent handler. At startup, it checks that every declared command has a receiving service before starting inbound workers. The worker keeps its interval, request verification, secrets, and input-to-command mapping in its own module.

Existing single-command workers can keep `Integration.inbound`, `Timer.every`, and `Application.withInbound`. Those workers do not declare a complete output set. `InboundOutputsOf Handler` is the typed output relation for new handlers; there is no `CommandOf Handler` API to migrate. An Event Model Slice can describe a typed handler and its command targets, but the model does not register the worker. Keep `Application.withInboundHandler @Handler` until Event Model runtime wiring is available.

Run `neo build` to check the declared outputs and `neo test --ci` to run the project's tests. See [Testing](testing.md) for the project test workflow.
