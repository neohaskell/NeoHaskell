---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Events created by a command now record the verified user and a correlation ID. When an integration runs another command, its events retain that attribution and identify the event that caused them. JSON logs carry the same IDs through commands and integrations. Scheduled commands have a correlation ID without an invented user. Existing events stay unchanged.

## Migration

Applications built with `Application` and the built-in transports need no code changes. If your code calls `CommandExecutor.execute` directly, pass `Nothing` before the request context for a root command. Pass `Just sourceEvent.metadata` for a command triggered by an event.

If your code calls `ServiceRunner.getEndpointsByTransport` directly, its third result now contains `DispatchHandler` values. Each handler takes `Maybe EventMetadata` before the existing request context, command bytes, and response callback. Update any explicit map type and pass `Nothing` for autonomous work or `Just sourceEvent.metadata` for an event callback. Stored user attribution does not grant authorization.

### Verify

Build the application and run its command and integration tests. Execute a command and read its saved event. Check that `relatedUserSub` is the verified user and `correlationId` is the server request ID. For an event callback, check that `causationId` is the immediate source event ID.

## Agent prompt

```text
Update this NeoHaskell application for event attribution. Find direct calls to CommandExecutor.execute. Add Nothing before the request context for root commands, or Just sourceEvent.metadata for commands triggered by an event. Find direct calls to ServiceRunner.getEndpointsByTransport. Change explicit types for its third result from Map Text EndpointHandler to Map Text DispatchHandler. Pass Nothing or Just sourceEvent.metadata to each dispatch handler before its existing arguments. Keep verified request claims and stored event attribution separate. Do not change application authorization rules. Build the application, run its command and integration tests, execute a command, and read its saved events. Report the changed files, test results, and any calls whose event source is unclear.
```
