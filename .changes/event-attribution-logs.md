---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Events created by a command now record the verified user and a correlation ID. When an integration runs another command, its events retain that attribution and identify the event that caused them. JSON logs carry the same IDs through commands and integrations. Scheduled commands have a correlation ID without an invented user. Existing events stay unchanged. Applications using `Application.withService` and built-in transports need no source changes. Stored user attribution does not grant authorization.

## Migration

This affects applications that construct a custom `ServiceRunner` for `Application.withServiceRunner` and return command handlers in the third result of `getEndpointsByTransport`. That map now contains `DispatchHandler` values. Each value accepts `Maybe EventMetadata` before the existing request context, request bytes, and response callback. The first two result maps stay the same. A third map that is always empty needs no edit.

If a custom handler does not start another command, wrap its existing `EndpointHandler` as `\_sourceEvent requestContext body respond -> existingHandler requestContext body respond`. If it delegates to another dispatch handler, pass `sourceEvent` through. Preserve the existing request context for authorization. Event attribution is not permission to act as that user.

### Verify

Build the application and run its command and integration tests. Execute a command and inspect its saved event. Check that `relatedUserSub` is the verified user and `correlationId` is the server request ID. For a command started by an event, check that `causationId` is the immediate source event ID. An absent source should not produce a `causationId`.

## Agent prompt

```text
Update this NeoHaskell application only if it constructs a custom ServiceRunner for Application.withServiceRunner. In each getEndpointsByTransport result, inspect the third map of command dispatch handlers. Each handler now takes Maybe EventMetadata before its existing RequestContext, request bytes, and response callback. Add that leading parameter to custom handlers. If the handler does not create a command or delegate to another dispatch handler, it may ignore the source event. If it delegates to a dispatch handler, pass the source event through. Keep the verified RequestContext as the source of authorization; saved event attribution never grants permission. Do not change ordinary Service.command registrations or Application.withService wiring. Build the app and run its command and integration tests. Run a command and inspect saved event metadata. Check the verified user and request correlation, and for a command started by an event check causationId against the immediate source event ID. Report changed files, test results, and handlers whose event-source behavior is unclear.
```
