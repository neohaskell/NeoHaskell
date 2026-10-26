---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

The migration guidance for command event attribution now identifies the applications that need an edit. Applications using `Application.withService` and built-in transports need no source changes. Applications that construct a custom `ServiceRunner` for `Application.withServiceRunner` must update command handlers in the third result of `getEndpointsByTransport` to accept `Maybe EventMetadata` before the request context, request bytes, and response callback. The first two result maps are unchanged, and an empty third map needs no edit. This clarifies the already released 0.11.0 change and introduces no additional supported API break.

For a handler that does not start another command, wrap its existing `EndpointHandler` as `\_sourceEvent requestContext body respond -> existingHandler requestContext body respond`. A handler that delegates to another dispatch handler must pass `sourceEvent` through. Keep the verified request context as the source of authorization. Stored event attribution does not grant permission to act as its user.

Build the application and run its command and integration tests. Execute a command and inspect its saved event. Check that `relatedUserSub` is the verified user and `correlationId` is the server request ID. For a command started by an event, check that `causationId` is the immediate source event ID. An absent source should not produce a `causationId`.
