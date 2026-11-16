---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Web commands can read the socket client address and User-Agent through `RequestContext.origin`. Applications that build a `WebTransport` record directly must add the new trusted proxy field. Apps using `WebTransport.server` need no change.

## Migration

If your app constructs every `WebTransport` field directly, add `trustedProxyAddresses = Set.empty` to keep the socket address as the client address. To accept `X-Forwarded-For`, configure exact proxy addresses with `trustedProxyAddressFromText`. The set stays empty by default.

If your code constructs a complete `RequestContext` record directly, add `origin = Nothing` for an internal context. The `emptyContext`, `anonymousContext`, `authenticatedContext`, and `trustedContext` helpers already set it.

### Verify

Build the app and run the focused Web-origin tests and strict testbed suite:

```sh
nix develop --command .agents/skills/tdd/scripts/test-match 'Web request origin' nhcore-test
nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

## Agent prompt

```text
Update this NeoHaskell app for the request-origin API. Find any complete WebTransport record construction and add trustedProxyAddresses. Use Set.empty to preserve the default behavior. Configure only exact proxy IP addresses with trustedProxyAddressFromText when this app has an explicit trusted proxy. Find any complete RequestContext construction and add origin = Nothing for internal contexts. Keep raw client addresses and User-Agent values out of events and logs unless the app has a reviewed privacy and retention purpose. Build the app, run the Web request-origin Hspec selection, and run the strict testbed suite. Report changed files, commands, results, and any unresolved record construction.
```
