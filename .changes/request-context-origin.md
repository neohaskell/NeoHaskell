---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Web commands can read the socket client address and User-Agent through `RequestContext.origin`. `Text.fromBytesIfValid` decodes valid UTF-8 bytes without throwing. Apps that construct `WebTransport` or `RequestContext` records directly must add the new fields. The trusted proxy set stays empty by default.

## Migration

Calls to `WebTransport.server` need no change. `emptyContext`, `anonymousContext`, `authenticatedContext`, and `trustedContext` already set `origin = Nothing`. If your app constructs either record directly, add the corresponding field below. Import `Service.Transport.Web` as `WebTransport` and `Service.Auth` as `Auth` to match the examples. Import `Set qualified as Set` if your app does not already import `Set`.

```diff
  , WebTransport.readinessProbe = Nothing
+ , WebTransport.trustedProxyAddresses = Set.empty
```

```diff
  , Auth.trustedBypass = False
+ , Auth.origin = Nothing
```

`WebTransport.server` leaves the trusted proxy set empty. Trust a proxy only when your deployment requires it. `trustedProxyAddressFromText :: Text -> Result Text TrustedProxyAddress` accepts an exact IPv4 or IPv6 literal. Handle `Result.Err` before adding a parsed address to the set. Treat request-origin values as untrusted. Do not store them in events or logs without a reviewed privacy and retention purpose.

### Verify

From the generated app's root, run:

```sh
neo build --ci
neo test --ci
```

Both commands must succeed. `neo test --ci` runs the app's Cabal tests and discovered Hurl scenarios.

## Agent prompt

```text
Update this NeoHaskell app for the request-origin changes in NeoHaskell 0.18.0.

Find every app-authored full WebTransport and RequestContext record constructor. Leave WebTransport.server and the emptyContext, anonymousContext, authenticatedContext, and trustedContext helpers unchanged. Add trustedProxyAddresses = Set.empty to each direct WebTransport record. Add origin = Nothing to each internal RequestContext record.

For example, a WebTransport record that ends with readinessProbe = Nothing gains trustedProxyAddresses = Set.empty. An internal RequestContext that ends with trustedBypass = False gains origin = Nothing. Use the field qualification that matches the app's imports.

Import Set qualified as Set if the app does not already import Set.

Do not enable forwarded addresses by default. Verify that trustedProxyAddressFromText :: Text -> Result Text TrustedProxyAddress accepts exact IPv4 or IPv6 literals. If the app's deployment explicitly requires a trusted proxy, handle Result.Err. Add only a parsed Result.Ok address to the trusted set. Ask the app owner to settle an unclear proxy policy. Treat request-origin values as untrusted. Do not store them in events or logs without a reviewed purpose.

From the app root, run neo build --ci and neo test --ci. Both commands must pass. The test command runs the app's Cabal tests and discovered Hurl scenarios. Report changed files, command results, remaining direct record constructors, and any proxy policy that needs app-owner input.
```
