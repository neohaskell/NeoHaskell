---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

You can now choose which network address and port your web server listens on. Add `Application.withHost "127.0.0.1"` to accept connections from the same machine only, and `Application.withPort 9090` to change the port. `host` also accepts `"*"` (all addresses, IPv4 and IPv6), `"*4"`, `"*6"` or a literal address.

Loopback (`"127.0.0.1"`) is the right choice when your reverse proxy runs on the same machine or in the same container as the app. If the proxy runs somewhere else (its own container with its own network, another host), it cannot reach a loopback listener; keep the default bind and restrict access with your firewall or network policy instead.

A mistake in these settings fails at startup with a clear message instead of starting something unintended. The host must be a bare address or hostname (an empty host, a URL like `"http://127.0.0.1"`, a `host:port` pair or whitespace is rejected with `Application.withHost: ...`), and the port must be between 1 and 65535 (`0` and out-of-range values are rejected with `Application.withPort: ...`). If the port is already in use or the address does not belong to this machine, the app stops with `WebTransport could not bind <host>:<port>: ...`.

Your app behaves exactly as before unless you opt in: it still listens on all IPv4 addresses on port 8080. To also accept IPv6 connections, set `Application.withHost "*"`.

This is marked breaking for one narrow case. `WebTransport` and `Application` each gained fields, so code that builds either record by naming every field must add the new ones. Apps that use `WebTransport.server` or `Application.new` are not affected and need no change.

## Migration

Search your code for a `WebTransport { ... }` or `Application { ... }` record written with every field. Most apps have none and can stop here.

- In a `WebTransport { ... }` record, add `host = "*4"` to keep the old behaviour, or `host = "127.0.0.1"` to accept local connections only.
- In an `Application { ... }` record, add `bindHost = Nothing` and `bindPort = Nothing`.

Prefer `WebTransport.server { port = 9090 }` or `Application.withHost` and `Application.withPort` over naming every field.

### Verify

Build your app, then start it and request `/health` on the address you chose. To check the framework change itself, run `./dev test "Service.Transport.Web.Bind" nhcore-test-service`. All 27 examples pass, including one that starts the server on `127.0.0.1`, fetches its health endpoint and confirms the port is still free on the machine's other addresses, and one that shows a port already in use is reported as `could not bind`.

## Agent prompt

```text
My NeoHaskell app fails to compile after upgrading nhcore. `WebTransport` gained a `host :: Text` field and `Application` gained `bindHost :: Maybe Text` and `bindPort :: Maybe Int`. Find every place that builds a `WebTransport { ... }` or `Application { ... }` record by naming all fields (search for `WebTransport {`, `WebTransport$` followed by `{`, and `Application {`). In each `WebTransport` record add `host = "*4"` so it keeps listening on all IPv4 addresses exactly as before. In each `Application` record add `bindHost = Nothing` and `bindPort = Nothing`. Do not change anything that uses `WebTransport.server` or `Application.new`. Keep the port and every other field exactly as they were. Then build the project and run its tests. Report which files you edited and whether the build and tests pass. If my app sits behind a reverse proxy that runs on the same machine or in the same container, tell me I can use `Application.withHost "127.0.0.1"` for local-only access; if the proxy runs elsewhere, tell me to keep the default bind. Do not change the bind host without asking.
```
