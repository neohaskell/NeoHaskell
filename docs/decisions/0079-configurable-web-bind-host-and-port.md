# ADR-0079: Configurable web bind host and port

## Status

Accepted

## Context

`WebTransport` always bound Warp's default host and a port fixed in `server`.
Apps behind a reverse proxy want loopback-only binding, and the port could only
be changed by editing the `WebTransport` record, which `Application` hides
inside an existing transport value. The maintainer accepted a loopback-bind fix
and asked for the port to be configurable too.

## Decision

Add `host :: Text` to `WebTransport` and start Warp with `runSettings`, using
`setPort` and `setHost`. The text maps to Warp's `HostPreference`: `"*"` all
interfaces, `"*4"`, `"*6"`, or a literal address such as `"127.0.0.1"`.

The default is `"*4"`: exactly what the previous `Warp.run` bound (all IPv4
interfaces). A loopback default would silently break containers and any app
that is reached from another machine; a `"*"` default would silently start
listening on IPv6 too, a new reachable surface on dual-stack hosts whose
firewall rules only cover IPv4. Neither change belongs in a "make it
configurable" PR, so both loopback-only and IPv6 are opt-in through `withHost`.

Add `Application.withHost` and `Application.withPort`, stored as `bindHost` and
`bindPort` and applied when the web transport starts. They are on `Application`,
like `withCors` and `withHealthCheck`, because `withTransport` stores the
transport as an opaque value, so a builder cannot edit it. The override applies
whatever the order of `withTransport` and the builder.

The host and port stay plain `Text` and `Int` (no wrapper types), but the
contract is checked before Warp starts: `validateBindHost` accepts the Warp
wildcards and a bare literal address or hostname and rejects an empty host, a
URL or path, a `host:port` pair and whitespace; `validateBindPort` accepts
`1..65535` and rejects `0` (the OS would pick a port nothing reports back).
The error names the builder (`Application.withHost: …`). A bind the OS refuses
is caught as an `IOException` and re-thrown as
`WebTransport could not bind <host>:<port>: …`, so it travels the same
`Task.finally` cleanup path as every other startup failure.

## Consequences

- Constructing `WebTransport` or `Application` with every field named must add
  the new fields. This is the one breaking effect; `WebTransport.server` users
  are unaffected.
- Apps can bind loopback only, or choose the port, in one line.
- The start-up log now shows `<host>:<port>`.
- The default is unchanged: all IPv4 interfaces (`"*4"`), exactly what
  `Warp.run` bound before. IPv6 (`"*"` or `"*6"`) and loopback-only are opt-in
  through `withHost`.
- Loopback-only is advised only when the reverse proxy shares the app's
  network namespace (same host or container, shared pod network). A proxy in
  a separate namespace cannot reach a loopback listener; it needs a
  non-loopback bind plus firewalling.
- A mistyped host or port fails at startup with a message naming the builder;
  a port in use or an address this machine does not own fails with
  `could not bind <host>:<port>` instead of an uncaught `IOException`.
- The loopback test proves exclusion, not only reachability: it serves a
  per-test unique health path (instance identity) and checks that the port is
  still bindable on a non-loopback address, with a wildcard-bind negative
  control that shows the probe fails when the listener is not loopback-only.
