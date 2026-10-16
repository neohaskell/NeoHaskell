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

The default remains `"*4"`, the same IPv4 preference the previous `Warp.run`
used. It may fall back to IPv6; `"!4"` is the strict IPv4 form. A loopback default would silently break containers and any app
that is reached from another machine; a `"*"` default would silently start
listening on IPv6 too, a new reachable surface on dual-stack hosts whose
firewall rules only cover IPv4. Neither change belongs in a "make it
configurable" PR, so the existing preference remains unchanged; use a literal loopback address
for loopback-only reachability.

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
`WebTransport could not bind <host>:<port>: …`, so it reaches the transport-failure cleanup path. The owned inbound-worker
bind-failure criterion proves its specific cleanup boundary, not all startup
exceptions or cancellation phases.

## Consequences

- Constructing `WebTransport` or `Application` with every field named must add
  the new fields. Existing custom port `0` usage must also move to a fixed
  port in `1..65535`; the feature already rejects ephemeral ports.
- Apps can bind loopback only, or choose the port, in one line.
- The start-up log now shows `<host>:<port>`.
- The default remains the `"*4"` IPv4 preference, including underlying family
  fallback. `"*6"` prefers IPv6; `"!4"`/`"!6"` are strict-family forms.
- Loopback-only is advised only when the reverse proxy shares the app's
  network namespace (same host or container, shared pod network). A proxy in
  a separate namespace cannot reach a loopback listener; it needs a
  non-loopback bind plus firewalling.
- A mistyped host or port fails at startup with a message naming the builder;
  a port in use or an address this machine does not own fails with
  `could not bind <host>:<port>` instead of an uncaught `IOException`.
- The loopback test proves exclusion, not only reachability: it serves a
  per-test unique health path plus an owned query-handler acknowledgment and checks that the port is
  still bindable on a non-loopback address, with a wildcard-bind negative
  control that shows the probe fails when the listener is not loopback-only.
