# Change 019: Make the Warp bind host and port configurable

The web server always bound Warp's default host on a port fixed in code, so an
app running behind a reverse proxy could not restrict itself to loopback and
could not choose its port without building a `WebTransport` by hand. The
maintainer accepted a loopback-bind fix and asked to "make the port configurable
too". `WebTransport` gains a `host` field (default `"*4"`, all IPv4 interfaces —
exactly what `Warp.run` bound before, so behaviour is unchanged) and `Application` gains `withHost` and `withPort`
builders next to the other `with*` builders. `runTransport` now starts Warp with
`runSettings` built from that host and port, and logs `<host>:<port>`.

Input contract: the host is a Warp wildcard (`"*"`, `"*4"`, `"*6"`) or a bare
literal address / hostname; the port is `1..65535`. An empty host, a URL or path
in the host field, a `host:port` pair, whitespace, port `0` or an out-of-range
port is rejected before any socket is opened, with an error that names the
builder (`Application.withHost: …` / `Application.withPort: …`). A bind the OS
refuses (port in use, address not owned) fails with
`WebTransport could not bind <host>:<port>: …` instead of an uncaught
`IOException`; the `Task.finally` cleanup in `Application.run` still releases
the resources the run owns (JWKS manager, dispatcher, file upload).

```yaml spec
issue: adhoc:server-bind-host-port
kind: feature
touches: [http-transport]
breaking: true
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

`WebTransport` and `Application` are public records, so adding a field changes
their constructor signature lines (a `-`/`+` pair each). `runTransports` gains
the two optional overrides that carry `withHost` / `withPort` to the transport.
`hostPreference` and `warpSettings` are the small, testable conversion from the
transport's host and port to Warp settings. `applyBindOverrides` is the pure
override rule; `validateBindHost` / `validateBindPort` are the pure input checks
that `runWebTransport` runs before starting Warp.

```diff signatures
- Service.Application: Application :: Maybe ConfigSpec -> Maybe EventStoreFactory -> Maybe QueryObjectStoreConfigValue -> Array QueryDefinition -> QueryRegistry -> Array ServiceRunner -> Map Text TransportValue -> Map Text QueryEndpointHandler -> Array OutboundRunner -> Array OutboundLifecycleRunner -> Array Inbound -> Maybe WebAuthFactory -> Maybe OAuth2Setup -> Maybe FileUploadFactory -> Maybe SecretStore -> Maybe ApiInfo -> Maybe CorsConfig -> Maybe HealthCheckConfig -> Maybe DispatcherConfig -> Maybe CorsFactory -> Maybe ApiInfoFactory -> Maybe HealthCheckFactory -> Maybe DispatcherConfigFactory -> Maybe SecretStoreFactory -> Array DeferredOutboundLifecycleReg -> Array DeferredInboundReg -> Array IntegrationRegistrationEntry -> Maybe ReadinessConfig -> Application
+ Service.Application: Application :: Maybe ConfigSpec -> Maybe EventStoreFactory -> Maybe QueryObjectStoreConfigValue -> Array QueryDefinition -> QueryRegistry -> Array ServiceRunner -> Map Text TransportValue -> Map Text QueryEndpointHandler -> Array OutboundRunner -> Array OutboundLifecycleRunner -> Array Inbound -> Maybe WebAuthFactory -> Maybe OAuth2Setup -> Maybe FileUploadFactory -> Maybe SecretStore -> Maybe ApiInfo -> Maybe CorsConfig -> Maybe HealthCheckConfig -> Maybe DispatcherConfig -> Maybe CorsFactory -> Maybe ApiInfoFactory -> Maybe HealthCheckFactory -> Maybe DispatcherConfigFactory -> Maybe SecretStoreFactory -> Array DeferredOutboundLifecycleReg -> Array DeferredInboundReg -> Array IntegrationRegistrationEntry -> Maybe ReadinessConfig -> Maybe Text -> Maybe Int -> Application
+ Service.Application: [bindHost] :: Application -> Maybe Text
+ Service.Application: [bindPort] :: Application -> Maybe Int
+ Service.Application: withHost :: Text -> Application -> Application
+ Service.Application: withPort :: Int -> Application -> Application
- Service.Application.Transports: runTransports :: Map Text TransportValue -> Map Text (Map Text EndpointHandler) -> Map Text (Map Text EndpointSchema) -> Map Text QueryEndpointHandler -> Map Text EndpointSchema -> Maybe AuthEnabled -> Maybe OAuth2Config -> Maybe FileUploadEnabled -> Maybe ApiInfo -> Maybe CorsConfig -> Maybe HealthCheckConfig -> Maybe IntegrationStatus -> Maybe ReadinessConfig -> QuerySubscriber -> Task Text Unit
+ Service.Application.Transports: runTransports :: Map Text TransportValue -> Map Text (Map Text EndpointHandler) -> Map Text (Map Text EndpointSchema) -> Map Text QueryEndpointHandler -> Map Text EndpointSchema -> Maybe AuthEnabled -> Maybe OAuth2Config -> Maybe FileUploadEnabled -> Maybe ApiInfo -> Maybe CorsConfig -> Maybe HealthCheckConfig -> Maybe IntegrationStatus -> Maybe ReadinessConfig -> Maybe Text -> Maybe Int -> QuerySubscriber -> Task Text Unit
- Service.Transport.Web: WebTransport :: Int -> Int -> Maybe AuthEnabled -> Maybe OAuth2Config -> Maybe FileUploadEnabled -> Maybe ApiInfo -> Maybe CorsConfig -> Maybe HealthCheckConfig -> Maybe IntegrationStatus -> Maybe ReadinessConfig -> Maybe (Task Text Readiness) -> WebTransport
+ Service.Transport.Web: WebTransport :: Int -> Text -> Int -> Maybe AuthEnabled -> Maybe OAuth2Config -> Maybe FileUploadEnabled -> Maybe ApiInfo -> Maybe CorsConfig -> Maybe HealthCheckConfig -> Maybe IntegrationStatus -> Maybe ReadinessConfig -> Maybe (Task Text Readiness) -> WebTransport
+ Service.Transport.Web: [host] :: WebTransport -> Text
+ Service.Transport.Web: hostPreference :: Text -> HostPreference
+ Service.Transport.Web: warpSettings :: WebTransport -> Settings
+ Service.Transport.Web: applyBindOverrides :: Maybe Text -> Maybe Int -> WebTransport -> WebTransport
+ Service.Transport.Web: validateBindHost :: Text -> Maybe Text
+ Service.Transport.Web: validateBindPort :: Int -> Maybe Text
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | `WebTransport` binds all IPv4 interfaces on port 8080 by default, exactly as before (`Warp.run`) | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#server binds all IPv4 interfaces by default (unchanged from Warp.run)`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#server listens on port 8080 by default` | unit | none |
| C2 | `Application.withHost` / `Application.withPort` record the bind host and port, and a new `Application` leaves them unset | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#withHost records the bind host`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#withPort records the bind port`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#bind host and port are unset on a new Application` | unit | none |
| C3 | The host text maps to the right Warp `HostPreference` (`127.0.0.1`, `*`, `*4`, `*6`, `::1`) | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#maps a literal IPv4 address to that host`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#maps * to all interfaces`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#maps *4 to all IPv4 interfaces`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#maps *6 to all IPv6 interfaces`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#maps a literal IPv6 address to that host` | unit | none |
| C4 | The Warp settings the transport runs with carry the configured host and port | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#carries the configured host and port into Warp` | unit | none |
| C5 | An application started with `withHost "127.0.0.1"` and `withPort <free port>` answers `GET /<unique health path>` on loopback (instance identity) and leaves the port free on a non-loopback address (loopback exclusion); the same probe finds the port taken when the default wildcard bind is used (negative control) | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#serves /health on loopback when bound to 127.0.0.1`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#a wildcard bind occupies the port on every address (negative control for the loopback probe)` | integration | http:real |
| C6 | Overrides apply whatever the order of `withTransport` and the builders; an absent override preserves the transport's own host/port; a host-only override keeps a custom port and a port-only override keeps a custom host | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#withHost and withPort apply whether they come before or after withTransport`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#absent overrides preserve the transport's custom host and port`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#a host-only override keeps the transport's custom port`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#a port-only override keeps the transport's custom host` | unit | none |
| C7 | `validateBindHost` accepts the Warp wildcards and bare literal addresses or hostnames, and rejects an empty host, a URL, a `host:port` pair and whitespace anywhere, including surrounding spaces, tabs and newlines; `validateBindPort` accepts `1..65535` and rejects `0` and out-of-range ports | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#accepts the Warp wildcards and bare literal addresses or hostnames`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects an empty or blank host`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects a URL pasted into the host field`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects a host that carries a port`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects a host with whitespace inside`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects a host with leading or trailing spaces`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects a host surrounded by tabs or newlines`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#accepts the full 1..65535 range`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects port 0 and ports outside the range` | unit | none |
| C8 | An invalid host or port makes `Application.runWith` fail with an `Application.withHost:` / `Application.withPort:` error before any socket is opened | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#an invalid host fails before any socket is opened`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#an invalid port fails before any socket is opened` | unit | none |
| C9 | A port already in use fails with `could not bind <host>:<port>` | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#a port already in use fails with an error naming the host and port` | integration | http:real |

C8 uses an in-memory application and rejects the setting before Warp starts,
so it has no real HTTP boundary. C5 exercises real HTTP listeners; C9 exercises
Warp's socket-bind failure against an occupied port. Their exact selectors
are attested with the `WebTransport bind host and port` fixture group printed
by BindSpec.

## User impact

Breaking only for code that builds the records positionally or with every field
named; nothing else changes. Apps that use `WebTransport.server` and apps built with `Application.new`
and the `with*` builders are unaffected and keep binding all IPv4 interfaces
on port 8080 unless they opt in; an app that already sets `server { port = ... }`
keeps that port, because an absent `withPort` preserves the transport's own
setting. Migration: a program that writes `WebTransport { ... }` with all fields
must add `host = "*4"` (the previous behaviour) or `host = "127.0.0.1"` for
loopback only; a program that writes `Application { ... }` with all fields must
add `bindHost = Nothing` and `bindPort = Nothing`. The testbed uses
`WebTransport.server` with no overrides, so its behaviour is unchanged.
New ways to use it: `Application.withHost "127.0.0.1"` and
`Application.withPort 9090`; `host` also accepts `"*"`, `"*4"`, `"*6"` or a
literal address. Loopback-only is the right choice when the reverse proxy runs
in the same network namespace as the app (same host or container, or a shared
pod network); a proxy in a separate namespace cannot reach the listener through
its own loopback and needs a non-loopback bind. A mistyped host or port now fails
at startup with a message naming the builder, instead of binding something
unintended or dying with a raw `IOException`.

## ADR

[ADR-0079](../decisions/0079-configurable-web-bind-host-and-port.md): why the
default stays `"*4"` (no new IPv6 surface) and why the overrides live on `Application`.
