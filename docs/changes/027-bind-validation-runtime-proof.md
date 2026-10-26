# Change 027: Enforce the existing bind validation and runtime proof contract

Continuation of change019 and [NickSeagull’s maintainer-reviewed authorized follow-up](https://github.com/neohaskell/NeoHaskell/pull/909#issuecomment-6040979623), source identity `NickSeagull`, comment `6040979623`. It requests earliest-practical checks, owned cleanup, instance identity/isolation and single-setting preservation; it does not approve a new config API or a new machine receipt. Keep the existing static builders, `*4` default, custom transport settings and port range1..65535. Fix early validation and representative bracket misuse; prove owned-instance identity and real assembly behavior without a configuration API redesign. Scoped continuation; final review and full verification remain required.

```yaml spec
issue: adhoc:bind-validation-runtime-proof
kind: bug
touches: [http-transport, service-wiring]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No public signatures or new production constants. Present static overrides for a registered WebTransport are checked in run/runWith before avoidable owned startup effects. Effective transport settings remain validated before listener startup. Existing no-WebTransport behavior is not expanded in this slice. Bare IPv6 remains supported; bracketed host:port and malformed bracket input produce actionable diagnostics. Port0 rejection remains, with truthful existing-breaking disclosure in change019 rather than new ephemeral-port behavior.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Bracketed host:port is rejected by the existing validator, with withPort guidance; bare IPv6 still accepted | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects bracketed IPv6 host-port before startup` | unit | none |
| C2 | Malformed bracketed IPv6 is rejected, bare IPv6 retained | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects malformed brackets and preserves bare IPv6` | unit | none |
| C3 | Invalid present static bind override with a registered WebTransport fails before auth discovery; a positive control proves the owned auth HTTP fixture is reachable | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects invalid static bind before owned auth discovery` | integration | http:real |
| C4 | run rejects an invalid known static override before attempting event-store setup | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects invalid static bind before run acquires an event store` | unit | none |
| C5 | applies host and port set before withTransport at runtime | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#applies host and port set before withTransport at runtime` | integration | http:real |
| C6 | applies host and port set after withTransport at runtime | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#applies host and port set after withTransport at runtime` | integration | http:real |
| C7 | preserves a custom registered host and port with absent overrides at runtime | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#preserves a custom registered host and port with absent overrides at runtime` | integration | http:real |
| C8 | preserves a custom registered port with a host-only override at runtime | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#preserves a custom registered port with a host-only override at runtime` | integration | http:real |
| C9 | preserves a custom registered host with a port-only override at runtime | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#preserves a custom registered host with a port-only override at runtime` | integration | http:real |
| C10 | accepts the intended Application and excludes another owned interface | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#accepts the intended Application and excludes another owned interface` | integration | http:real |
| C11 | a wildcard listener defeats the owned-interface bind probe | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#a wildcard listener defeats the owned-interface bind probe` | integration | http:real |
| C12 | rejects an unrelated catch-all 200 after the intended Application bind fails | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects an unrelated catch-all 200 after the intended Application bind fails` | integration | http:real |
| C13 | Returned bind failure stops and closes an already-started owned inbound socket before returning; live/release control makes the cleanup oracle falsifiable | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#closes a started inbound socket before returning the bind error`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#the same worker stays live until explicitly released without Application cleanup` | integration | http:real |
| C14 | Both `::1` and `[::1]` serve the owned Application on IPv6 loopback; bracketed names do not become wildcard preferences | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#serves the owned application on bare and bracketed IPv6 loopback`<br>`hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#does not unwrap bracketed names or wildcard preferences` | integration | http:real |

### Verification scope

C1–C4 cover bracket mistakes and early static validation. C5–C12 use real
Application assembly and owned handler acknowledgments, including wildcard and
competing-server negative controls. C13 uses an explicit inbound-start barrier,
then checks cleanup before fixture rescue or caller-owned EventStore closure.
The test-only watchdog is five seconds; elapsed time is never the evidence.
No new product constants are introduced.

C14 preserves the accepted `[::1]` input by removing IPv6 brackets when building
Warp settings. The bare `::1` control must answer before the bracketed case runs.
Both cases require the owned query handler to answer. Bracketed names such as
`[]`, `[*]` and `[localhost]` remain unchanged and can still fail at address
resolution. This is not a claim of complete early address validation.

Fresh native C1 receipt `a31fd74ad8e86d6f4546a9b3cad3ee314dee490a0322d64522471d197c079057`
was recorded at clean successor head `e9aa116e8bc99c40a561d068a951a1e295173688`,
PR914, before implementation. NickSeagull's confirmed Gate1 continuation source
is https://github.com/neohaskell/NeoHaskell/pull/909#issuecomment-6150731207 .
Local native approval and security reviews are separate records. This is not a
claim of historical red-first compliance, current green, final review or release
permission. PR914 is a no-force successor of909 with the same initial source tree.

## User impact

Existing static builder signatures and custom transport settings are preserved. Invalid bracket input fails earlier and clearly. Invalid known bind settings no longer lose their intended error behind avoidable startup work. `*4`/`*6` are family preferences with platform-specific fallback; this change does not promise strict family isolation. Document existing port0 rejection accurately. No new config-factory API, no expanded transportless validation, no broad lifecycle rewrite or Local/FileUpload changes.

## ADR

Existing ADR0079 owns bind choices. ADR0035 config-factory parity/explicit deferral remains a separate unresolved decision; this repair does not claim to settle it or infer approval. No new mechanical ADR trigger for fulfilling the existing validation contract.

## Localized scope and verification

Files: Service/Application.hs entry run/runWith, Service/Application/Transports.hs effective validation, Service/Transport/Web.hs validateBindHost, core/test/Service/Transport/Web/BindSpec.hs and test-surfaces, change019/ADR0079/fragment wording, generated codemap. Tests copy-adapt current BindSpec and its held-loopback/socket fixture. Preserve all existing expectations; add tests first. Security design review is local-only after authentic Gate1 continuation and before product edits. Runtime tests use owned loopback listeners/synthetic payloads only. Full required suites/testbed and exact-head CI remain separate gates; no unrelated PostgreSQL access.
