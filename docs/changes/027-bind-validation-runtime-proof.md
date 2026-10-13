# Change 027: Enforce the existing bind validation and runtime proof contract

Continuation of change019 and [NickSeagull’s maintainer-reviewed authorized follow-up](https://github.com/neohaskell/NeoHaskell/pull/909#issuecomment-6040979623), source identity `NickSeagull`, comment `6040979623`. It requests earliest-practical checks, owned cleanup, instance identity/isolation and single-setting preservation; it does not approve a new config API or a new machine receipt. Keep the existing static builders, `*4` default, custom transport settings and port range1..65535. Fix early validation and representative bracket misuse; prove owned-instance identity and real assembly behavior without a configuration API redesign. Preliminary regression draft, not final approval/readiness.

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

No public signatures or new production constants. Present static overrides for a registered WebTransport are checked in run/runWith before avoidable owned startup effects. Effective transport settings remain validated before listener startup. Existing no-WebTransport behavior is not expanded in this slice. Bare IPv6 remains supported; bracketed host:port, unmatched brackets and bracket-wrapped host input produce actionable diagnostics. Port0 rejection remains, with truthful existing-breaking disclosure in change019 rather than new ephemeral-port behavior.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Bracketed host:port is rejected by the existing validator, with withPort guidance; bare IPv6 still accepted | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects bracketed IPv6 host-port before startup` | unit | none |
| C2 | Bracket-wrapped/malformed IPv6 is rejected, bare IPv6 retained | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects brackets around a bind host and preserves bare IPv6` | unit | none |
| C3 | Invalid present static bind override with a registered WebTransport fails before auth discovery; a positive control proves the owned auth HTTP fixture is reachable | `hspec:nhcore-test-service:core/test/Service/Transport/Web/BindSpec.hs#rejects invalid static bind before owned auth discovery` | integration | http:real |

### Remaining executable criteria before implementation

- Invalid present override with a real registered WebTransport and owned local auth fixture yields the intended validation error and zero discovery requests; fixture positive control proves it is listening. Cover run/runWith at their real effect boundary.
- Both builder orders, absent overrides, host-only/custom-port and port-only/custom-host consume the registered transport via real Application.runWith. Reset-to-default call-site mutant fails; original implementation currently preserves these settings.
- Intended application lifecycle is tied to readiness. A competing catch-all200 with failed intended bind cannot satisfy the success oracle. Preserve loopback exclusion and wildcard negative control.
- A bind-failure fixture with an observable owned worker proves its shutdown, not just the returned error. Wider inherited lifecycle defects require separate exact scope if discovered.

These are mandatory unfinished tests, not passing locators. Native committed-red C1 and current verified receipt precede new product edits; final exact criteria and authentic continuation approval/review records remain required. Existing explicit maintainer requests are scoped authority, not fabricated receipt or final Gate2.

## User impact

Existing static builder signatures and custom transport settings are preserved. Invalid bracket input fails earlier and clearly. Invalid known bind settings no longer lose their intended error behind avoidable startup work. `*4`/`*6` are family preferences with platform-specific fallback; this change does not promise strict family isolation. Document existing port0 rejection accurately. No new config-factory API, no expanded transportless validation, no broad lifecycle rewrite or Local/FileUpload changes.

## ADR

Existing ADR0079 owns bind choices. ADR0035 config-factory parity/explicit deferral remains a separate unresolved decision; this repair does not claim to settle it or infer approval. No new mechanical ADR trigger for fulfilling the existing validation contract.

## Localized scope and verification

Files: Service/Application.hs entry run/runWith, Service/Application/Transports.hs effective validation, Service/Transport/Web.hs validateBindHost, core/test/Service/Transport/Web/BindSpec.hs and test-surfaces, change019/ADR0079/fragment wording, generated codemap. Tests copy-adapt current BindSpec and its held-loopback/socket fixture. Preserve all existing expectations; add tests first. Security design review is local-only after authentic Gate1 continuation and before product edits. Runtime tests use owned loopback listeners/synthetic payloads only. Full required suites/testbed and exact-head CI remain separate gates; no unrelated PostgreSQL access.
