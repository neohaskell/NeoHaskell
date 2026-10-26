# Change 022: Enforce the byte-level HTTP transport guarantees

Approved existing-guarantee remediation for #905 (NickSeagull,
[Gate 1](https://github.com/neohaskell/NeoHaskell/pull/905#issuecomment-6150721779)). Byte-level
clients must verify HTTPS peers, stop reading oversized bodies before EOF,
and bypass environment proxies. Existing JSON APIs are outside this change.

```yaml spec
issue: adhoc:http-byte-transport-guarantees
kind: bug
touches: [http-client, core-primitives, ci-cd, dev-pipeline]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No new public HTTP type or method. Preserve the signatures and default values
of `sendSecure`, `getSecure`, `sendRaw` and `getRaw`. The existing helper export
is retained and accurately declared in Change 016; no public API removal or
redesign is part of this repair. Existing timeout semantics remain response
inactivity timeout, not a total-body deadline. Limits apply to byte-level
responses, not every JSON request.

```diff signatures
```

## Criteria

C1 is the minimal newly added repro. It sends five bytes on a real chunked
response with limit four, flushes them, and withholds EOF behind a latch. The
client must return `ResponseTooLarge 4` while the latch remains closed. An
actual HTTP200 positive control checks the same listening fixture first. A
separate source-flushed signal must be observed before the client observation
window begins; missing delivery fails as fixture setup, not the regression
identity. Two seconds is each test observation window, not a new product timeout. The test
releases its source and stops its client on either outcome. The existing
fixture's readiness sleep is inherited; failure to connect is not an accepted
red identity. Formal red evidence requires the exact assertion below.

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Reject an oversized chunk before the source may finish | `hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#sendRaw rejects an oversized chunk before source EOF` | integration | http:real |
| C2 | Preserve method, body, headers and raw non-2xx behavior | `hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#sendRaw PUT delivers method, raw body and custom header`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#sendRaw DELETE surfaces a 404 as a status code, not an error` | integration | http:real |
| C3 | Preserve independent public Crypto compatibility vectors | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches the HMAC-SHA256 known answer`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches the known answer for an empty message` | unit | none |
| C4 | Require successful transport evidence in the current aggregate CI gate | `script:core/test/Http/test_transport_fixture.py#WorkflowTests.test_aggregate_requires_transport_success` | unit | none |
| C5 | Test-manager trusted TLS succeeds, untrusted CA/wrong hostname fail, public untrusted TLS fails (not public trusted success) | `script:core/test/Http/transport_fixture.py#tls` | integration | http:real |
| C6 | Fresh-process lower/uppercase HTTP and CONNECT proxy controls work; actual raw and trusted public secure requests return exact origin status, header and bytes without additional trap events | `script:core/test/Http/transport_fixture.py#proxy` | integration | http:real |
| C7 | Raw exact/empty/HEAD, finite overflow, advertised/decompressed size and existing zero-builder rejection controls pass; chunked/unknown-length reads reject before EOF and close | `script:core/test/Http/transport_fixture.py#body` | integration | http:real |
| C8 | Public HTTPS accepts independently trusted A and B peers with exact status/header/bytes; A-only trust rejects B and a wrong hostname; TLS1.2 succeeds and TLS1.1 fails against a verified legacy-client positive control | `script:core/test/Http/transport_fixture.py#public-trust` | integration | http:real |
| C9 | The internal secure reader rejects chunked/unknown-length bodies before EOF and closes the actual socket (plaintext diagnostic, not public TLS) | `script:core/test/Http/transport_fixture.py#secure-internal-body` | integration | http:real |
| C10 | Public trusted HTTPS satisfies the complete C7 body matrix, including withheld advertised body, paced five-byte chunked and close-delimited overflow, typed early rejection and live-process socket closure | `script:core/test/Http/transport_fixture.py#public-body` | integration | http:real |

### Executable ownership and required evidence

C6, C8 and C10 require the pinned Linux trust-loader capability. `SYSTEM_CERTIFICATE_PATH`
selects the fixture bundle in a fresh process. The pinned macOS loader uses
Keychains and has no environment-bundle override; non-Linux hosts return exit77,
which is unsupported evidence, never a passing or skipped-green criterion.
The fixture clears `SSL_CERT_FILE`, `SSL_CERT_DIR` and `NIX_SSL_CERT_FILE` in
the child environment so only `SYSTEM_CERTIFICATE_PATH` selects its bundle.
C9 invokes the actual private
secure reader over a plaintext loopback socket to isolate read/close behavior;
it cannot stand in for public secure request success. All three body modes observe
peer closure and require it before the source EOF latch/deadline.

The script uses Python stdlib loopback fixtures and invokes
`.agents/skills/neohaskell-best-practices/scripts/with-toolchain cabal repl`
for the actual Haskell request path. It checks clean compilation and
explicit output markers rather than trusting GHCi exit0. Temporary synthetic
keys are deleted and listeners close in `finally`; no system trust is changed.

Linux CI runs all six modes in `test-transport` and requires that job in
`ci-gate`. Current main removed `./dev`, `scripts/spec-check`,
`scripts/workflow-check` and `codemap/`. Their removal is preserved. C4 now
executes the current aggregate gate with successful, failed, cancelled,
skipped and missing transport results. The obsolete combined criteria-report
consumer is not restored. Core and service jobs remain required independently.
Each successful transport mode emits one exact
`PASS script:core/test/Http/transport_fixture.py#<mode>` receipt and the printed
`Http.Client.syntheticTransport` fixture marker. Nonzero exit (including 77),
missing or duplicate receipts, wrong modes, and missing fixture markers fail
the gate. Scripts do not fabricate Hspec summaries. Each native-toolchain
session has a 600-second timeout. CI retains generated probes, output and
server events alongside the transport report, without private keys.
Body probes keep the client process alive for three seconds after rejection
and require socket closure before 2.5 seconds while that process is still alive.
The typed result also excludes closure by the two-second read watchdog.
The advertised-length fixture sends headers but no body and withholds EOF.
The chunked and close-delimited fixtures flush four bytes, then one byte,
and withhold EOF. Production timeouts are unchanged.

C5 and C9 remain diagnostics. C6, C8 and C10 execute `Http.sendSecure`
through its actual cached manager, without a production test seam. C8 also
checks ordinary library system-store loading. Its TLS1.1 server uses A's
trusted certificate. A certificate-verifying legacy client must negotiate
TLS1.1 and receive the exact response before public rejection can pass.
Legacy protocol and cipher settings exist only in this fixture.

Gate 1 authorizes repair of existing guarantees, not readiness or merge. The
following validation promises must be proved before final review;
no placeholder, skipped test, helper-only proof, or existing suite count can
stand in for them. `ClientRawSpec.hs` retains C1 and the existing byte API
tests in `nhcore-test-core`. The public TLS and fresh-process proxy promises
map to C6, C8 and C10 in the existing `http:real` script entry in
`test-surfaces.json`. They do not launch a nested Cabal REPL from a Cabal-owned
Hspec test, which can contend for the same build directory. This ownership
adjustment changes no response, trust, proxy or resource guarantee.
The Python unit suite validates fixture logic only, never Haskell behavior.

- `sendSecure accepts a trusted TLS peer`: synthetic local CA via a test-only
  trust seam through the same secure request path; verify status, header and
  bytes. Preserve chain/hostname verification and TLS 1.2+. Also verify the
  supported library/system trust-loading path separately without public egress
  or installing a fixture CA globally. Stop if the chosen library cannot offer
  an isolated test store without weakening the default manager.
- `sendSecure rejects an untrusted TLS peer` and
  `sendSecure rejects a wrong TLS hostname`: separate CA and wrong-name
  negatives against the otherwise working TLS fixture. Disabling verification
  must make the negatives fail. TLS 1.1 must remain rejected.
- `sendSecure rejects an oversized chunk before source EOF`: the C1 contract
  through real trusted TLS, plus socket-close observation. Raw C1 alone is not
  proof of the secure path.
- `byte paths preserve exact zero unknown length and HEAD limits`: both paths;
  zero is currently rejected by the builder (C7 preserves that behavior);
  any future zero-byte reader contract needs explicit approval, exactly N
  succeeds, N+1 fails,
  chunked and close-delimited unknown lengths stop early, oversized advertised
  length rejects without a full body, HEAD succeeds with empty body despite
  large advertised length. Count delivered/decompressed body bytes. A finite
  paced tail and EOF latch must distinguish post-read checks; allow transport
  buffer overshoot rather than requiring server writes equal N.
- `byte paths bypass a proven environment proxy trap`: fresh process per env
  case, both cases of HTTP_PROXY/HTTPS_PROXY set before any cached manager,
  empty NO_PROXY/no_proxy. Positive control reaches trap with absolute HTTP
  request and HTTPS CONNECT. Actual byte paths then reach owned origins with
  success and no trap events. Use only synthetic authorization. Assert direct
  manager behavior, not merely `request.proxy = Nothing`.

Use held loopback sockets/readiness signals for the expanded fixtures; do not
solve fixture races by increasing sleeps. The hard requirement is early
termination and response closure; no fixed RSS bound or universal buffer size
is invented. Test the actual boundary, not only the helper. Independent known
answers remain the Crypto oracle. These scoped principles are obligations of
this spec and its executable checks, not a new agent framework.

## Implementation boundary and gates

Localized sources: `core/http/Http/Client.hs` (`secureTlsManager`,
`sendSecureRawIO`, `readBodyFully`, `readBodyChunks`, `applyRequestOptions`),
`core/http/Http/Client/Internal.hs` (`sendRawInternalIO`, `applyRequestOptions`).
Copy/adapt tests from `core/test/Http/ClientRawSpec.hs`; preserve existing
expectations. Prefer a bounded incremental reader with prompt response close,
without a new public abstraction. Do not alter Crypto production code.

Supporting scope: correct Change 016 CI/tooling ownership and inaccurate
loopback/shared-manager wording; point Change 021 C4 at existing independent
known answers. Preserve current main's CI jobs and tooling deletions.
Correct API, release and header/limit/timeout prose only as approved.

Gate 1 requires a committed C1 failure, verified native red-evidence receipt,
final exact criteria, and explicit maintainer approval. No historical receipt
was recovered in this worktree. Required local security review still precedes
product edits; do not publish local security notes. The retired spec-check
entrypoint is not a runnable gate on current main.
Performance review follows actual risk routing, not an invented
requirement. Full native tests and current-base CI remain future gates.

## User impact

Apps using byte-level HTTP should receive valid HTTPS responses, avoid proxy
environment routing, and reject oversized responses without downloading the
whole source. No app edits are proposed for existing intended APIs. `sendRaw`
remains a trusted-caller API: loopback is guidance, not an enforced host policy.
The existing public helper is retained and declared without changing its
signature. No release is claimed.

## ADR

Not required by mechanical flags at this draft stage. The existing public API
ADR rule and any helper removal require an explicit maintainer disposition;
choosing removal must update breaking metadata and the relevant ADR before
approval. The incremental limit implements the existing ADR-0036 obligation,
not a relaxed post-read promise.
