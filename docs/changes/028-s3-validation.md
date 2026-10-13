# Change 028: Reject malformed S3 configuration before requests

Repair the audited existing constructor guarantee without a new public API.
NickSeagull approved continuing audited fixes to existing guarantees in
https://github.com/neohaskell/NeoHaskell/pull/906#issuecomment-6150728911.
Public Application backend selection remains a separate unresolved decision.

```yaml spec
issue: adhoc:s3-validation
kind: bug
touches: [file-upload]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No public signature changes. Parse endpoint authority once before constructing
S3Store: require a valid DNS/IPv4 host or bracketed IPv6, and an optional decimal
port in1–65535. Reject controls, malformed brackets, empty ports and trailing
junk. Keep existing HTTPS and loopback/localhost policy, trailing slash and
surrounding whitespace normalization. Reject ASCII controls in header-bound
region/accessKeyId; secretAccessKey remains arbitrary nonempty HMAC material.
Errors are typed Task Text errors and do not echo configuration or credentials.
No network request is made by the constructor. Do not catch asynchronous
exceptions or change the production signer, retries, limits or HTTP layer.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | All seven audited malformed authorities return typed errors, with a valid HTTPS control | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects malformed endpoint authorities before creating a store` | unit | none |
| C2 | Header-bound controls fail before requests; valid config still performs PUT | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects header-bound controls before any HTTP request` | integration | http:real |
| C3 | Nonempty HMAC secret material is not constrained as a header | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts nonempty secret material that is not an HTTP header` | unit | none |
| C4 | Existing bracketed IPv6 and IPv4 loopback endpoints remain accepted | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts loopback over plain http, including bracketed IPv6`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#validates endpoint port bounds and bracket suffixes without network I/O` | unit | none |

## User impact

Malformed operator configuration now fails during store creation instead of
escaping the typed error path during a request. Existing valid configuration,
public signatures, Local defaults and S3 signing remain unchanged.

## ADR

Not required: internal validation repair, no new public API or compatibility policy.
