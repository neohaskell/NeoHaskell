# Change 017: Add an S3-compatible BlobStore backend

Applications that deploy outside a single machine cannot keep uploads in
`BlobStore/Local.hs` (ephemeral disks, several replicas). Issue #370 asks for
an S3 backend. Per the maintainer's review of the first attempt: the backend
must be a record "like `BlobStore/Local.hs`, configuration managed completely
by the blob store module", AWS-agnostic, built on the Core `Http.Client`
rather than a raw HTTP library, with credentials supplied by the application's
own `Config.hs`/`App.hs`, and tested against a mock HTTP server rather than a
real S3 service. This change delivers exactly that facade: `S3Config` plus
`createBlobStore`, the four `BlobStore` operations, hand-written SigV4 (no new
dependency), one request per operation, no retries, no redirects.

```yaml spec
issue: issue#370
kind: feature
touches: [file-upload]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: true
```

## Contract delta

Depends on change 016 (`Http.Client.sendSecure` / `Http.Client.Internal.sendRaw`)
and change 021 (`Crypto.sha256`, `Crypto.hmacSha256`, `Crypto.toHex`); this PR
is stacked on them, so `SigV4` imports nothing from `crypton` or
`Data.ByteArray` and never unwraps `Bytes`. `SigV4` is exported as its own module so the signing
steps are unit-testable against the AWS reference vectors; applications are
expected to use only `S3Config` and `createBlobStore`.

```diff signatures
+ Service.Application: withFileUploadUsing :: Typeable config => (config -> FileUploadConfig) -> (config -> Task Text BlobStore) -> Application -> Application
+ Service.FileUpload.BlobStore.S3: data S3Config
+ Service.FileUpload.BlobStore.S3: S3Config :: Text -> Text -> Text -> Text -> Text -> S3Config
+ Service.FileUpload.BlobStore.S3: [endpoint] :: S3Config -> Text
+ Service.FileUpload.BlobStore.S3: [bucket] :: S3Config -> Text
+ Service.FileUpload.BlobStore.S3: [region] :: S3Config -> Text
+ Service.FileUpload.BlobStore.S3: [accessKeyId] :: S3Config -> Text
+ Service.FileUpload.BlobStore.S3: [secretAccessKey] :: S3Config -> Text
+ Service.FileUpload.BlobStore.S3: createBlobStore :: S3Config -> Task Text BlobStore
+ Service.FileUpload.BlobStore.S3.SigV4: data Credentials
+ Service.FileUpload.BlobStore.S3.SigV4: Credentials :: Text -> Text -> Text -> Credentials
+ Service.FileUpload.BlobStore.S3.SigV4: [accessKeyId] :: Credentials -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: [secretAccessKey] :: Credentials -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: [region] :: Credentials -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: data CanonicalRequest
+ Service.FileUpload.BlobStore.S3.SigV4: CanonicalRequest :: Text -> Text -> Array (Text, Text) -> Array (Text, Text) -> Text -> CanonicalRequest
+ Service.FileUpload.BlobStore.S3.SigV4: [method] :: CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: [path] :: CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: [query] :: CanonicalRequest -> Array (Text, Text)
+ Service.FileUpload.BlobStore.S3.SigV4: [headers] :: CanonicalRequest -> Array (Text, Text)
+ Service.FileUpload.BlobStore.S3.SigV4: [payloadHash] :: CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: canonicalRequest :: CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: stringToSign :: Credentials -> DateTime -> CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: signature :: Credentials -> DateTime -> CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: signedHeaders :: Array (Text, Text) -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: authorizationHeader :: Credentials -> DateTime -> CanonicalRequest -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: sha256Hex :: Bytes -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: uriEncode :: Text -> Text
+ Service.FileUpload.BlobStore.S3.SigV4: formatAmzDate :: DateTime -> Text
```

### Decisions carried from the review

- **Application selection.** `withFileUploadUsing` accepts upload policy and a
  backend factory using the same application configuration. The existing
  `withFileUpload` remains Local. Both replace the whole upload registration.
  Common limits are validated before the backend factory is applied or executed.
  Custom storage ignores `blobStoreDir`; startup errors propagate without Local
  fallback. Backend creation runs once per startup, not once per upload.
  Application still owns file-state storage, upload routes and their cleanup.
  A custom backend owns any extra resources it allocates; `BlobStore` has no
  shutdown operation. The S3 constructor allocates no such resources.
- **Shape.** Same as `Local`: a config record and one constructor. No extra
  operations outside the `BlobStore` facade. Conditional writes
  (`If-None-Match`/`If-Match`) are **not** included. Application dedup uses
  owner-scoped content-hash lookups, but concurrent uploads may write provisional
  blobs; direct S3 store calls overwrite unconditionally.
- **Provider-agnostic.** SigV4 + path-style addressing is the protocol shared
  by AWS S3, Cloudflare R2, Backblaze B2, MinIO and RustFS. Azure Blob Storage
  uses a different protocol (SharedKey REST) and is out of scope; it would be a
  sibling module `BlobStore/Azure.hs` with the same `Config`/`createBlobStore`
  shape.
- **Transport.** `Http.Client.sendSecure` (HTTPS, pinned TLS 1.2+, no
  redirects, no proxy, bounded response) for every endpoint;
  `Http.Client.Internal.sendRaw` only for127.0.0.1, [::1] or localhost
  (the existing local-fixture exception, unchanged by this repair). The module imports nothing from `Network.HTTP.*`.
- **Credentials.** Read by the application (its `Config.hs`), passed in
  `S3Config` from `App.hs`. The backend never reads the environment.
  `S3Config` has no `Show` instance; `Http.Client.Request` redacts headers, so
  the `Authorization` value cannot reach logs. Per-file authorization is an
  application concern (the app's own grant/ownership aggregate) and needs no
  core convention or RFC; `Integration.Http.Auth` (JWT/OAuth) is unrelated.
- **Errors.** 404 on GET -> `NotFound`; 404 on HEAD -> `exists = False`; DELETE
  accepts 204/200/404 (idempotent). Any other status -> `StorageError "S3 HTTP
  <code>"` (status only; S3 error bodies may echo the request). Transport
  failures -> `StorageError` without the URL.
- **Testing.** No Docker, no live endpoint in CI. `SigV4Spec` reproduces the
  four worked examples of the AWS S3 API reference (GET Object, PUT Object,
  GET Bucket Lifecycle, GET Bucket) step by step. `S3Spec` drives the real
  facade against `FakeS3`, an in-process WAI/Warp fake of the S3 object API on
  a held loopback socket with readiness notification and owned cleanup. The fake
  independently verifies received signatures and body hashes. Verified before writing it: no reusable in-process HTTP
  stub exists in `core/test*` today (only inline `GhcWarp.run` calls in
  `Http/ClientRawSpec.hs`, `Http/ClientSpec.hs`,
  `Auth/OAuth2/TokenRefreshSpec.hs`). **Boundary of `FakeS3`** (maintainer
  request, so a future package split keeps it with the S3 backend): it lives
  under `core/test/Service/FileUpload/BlobStore/S3/`, speaks only the S3 wire
  protocol (PUT/GET/HEAD/DELETE, ETag, 200/204/404/403), depends only on
  wai/warp/http-types/network (already in `common_cfg`) and Core, imports
  nothing from the FileUpload service or other test infrastructure, and is
  imported only by `S3Spec`. A run against a real S3-compatible server stays a
  manual recipe (below), never a CI gate.

### Manual check against a real S3-compatible server (not a gate)

Disposable RustFS container on loopback, throw-away credentials, then point an
application (or a `cabal repl` session) at it:

```sh
export RUSTFS_ACCESS_KEY=$(openssl rand -hex 12) RUSTFS_SECRET_KEY=$(openssl rand -hex 32)
docker run -d --name s3-manual --cap-drop ALL --security-opt no-new-privileges \
  -p 127.0.0.1::9000 --tmpfs /data --tmpfs /logs \
  -e RUSTFS_ACCESS_KEY -e RUSTFS_SECRET_KEY -e RUSTFS_CONSOLE_ENABLE=false \
  rustfs/rustfs:1.0.1
ENDPOINT="http://$(docker port s3-manual 9000/tcp)"
# create the bucket once (any S3 client, e.g. mc or aws cli), then in ghci:
#   S3.createBlobStore S3Config {endpoint = ENDPOINT, bucket = "manual", region = "us-east-1",
#                                accessKeyId = RUSTFS_ACCESS_KEY, secretAccessKey = RUSTFS_SECRET_KEY}
#   and exercise store / retrieve / exists / delete.
docker rm -f s3-manual
```

Other S3-compatible providers require their own endpoint and bucket setup.
The committed local wire fixture and AWS vectors are protocol evidence, not
fresh deployment certification for AWS, R2, B2, MinIO or RustFS.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | SigV4 reproduces the AWS reference vectors (canonical request hash, string to sign, signature, Authorization header) for GET Object, PUT Object, GET Bucket Lifecycle and GET Bucket | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#GET Object: canonical request hash, string to sign and signature match AWS`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#PUT Object: signs a non-empty payload with extra headers`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#GET Bucket Lifecycle: encodes a valueless query parameter`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#GET Bucket: sorts query parameters by name` | unit | none |
| C2 | SigV4 helpers: empty-payload hash, `x-amz-date` format, AWS URI encoding, sorted lowercase signed headers | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#hashes the empty payload to the documented SHA-256`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#formats x-amz-date as YYYYMMDD'T'HHMMSS'Z'`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#keeps unreserved characters and percent-encodes the rest in uppercase`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3/SigV4Spec.hs#lower-cases and sorts header names` | unit | none |
| C3 | `store`/`retrieve` round-trip through signed PUT and GET against the fake, overwrite semantics, empty blob | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#store and retrieve roundtrips bytes through signed PUT and GET`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#store overwrites an existing blob`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#stores and retrieves an empty blob` | integration | http:real |
| C4 | `exists` via HEAD (200 -> True, 404 -> False); `delete` idempotent | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#exists answers True after store and False for an unknown key`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#delete removes the blob and is idempotent` | integration | http:real |
| C5 | Error mapping from fake responses: 404 -> `NotFound`, 403 -> `StorageError` with status only | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#retrieve of a missing key is NotFound`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#a 403 from the store is a StorageError carrying only the status` | integration | http:real |
| C6 | Keys: each segment URI-encoded with slashes kept; empty keys and dot segments rejected before any request | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#encodes each key segment and keeps slashes as separators`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects dot segments and empty keys without sending a request` | integration | http:real |
| C7 | Configuration: HTTPS required except IPv4/IPv6 loopback or localhost, no path/query/userinfo in the endpoint, non-empty fields, DNS-style bucket of 3 to 63 lowercase letters, digits, hyphens and dots whose dot-separated labels are non-empty and start and end with a letter or digit; an invalid config is rejected by the constructor, so no store exists to send a request; a valid config does not contact the endpoint | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects an http endpoint that is not loopback`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects an endpoint carrying a path, query or credentials`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects empty fields and non-DNS bucket names`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects bucket names with an empty label or a hyphen at a label edge`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts dotted and hyphenated bucket names`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#enforces the 3 to 63 character bucket name bounds`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts an https endpoint without contacting it`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts loopback over plain http, including bracketed IPv6` | unit | none |
| C8 | An unreachable endpoint maps to `StorageError` without the URL | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#an unreachable endpoint is a StorageError without the URL` | unit | none |

| C9 | Malformed authorities are rejected as typed constructor errors before a store exists; every audited case is checked and a valid HTTPS control succeeds | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects malformed endpoint authorities before creating a store`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts nonempty secret material that is not an HTTP header` | unit | none |
| C10 | Header-bound controls are rejected before any request; valid configuration can still perform a PUT | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects header-bound controls before any HTTP request` | integration | http:real |
| C11 | Independently authenticated requests reject a wrong secret without storing data; C3 and C4 retain the valid four-method controls | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects a wrong signing secret without storing bytes` | integration | http:real |
| C12 | The independent wire verifier accepts the AWS GET reference signature and rejects wrong-secret/body/method/path/Host mutations | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#accepts the published AWS GET signature independently`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#rejects wrong-secret and body method path host tampering` | unit | none |
| C13 | The fake owns a bound loopback socket, signals readiness and serves immediate requests without a fixed startup delay | `hspec:nhcore-test-service:core/test/Service/FileUpload/BlobStore/S3Spec.hs#serves immediately on each owned socket without a startup delay` | integration | http:real |

C3–C6 drive the real facade over loopback HTTP against `FakeS3`, the
in-process Warp listener started by `withFakeS3`, so they are `integration` /
`http:real`; `docs/changes/test-surfaces.json` attests each exact selector with
its `Service.FileUpload.BlobStore.S3` fixture group. C6's rejection example
uses the same fixture to prove that zero requests reach the wire. C1, C2 and
C7 stay `unit` / `none`: the signer is pure, and configuration validation
fails or succeeds inside `createBlobStore` without any network I/O. C8 also
stays `unit` / `none`: it only proves that a connection attempt to a closed
loopback port becomes a sanitized `StorageError`, and no fixture server ever
answers it.

## Current verification and historical gates

Run the focused S3 tests with the maintained runner:

```sh
.agents/skills/tdd/scripts/test-match "BlobStore.S3" nhcore-test-service
.agents/skills/tdd/scripts/test-match "file upload backend selection" nhcore-test-service
.agents/skills/tdd/scripts/test-match "Application.withFileUploadUsing" nhcore-test-core
```

This selection includes both `S3Spec` and `SigV4Spec`. The runner rejects empty
selections, setup errors, failed assertions, and missing or ambiguous summaries.
The Application selection test sends an HTTP upload and download through a
running Application. Its S3 fixture checks the signed requests and stored bytes.
The startup tests cover Local compatibility, builder replacement, factory errors,
limit validation before backend application, and real config loading.
For broader runtime verification, use
`.agents/skills/neohaskell-test/scripts/run-full-suite --strict`.

The criteria and fixture attestations above retain the original contract and
test mapping. They are not a fresh execution receipt. The historical workflow
classified `file-upload` as security-sensitive and used `./dev spec-check --plan`
after Gate 1 to require a local-only security design review under ADR-0069.
Current main removed `./dev` and `scripts/spec-check`. Their retirement does not
waive the review requirements.

## User impact

Existing applications using `Application.withFileUpload` keep Local storage.
Applications can select S3 without constructing upload routes themselves:

```haskell
Application.new
  |> Application.withConfig @AppConfig
  |> Application.withFileUploadUsing
       makeFileUploadConfig
       (\config -> S3.createBlobStore (makeS3Config config))
```

For static configuration, use `withFileUploadUsing @()` with two functions that
ignore their argument. Static registration also works with `runWith`.
Config-dependent registration requires `run` to load application configuration.
Neither mode reads or creates `blobStoreDir` when a custom backend is selected.

The existing S3 primitive uses application-supplied configuration and supports
single-request uploads (no multipart). The bucket must already exist. Responses
have a256MiB cap; this is not an upload cap, and incremental enforcement depends
on change016/022. No retries: a failed write may or may not have landed.

## ADR

Application selection follows the config-dependent builder pattern in
[ADR-0035](../decisions/0035-config-dependent-application-builders.md) and retains
the storage interface in [ADR-0011](../decisions/0011-file-upload-architecture.md).
The private evaluated/deferred factory now carries an optional custom backend.
An alternative normalized upload record would duplicate existing configuration
without strengthening this contract. No backend registry, new dependency or
new resource lifecycle is introduced. The additive extension and its limits are
recorded above.
