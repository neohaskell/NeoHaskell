---
title: Attach files to business actions
description: Upload files, validate references, and keep storage lifecycle separate from business meaning.
sidebar:
  order: 5
---

A customer uploads artwork for a personalised mug, then closes the browser before placing the order. The shop needs temporary storage for unfinished uploads and a clear association once the artwork becomes part of an accepted order.

NeoHaskell provides file references, upload/download routes, ownership checks on the user-facing path, and a file lifecycle. Your shop still decides which files are acceptable and when an attachment becomes business evidence.

## Follow the two-step interaction

1. Upload the bytes to `/files/upload` as multipart form data with a `file` field.
2. Take the returned `fileRef` and include it in a business command field typed as `FileRef`.

The public testbed's `CreateDocument` uses an `attachment :: FileRef`. The framework resolves it before the command runs; the command looks for the resolved metadata in `RequestContext.files`.

The following shell command is a **testbed exercise**, run from the repository root with its local server running. It sends the existing public fixture:

```sh
curl -F 'file=@testbed/tests/files/fixtures/hello.txt;type=text/plain' \
  http://localhost:8080/files/upload
```

Expect a JSON response including `fileRef`, `filename`, `contentType`, `sizeBytes`, and `expiresAt`. For an authenticated application, supply its credentials as described in [permissions](/build/access-control/); the testbed's local route configuration is not a production access policy.

The download route is `GET /files/{fileRef}`. The public testbed tests check bytes and content headers in its no-auth configuration. **Current authenticated-download limitation:** the route uses the `Everyone` middleware mode, which returns anonymous claims even when a token is present. An upload owned by an authenticated subject therefore cannot be assumed downloadable through this route; prove and repair that path before using private customer attachments. The local testbed round trip does not establish authenticated ownership support end to end.

Keep the reference, rather than copying raw file bytes into an event. Your event records the business association, such as which artwork belongs to the accepted order.

## Enable storage deliberately

The application registration is a fragment from the public testbed:

```haskell
    |> Application.withFileUpload makeFileUploadConfig
```

`makeFileUploadConfig` is a function from your application configuration to `FileUploadConfig`. It chooses:

| Field | Decision for the shop |
| --- | --- |
| `blobStoreDir` | Where the actual file bytes live |
| `stateStoreBackend` | Where file lifecycle metadata persists |
| `maxFileSizeBytes` | Largest accepted upload |
| `pendingTtlSeconds` | How long an unfinished upload remains usable |
| `cleanupIntervalSeconds` | Cleanup scheduling configuration |
| `allowedContentTypes` | Allowed declared media types, or no restriction |
| `storeOriginalFilename` | Whether original names are retained |

The testbed uses a local blob directory and a PostgreSQL file state store. Persistent metadata does not make an ephemeral blob directory persistent. Include both in your [deployment and recovery design](/operate/deployment/).

Application startup checks positive size and timing values, requires a nonempty directory, and requires cleanup interval to be shorter than pending TTL. The inspected application setup does not start the available cleanup worker. Pending-reference expiry is enforced on access, but do not assume abandoned blob bytes are automatically reclaimed by this wiring. Arrange and test a cleanup strategy for the backend you deploy.

## Understand the validation boundary

The resolver checks file existence, deletion, pending expiry, ownership, and blob presence. Pending references expire; confirmed ones are not rejected merely for pending TTL expiry. The application still needs rules for retention and business-level removal.

The background integration file-access context is different from a user's request context. Its implementation retrieves by reference from storage; it does not carry a requesting user's ownership check. Therefore trigger processing only from an authorised business action that validated the association. Do not accept an arbitrary reference from an untrusted prompt and hand it to a background processor.

A declared media type is useful for routing and limits, but does not prove the bytes are valid artwork or a safe document. Validate whatever properties your business relies on before accepting them.

## Exercise: a customer's abandoned artwork

Decide when the shop should consider artwork attached, what happens after pending expiry, and what the customer sees if the stored bytes are missing.

<details>
<summary>Suggested checks</summary>

Test a valid owned reference, another user's reference, an expired pending upload, a deleted reference, and missing blob bytes. Test missing multipart data and an oversized file. Keep the business command rejected when its required attachment cannot be resolved. The public upload tests provide working request shapes; add an authenticated ownership scenario for your application.

</details>

Continue with [document processing](/connect/documents/) once the attachment lifecycle is clear.

## Implementation and examples

- [core/auth/Auth/Middleware.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/Middleware.hs)
- [core/service/Service/Transport/Web.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Transport/Web.hs)
- [core/service/Service/FileUpload/Resolver.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/FileUpload/Resolver.hs)
- [core/service/Service/FileUpload/Web.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/FileUpload/Web.hs)
- [core/service/Service/Application.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [testbed/src/Testbed/Document/Commands/CreateDocument.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Document/Commands/CreateDocument.hs)
- [testbed/tests/files/upload.hurl](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/files/upload.hurl)
- [testbed/tests/files/download.hurl](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/files/download.hurl)
- [testbed/tests/files/upload-errors.hurl](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/files/upload-errors.hurl)

