---
title: Upload and attach files
description: Store uploaded bytes, validate file references, and attach them to accepted application actions.
sidebar:
  order: 5
---

Someone uploads a file, then closes the browser before finishing the form. The application needs temporary storage for that unfinished upload and a clear association when the file becomes part of an accepted action.

NeoHaskell provides file references, upload/download routes, ownership checks on the user-facing path, and a file lifecycle. You decide which files are acceptable and when to attach them. Continue in `mug-shop`: enable a local upload route, upload a small sample, then design how artwork becomes attached to a personalised mug.

## Enable uploads in your application

Put this development configuration in `src/Shop/Uploads.hs`. The
[complete integration checkpoint](/examples/mug-shop-connect.tar.gz) includes
the surrounding module wiring:

```haskell
uploadConfig :: FileUploadConfig
uploadConfig = FileUploadConfig
  { blobStoreDir = "./uploads"
  , stateStoreBackend = InMemoryStateStore
  , maxFileSizeBytes = 10485760
  , pendingTtlSeconds = 21600
  , cleanupIntervalSeconds = 900
  , allowedContentTypes = Just ["text/plain", "image/png", "application/pdf"]
  , storeOriginalFilename = True
  }
```

In `src/App.hs`, bring the upload helper into scope as `Uploads` and add this
line to your existing application pipeline:

```haskell
    |> Application.withFileUpload @() (\_ -> Uploads.uploadConfig)
```

This static factory works without adding a configuration type. When the directory
and limits become deployment settings, pass a function from your configuration
type instead; see [configuration](/build/configuration/). File upload support
belongs to the core package.

Run `neo build`, `neo test`, then `neo run` from `mug-shop`. The blob directory is
created when needed. This example keeps file metadata in memory: restarting loses
those references even if bytes remain in `uploads/`. It is a learning setup;
choose persistent file metadata and persistent blob storage together for
[deployment](/operate/deployment/).

## Upload bytes before attaching them

In another terminal at your project root, create your own tiny fixture and send it
to the running server:

```sh
mkdir -p examples
printf 'Blue mug artwork draft\n' > examples/artwork-note.txt
curl -F 'file=@examples/artwork-note.txt;type=text/plain' \
  http://localhost:8080/files/upload
```

Expect JSON containing `fileRef`, `filename`, `contentType`, `sizeBytes`, and
`expiresAt`. This first round trip uses the local application without
authentication. If you have enabled authentication, supply your credentials as
described in [access control](/build/access-control/).

Use the returned reference to request `GET /files/{fileRef}` and compare the bytes
with your fixture. Replace the placeholder in this command:

```sh
curl http://localhost:8080/files/YOUR-FILE-REFERENCE
```

**Current authenticated-download limitation:** the download route uses the
`Everyone` middleware mode, which returns anonymous claims even when a token is
present. An upload owned by an authenticated subject therefore cannot be assumed
downloadable through this route. Verify and resolve that path before enabling
private attachments; the anonymous exercise does not establish authenticated
ownership support end to end.

Uploading bytes has not yet changed a cart. To add an artwork feature, design a
command with an `attachment :: FileRef` field. `FileRef` is the reference type
defined in `Service.FileUpload.Core`. Use the command marker from
[commands and events](/build/commands-and-events/). The framework resolves the
reference before the command runs and provides the metadata through
`RequestContext.files`.

Keep the file reference in the accepted event, together with the association to
its cart or artwork request. Do not copy raw bytes into the event. A command
that accepts artwork, its event, and the view that displays it are new application
work; add them before offering an “attach” action on the screen.

## Choose limits and cleanup behaviour

`FileUploadConfig` gives your application these choices:

| Field | Application decision |
| --- | --- |
| `blobStoreDir` | Where the actual file bytes live |
| `stateStoreBackend` | Where file lifecycle metadata persists |
| `maxFileSizeBytes` | Largest accepted upload |
| `pendingTtlSeconds` | How long an unfinished upload remains usable |
| `cleanupIntervalSeconds` | Cleanup scheduling configuration |
| `allowedContentTypes` | Allowed declared media types, or no restriction |
| `storeOriginalFilename` | Whether original names are retained |

Startup checks positive size and timing values, requires a nonempty directory,
and requires cleanup interval to be shorter than pending TTL. The current
application wiring does not start the available cleanup worker. Expiry is enforced
on access, but abandoned blob bytes are not automatically reclaimed by this setup.
Arrange and test cleanup for the backend you deploy.

## Understand the validation boundary

The resolver checks file existence, deletion, pending expiry, ownership, and blob presence. Pending references expire; confirmed ones are not rejected merely for pending TTL expiry. The application still needs rules for retention and removal.

The background integration file-access context is different from a user's request context. Its implementation retrieves by reference from storage; it does not carry a requesting user's ownership check. Therefore trigger processing only from an authorised action that validated the association. Do not accept an arbitrary reference from an untrusted prompt and hand it to a background processor.

A declared media type is useful for routing and limits, but does not prove the bytes are valid artwork or a safe document. Validate the properties your application relies on before accepting them.

## Exercise: a customer's abandoned artwork

In the practice project, decide when artwork becomes attached to an order, what happens after pending expiry, and what the screen shows if the stored bytes are missing.

<details>
<summary>Suggested checks</summary>

Test a valid owned reference, another user's reference, an expired pending upload, a deleted reference, and missing blob bytes. Test missing multipart data and an oversized file. Keep the command rejected when its required attachment cannot be resolved. Keep these checks in your project and run them with `neo test`; separately exercise the live HTTP upload route. Add an authenticated ownership scenario before enabling private attachments.

</details>

Continue with [document processing](/connect/documents/) once the attachment lifecycle is clear.

<details>
<summary>Framework source notes</summary>

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

</details>
