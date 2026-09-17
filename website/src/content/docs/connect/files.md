---
title: Upload and attach files
description: Store uploaded bytes, validate file references, and attach them to accepted application actions.
sidebar:
  order: 5
---

Someone uploads a file, then closes the browser before finishing the form. The application needs temporary storage for that unfinished upload and a clear association when the file becomes part of an accepted action.

NeoHaskell provides file references, upload and download routes, ownership checks on the user-facing path, and a file lifecycle. You decide which files are acceptable and when to attach them. Continue in your own `mug-shop` project: create one upload configuration file, append one application registration, upload a small sample, then design how artwork becomes attached to a personalised mug.

All paths below are relative to the `mug-shop` project root. The focused snippets explain the choices first. The complete files show the exact imports and surrounding application code needed for a runnable checkpoint.

## Choose the upload policy

For this local exercise, allow small text notes, PNG artwork, and PDFs. Keep bytes in `./uploads`, keep lifecycle metadata in memory, and expire unfinished references after six hours:

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

This policy is a learning setup. The metadata store is in memory, so restarting loses the references even if the bytes remain in `uploads/`. A deployed application should choose persistent metadata and persistent blob storage together.

## Create the upload configuration file

Create `src/Shop/Uploads.hs`. Copy the complete file below as one file rather than guessing which file upload types to import.

<!-- complete-file -->
```haskell title="src/Shop/Uploads.hs"
module Shop.Uploads (uploadConfig) where

import Core
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..))


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

The fields are application decisions:

| Field | Decide what it means for your deployment |
| --- | --- |
| `blobStoreDir` | Where the actual file bytes live |
| `stateStoreBackend` | Where file lifecycle metadata persists |
| `maxFileSizeBytes` | Largest accepted upload |
| `pendingTtlSeconds` | How long an unfinished upload remains usable |
| `cleanupIntervalSeconds` | Cleanup scheduling configuration |
| `allowedContentTypes` | Allowed declared media types, or no restriction |
| `storeOriginalFilename` | Whether original names are retained |

Startup checks positive size and timing values, requires a nonempty directory, and requires cleanup interval to be shorter than pending TTL. The current application wiring does not start the available cleanup worker. Expiry is enforced on access, but abandoned blob bytes are not automatically reclaimed by this setup. Arrange and test cleanup for the backend you deploy.

## Append upload support to `App.hs`

In `src/App.hs`, add this qualified import with the other `Shop` imports:

```haskell
import Shop.Uploads qualified as Uploads
```

Append this registration after the existing transport, services, and queries:

```haskell
  |> Application.withFileUpload @() (\_ -> Uploads.uploadConfig)
```

The `@()` factory is independent of `ShopConfig` for this local example. When the directory and limits become deployment settings, replace the factory with a function from the configuration type registered by `Application.withConfig`.

After completing [the workflow lesson](/connect/workflows/), the following is the complete resulting `src/App.hs`. It preserves the outbound registration and adds uploads. If you are entering this page directly, add the two workflow lines from that lesson at the same locations, or omit them until you complete that earlier page.

<!-- complete-file -->
```haskell title="src/App.hs"
module App (app) where

import Core
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Maybe qualified
import Path qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Shop.Config (ShopConfig (..))
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart
import Shop.Stock.Queries.StockLevel (StockLevel)
import Shop.Stock.Service qualified as Stock
import Shop.Uploads qualified as Uploads

app :: Application
app = Application.new
  |> Application.withConfig @ShopConfig
  |> Application.withEventStore (\(config :: ShopConfig) -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = config.persistEvents
    })
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
  |> Application.withService Stock.service
  |> Application.withQuery @StockLevel
  |> Application.withOutbound @ReserveStockOnItemAdded
  |> Application.withOutbound @ReserveStockOnItemAdded
  |> Application.withFileUpload @() (\_ -> Uploads.uploadConfig)
```

## Upload bytes before attaching them

Stop and rebuild after editing, then start the application from the project root:

```sh
neo build
neo test
neo run
```

In another terminal at the same project root, create a fixture and send it to the running server:

```sh
mkdir -p examples
printf 'Blue mug artwork draft\n' > examples/artwork-note.txt
curl -F 'file=@examples/artwork-note.txt;type=text/plain' \
  http://localhost:8080/files/upload
```

Expect JSON containing `fileRef`, `filename`, `contentType`, `sizeBytes`, and `expiresAt`. This first round trip uses the local application without authentication. If you have enabled authentication, supply credentials as described in [access control](/build/access-control/).

Use the returned reference to request the bytes. Replace the placeholder with the returned `fileRef`:

```sh
curl http://localhost:8080/files/YOUR-FILE-REFERENCE
```

**Current authenticated-download limitation:** the download route uses the `Everyone` middleware mode, which returns anonymous claims even when a token is present. An upload owned by an authenticated subject therefore cannot be assumed downloadable through this route. Verify and resolve that path before enabling private attachments; the anonymous exercise does not establish authenticated ownership support end to end.

## Attach a reference through an accepted action

Uploading bytes has not changed a cart. To add an artwork feature, create a command with an `attachment :: FileRef` field. `FileRef` is the reference type defined in `Service.FileUpload.Core`. Use the command marker from [commands and events](/build/commands-and-events/). The framework resolves the reference before the command runs and provides metadata through `RequestContext.files`.

Keep the file reference in the accepted event together with its association to the cart or artwork request. Do not copy raw bytes into the event. The command, event, and view that display artwork are new application work: create those files before offering an “attach” action on the screen.

The resolver checks file existence, deletion, pending expiry, ownership, and blob presence. Pending references expire; confirmed ones are not rejected merely for pending TTL expiry. The application still needs rules for retention and removal.

The background integration file-access context is different from a user's request context. Its implementation retrieves by reference from storage; it does not carry a requesting user's ownership check. Trigger processing only from an authorised action that validated the association. Do not accept an arbitrary reference from an untrusted prompt and hand it to a background processor.

A declared media type is useful for routing and limits, but does not prove that the bytes are valid artwork or a safe document. Validate the properties your application relies on before accepting them.

## Exercise: a customer's abandoned artwork

In the practice project, decide when artwork becomes attached to an order, what happens after pending expiry, and what the screen shows if the stored bytes are missing. Test a valid owned reference, another user's reference, an expired pending upload, a deleted reference, missing blob bytes, missing multipart data, and an oversized file. Keep the command rejected when its required attachment cannot be resolved. Run those checks with `neo test`, and separately exercise the live HTTP upload route. Add an authenticated ownership scenario before enabling private attachments.

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
