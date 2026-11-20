# Server-created files

Use `createFile` when an integration renders a PDF or produces another document as bytes. Enable file storage with the named Event Model's `withFileUpload` configuration factory. An application assembled manually uses `Application.withFileUpload`. Both provide `ActionContext.fileAccess` to integration actions. See [configuration and wiring](configuration-and-wiring.md) for configuration factories.

The request is an `Integration.CreateFileRequest` with `owner`, `creationKey`, `filename`, `contentType`, and `content`. The operation returns `Task Integration.FileCreationError Integration.FileRef`. This fragment belongs inside `Integration.action`. It assumes an authorized event payload, rendered `pdfBytes`, and an application completion command named `RecordDocument`.

```haskell ignore
files <- case context.fileAccess of
	Nothing -> Task.throw (Integration.ValidationError "File storage is not configured")
	Just access -> Task.yield access

fileRef <- files.createFile Integration.CreateFileRequest
	{ owner = payload.ownerId
	, creationKey = payload.documentJobKey
	, filename = "contract.pdf"
	, contentType = "application/pdf"
	, content = pdfBytes
	}
	|> Task.mapError (\_ -> Integration.UnexpectedError "Document storage failed")

Integration.emitCommand RecordDocument
	{ documentId = payload.documentId
	, fileRef = fileRef
	}
```

Import `Integration` and `Task` qualified. Your command must authorize the document request and capture the authenticated user's ID as its owner. Persist that owner and the document job key in the event consumed by the integration. Integration actions have trusted server access. `createFile` trusts the supplied owner and does not infer a signed-in user. The authenticated download route checks the downloader against that owner.

Choose one stable `creationKey` for a logical document job or revision. Use the same key on retries and a new key for a new revision. Keys are scoped to their owner. The configured file size limit and allowed content types apply to each request.

A successful call returns a retained `FileRef` with no temporary-upload expiry. A retry with valid input and unchanged metadata returns the same reference when the retained file exists. It keeps the first completed bytes even when the renderer supplies different bytes. Reusing the key with a different valid content type or stored filename fails with `Integration.FileCreationConflict`. The `storeOriginalFilename` configuration still controls whether the filename is stored.

Before creation completes, a retry must supply the original bytes. Repairing a missing retained file also requires the original bytes. Different bytes fail with `Integration.FileCreationConflict`. If you cannot reproduce those bytes, create an explicit new document job.

Record the returned reference through a completion command. Keep the document bytes out of domain events. File creation and completion are separate operations. An abandoned job can leave a retained file, so the application or operator must explicitly reconcile abandoned jobs. Temporary-upload cleanup does not remove these files. This operation adds no file deletion API.
