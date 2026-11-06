# Reusable side effects

Use an outbound integration to react to an accepted domain event. The application handler constructs an `Integration.Outbound`. An integration package converts a configuration record into an executable `Integration.Action` through `ToAction`. The runtime executes that action and dispatches any resulting command.

## A reusable HTTP operation

The data shape is a delivery configuration parameterized by its result command. The package owns endpoint construction and transport settings. The application supplies domain callbacks. This explanatory fragment needs imports, extensions, and a package module.

```haskell ignore
data Delivery command = Delivery
	{ destination :: Text
	, payload :: Json.Value
	, onDelivered :: command
	, onFailed :: Text -> command
	}

instance
	(Json.ToJSON command, KnownSymbol (NameOf command)) =>
	Integration.ToAction (Delivery command)
	where
	toAction delivery =
		Integration.toAction Http.Request
			{ method = Http.POST
			, url = delivery.destination
			, headers = []
			, body = Http.json delivery.payload
			, onSuccess = \_response -> delivery.onDelivered
			, onError = Just delivery.onFailed
			, auth = Http.Bearer "${DELIVERY_API_TOKEN}"
			, retry = Http.defaultRetry
			, timeoutSeconds = 30
			}
```

`Http` here names `Integration.Http`. Its `Request command` record supports a method, URL, headers, body, success callback, optional error callback, authentication, retry settings, and timeout. `onSuccess` and `onError` return the same command type. A sum type can represent both outcomes.

Use `Http.json`, `Http.form`, `Http.raw`, or `Http.noBody` for the body. Authentication supports `NoAuth`, `Bearer`, `Basic`, and `ApiKey`. URL, header values, and authentication values support runtime `${VARIABLE}` expansion.

For operations beyond HTTP configuration, implement `toAction` with `Integration.action`. The action receives `ActionContext` and returns a `Task IntegrationError (Maybe CommandPayload)`. Use `Integration.emitCommand` for a result command and `Integration.noCommand` for no command.

## Application wiring

Define a handler type and its `EntityOf` equation. Write `handleEvent :: Entity -> Payload -> Integration.Outbound` before `outboundIntegration ''Handler`. The payload needs an `EventVariantOf` mapping to the entity's stored event type. `deriveEntityEvents` generates that mapping for listed payloads. The runtime calls this handler only when the stored event contains its payload. Return `Integration.batch [Integration.outbound delivery]` to run the effect.

For an existing handler that still takes the full event type, add `import Service.OutboundIntegration.TH (outboundIntegrationFullEvent)` to its module and replace its marker with `outboundIntegrationFullEvent ''Handler`. This explicit compatibility path still sends every event to that handler. Move one event case into a concrete payload handler when you want payload-specific dispatch. For a handwritten event sum, define an `EventVariantOf` mapping for the payload without changing the stored event JSON.

For a model-backed application, declare an outbound Slice with `outboundIntegration @Handler`, one `onEvent @Payload`, and each callback's `triggersCommand @Command do { onTargetEntity @Entity }`. Declare each callback command in its own state-change Slice with `entity @Entity` and `command @Command`. The Event Model registers the handler and commands from those Slices. See [source-local Event Models](event-model.md) for the full declaration order. The callbacks need `Json.ToJSON` and `NameOf` to form command payloads. Their handlers still need valid routing and decisions.

For a legacy application assembled directly with `Application`, keep `Application.withOutbound @Handler` and register callback commands in their owning services. These manual registrations remain supported and are not deprecated.

Keep networking outside `decide` and `update`. The command decision describes domain changes. The outbound action performs the remote effect after an event.

## Store a server-created document

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

## Inbound workers with more than one command

Use [typed inbound workers](advanced-typed-inbound.md) when one worker emits commands to more than one entity.

## Retries and result validation

Treat a retried remote operation as repeatable work. Use a provider-supported idempotency key when duplicate requests would cause duplicate effects. The HTTP executor has retry behavior and the dispatcher separately owns action execution. Neither establishes a transaction with the remote service.

Validate response status and payload before treating an HTTP response as a domain success. The current executor sends an `Ok response` to `onSuccess`; that callback name alone is not a domain guarantee. A malformed payload must become an explicit domain failure rather than a fabricated identifier.

## The separate adapter API

`Service.Integration.Adapter.Integration request` defines associated `Response request`, `runReal`, and `runFake`. `EventModel.withIntegration @Request` registers this API in the named model for typed dispatch. Direct `Application.withIntegration @Request` remains available to applications that use manual assembly. Real, fake, and hybrid selection use application arguments such as `--integrations=fake`. Hybrid mode names fakes with `--fake=TypeName`.

Registering `withIntegration` does not replace the `ToAction` implementation for `Http.Request`. The built-in HTTP `ToAction` executes its HTTP path directly. Choose the owning API when designing a reusable package.

To test a reusable HTTP effect, send requests to a deterministic fake endpoint. Assert the outbound payload and idempotency key, repeat the request, and check the resulting command and domain state. Test a rejected response as a domain failure.
