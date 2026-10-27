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

For an existing handler that still takes the full event type, replace its marker with `outboundIntegrationFullEvent ''Handler`. This explicit compatibility path still sends every event to that handler. Move one event case into a concrete payload handler when you want payload-specific dispatch. For a handwritten event sum, define an `EventVariantOf` mapping for the payload without changing the stored event JSON.

Register the handler with `Application.withOutbound @Handler`. Register the callback commands in their owning services. The callbacks need `Json.ToJSON` and `NameOf` to form command payloads. Their handlers still need valid routing and decisions.

Keep networking outside `decide` and `update`. The command decision describes domain changes. The outbound action performs the remote effect after an event.

## Retries and result validation

Treat a retried remote operation as repeatable work. Use a provider-supported idempotency key when duplicate requests would cause duplicate effects. The HTTP executor has retry behavior and the dispatcher separately owns action execution. Neither establishes a transaction with the remote service.

Validate response status and payload before treating an HTTP response as a domain success. The current executor sends an `Ok response` to `onSuccess`; that callback name alone is not a domain guarantee. A malformed payload must become an explicit domain failure rather than a fabricated identifier.

## The separate adapter API

`Service.Integration.Adapter.Integration request` defines associated `Response request`, `runReal`, and `runFake`. `Application.withIntegration @Request` registers this API for typed dispatch. Real, fake, and hybrid selection use application arguments such as `--integrations=fake`. Hybrid mode names fakes with `--fake=TypeName`.

Registering `withIntegration` does not replace the `ToAction` implementation for `Http.Request`. The built-in HTTP `ToAction` executes its HTTP path directly. Choose the owning API when designing a reusable package.

To test a reusable HTTP effect, send requests to a deterministic fake endpoint. Assert the outbound payload and idempotency key, repeat the request, and check the resulting command and domain state. Test a rejected response as a domain failure.
