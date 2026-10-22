# Reusable side effects

Use an outbound integration to react to an accepted domain event. The application handler constructs an `Integration.Outbound`. An integration package converts a configuration record into an executable `Integration.Action` through `ToAction`. The runtime executes that action and dispatches any resulting command. See `core/service/Integration.hs` and `core/service/Service/Integration/Dispatcher.hs`.

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

`Http` here names `Integration.Http`. Its `Request command` record supports a method, URL, headers, body, success callback, optional error callback, authentication, retry settings, and timeout. `onSuccess` and `onError` return the same command type. A sum type can represent both outcomes. See `integrations/Integration/Http/Request.hs` and `integrations/Integration/Http/Internal.hs`.

Use `Http.json`, `Http.form`, `Http.raw`, or `Http.noBody` for the body. Authentication supports `NoAuth`, `Bearer`, `Basic`, and `ApiKey`. URL, header values, and authentication values support runtime `${VARIABLE}` expansion. See `integrations/Integration/Http/Request.hs`, `integrations/Integration/Http/Auth.hs`, and `integrations/Integration/Http/Internal.hs`.

For operations beyond HTTP configuration, implement `toAction` with `Integration.action`. The action receives `ActionContext` and returns a `Task IntegrationError (Maybe CommandPayload)`. Use `Integration.emitCommand` for a result command and `Integration.noCommand` for no command. See `core/service/Integration.hs`.

## Application wiring

Define a handler type and its `EntityOf` equation. Write `handleEvent :: Entity -> Event -> Integration.Outbound` before `outboundIntegration ''Handler`. Match the event that triggers delivery. Return `Integration.batch [Integration.outbound delivery]` for that event and `Integration.none` for unrelated events. See `core/service/Service/OutboundIntegration/TH.hs` and `testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs`.

Register the handler with `Application.withOutbound @Handler`. Register the callback commands in their owning services. The callbacks need `Json.ToJSON` and `NameOf` to form command payloads. Their handlers still need valid routing and decisions. See `core/service/Service/Application.hs`, `core/service/Integration.hs`, and [commands](commands.md).

Keep networking outside `decide` and `update`. The command decision describes domain changes. The outbound action performs the remote effect after an event. See `core/service/Service/Command/Core.hs`, `core/service/Service/Entity/Core.hs`, and `core/service/Service/Integration/Dispatcher.hs`.

## Retries and result validation

Treat a retried remote operation as repeatable work. Use a provider-supported idempotency key when duplicate requests would cause duplicate effects. The HTTP executor has retry behavior and the dispatcher separately owns action execution. Neither establishes a transaction with the remote service. See `integrations/Integration/Http/Internal.hs` and `core/service/Service/Integration/Dispatcher.hs`.

Validate response status and payload before treating an HTTP response as a domain success. The current executor sends an `Ok response` to `onSuccess`; that callback name alone is not a domain guarantee. A malformed payload must become an explicit domain failure rather than a fabricated identifier. See `integrations/Integration/Http/Internal.hs` and `integrations/Integration/Http/Response.hs`.

## The separate adapter API

`Service.Integration.Adapter.Integration request` defines associated `Response request`, `runReal`, and `runFake`. `Application.withIntegration @Request` registers this API for typed dispatch. Real, fake, and hybrid selection use application arguments such as `--integrations=fake`. Hybrid mode names fakes with `--fake=TypeName`. See `core/service/Service/Integration/Adapter.hs`, `core/service/Service/Integration/Selection.hs`, and `core/service/Service/Application.hs`.

Registering `withIntegration` does not replace the `ToAction` implementation for `Http.Request`. The built-in HTTP `ToAction` executes its HTTP path directly. Choose the owning API when designing a reusable package. See `integrations/Integration/Http/Internal.hs` and `core/test/Service/Integration/WireupSpec.hs`.

The stock reservation Hurl scenario proves local command dispatch between Cart and Stock. The generic HTTP side effect scenario exercises a consumer-owned `ToAction` mapping against a deterministic fake endpoint. It asserts the outbound payload and idempotency key, a duplicate request, the callback's domain result, and a rejected response that becomes a domain failure. See `testbed/tests/scenarios/stock-reservation.hurl`, `testbed/tests/integrations/generic-http-side-effect.hurl`, and `testbed/src/Testbed/ExternalEffect/`. Use that scenario as the pattern for a new remote integration.
