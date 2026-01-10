# ADR-0008: Integration Pattern

## Status

Proposed

## Context

NeoHaskell's event-sourced architecture handles domain logic well, but real applications need to interact with external systems:

- **Outbound**: When events occur, trigger external effects (send emails, publish to message queues, call third-party APIs)
- **Inbound**: When external systems notify us (webhooks, message consumers), translate to domain commands

Currently, there's no standard pattern for these integrations. Developers either:

1. Put side effects directly in command handlers (violates purity, complicates testing)
2. Create ad-hoc subscribers with inconsistent error handling
3. Build custom webhook handlers without command routing

We need a pattern that:

1. **Separates concerns**: Integration logic should not pollute domain logic
2. **Maintains ordering**: Events for the same entity should be processed in order
3. **Handles failures**: Retries, dead-letter handling, monitoring
4. **Enables reuse**: Common integrations (Sendgrid, Stripe, Kafka) should be packageable

### Two Personas

Designing integrations requires balancing two distinct users:

**Nick (Integration Developer)** - Advanced developer who builds integration packages:
- Understands HTTP, retries, circuit breakers, authentication
- Owns the "black box" implementation
- Ships reusable packages (`integration-sendgrid`, `integration-stripe`)

**Jess (Integration User)** - Junior developer configuring integrations for their app:
- Should only write pure mapping functions
- Never sees `Task`, `Http`, or error handling
- Configures what Nick built

This two-persona split is the core insight: Nick handles all effectful complexity; Jess writes pure configuration.

## Decision

We will implement an `Integration` module with two directions:

- `Integration.outbound` - Event happened, trigger external effect, optionally emit command
- `Integration.inbound` - External system notified us, translate to command

### Jess's API (Pure Configuration Only)

Jess configures integrations using pure functions. She never imports `Task`, `Http`, or deals with errors.

**Outbound Example** (Event -> External -> Command):

```haskell
import Integration.Sendgrid qualified as Sendgrid

welcomeEmail :: Sendgrid.Outbound User UserRegistered
welcomeEmail = Sendgrid.outbound
    { toEmail = \user event -> Sendgrid.WelcomeEmail
        { to = user.email
        , templateId = "welcome-v2"
        , variables = Map.fromList
            [ ("name", user.name)
            , ("registeredAt", event.timestamp |> Time.format)
            ]
        }
    , onSuccess = \user event response -> ConfirmEmailSent
        { userId = user.id
        , messageId = response.id
        }
    }
```

What Jess writes:
- `toEmail`: Pure function `entity -> event -> EmailPayload`
- `onSuccess`: Pure function `entity -> event -> response -> command`

What Jess does NOT write:
- HTTP calls
- API key management
- Retry logic
- Error handling

**Inbound Example** (External -> Command):

Inbound integrations are workers that run forever, listening to external sources. The source could be anything: HTTP webhooks, Kafka consumers, filesystem watchers, MQTT, polling, etc.

```haskell
import Integration.Stripe qualified as Stripe

paymentWebhook :: Stripe.Inbound Stripe.PaymentSucceeded
paymentWebhook = Stripe.webhook @Stripe.PaymentSucceeded
    { toCommand = \payment -> RecordPayment
        { orderId = payment.metadata.orderId
        , amount = payment.amount
        , stripePaymentId = payment.id
        }
    }
```

```haskell
import Integration.FileWatcher qualified as FileWatcher

invoiceImporter :: FileWatcher.Inbound
invoiceImporter = FileWatcher.watch
    { folder = "/imports/invoices"
    , pattern = "*.csv"
    , toCommand = \file -> ImportInvoice
        { filePath = file.path
        , detectedAt = file.modifiedAt
        }
    }
```

```haskell
import Integration.Kafka qualified as Kafka

orderConsumer :: Kafka.Inbound
orderConsumer = Kafka.consume
    { topic = "external-orders"
    , groupId = "neohaskell-app"
    , toCommand = \message -> CreateOrder
        { externalId = message.key
        , payload = message.value
        }
    }
```

What Jess writes:
- `toCommand`: Pure function `externalPayload -> command`
- Configuration specific to the integration (folder, topic, etc.)

What Jess does NOT write:
- Worker loop logic
- Connection management
- Authentication/verification
- Error handling and retries

### Nick's API (Black Box Builder)

Nick builds the integration packages that Jess uses. He handles all effectful complexity.

**Outbound Implementation**:

```haskell
module Integration.Sendgrid where

import Integration qualified

data OutboundConfig entity event = OutboundConfig
    { toEmail :: entity -> event -> WelcomeEmail
    , onSuccess :: entity -> event -> SendgridResponse -> command
    }

outbound ::
    forall entity event command.
    OutboundConfig entity event command ->
    Integration.Outbound entity event command
outbound config = Integration.outbound
    { execute = \entity event -> do
        apiKey <- Environment.require "SENDGRID_API_KEY"
        let email = config.toEmail entity event
        response <- Http.post sendgridUrl
            |> Http.bearer apiKey
            |> Http.jsonBody email
            |> Http.send
            |> Task.retry { attempts = 3, backoff = Exponential }
        config.onSuccess entity event response
            |> Integration.emitCommand
    }
```

What Nick owns:
- `execute`: Full `Task` with HTTP, retries, auth
- API-specific types (`WelcomeEmail`, `SendgridResponse`)
- Error classification (retryable vs permanent)
- `Integration.emitCommand` to emit resulting commands

**Inbound Implementation**:

Inbound integrations are workers - long-running `Task` loops. Nick builds the worker; Jess provides the mapper.

```haskell
module Integration.Stripe where

import Integration qualified

data WebhookConfig payload command = WebhookConfig
    { toCommand :: payload -> command
    }

webhook ::
    forall payload command.
    (Stripe.WebhookEvent payload) =>
    WebhookConfig payload command ->
    Integration.Inbound command
webhook config = Integration.inbound
    { run = \emit -> do
        secret <- Environment.require "STRIPE_WEBHOOK_SECRET"
        Http.serve "/webhooks/stripe" \request -> do
            signature <- request |> Http.header "Stripe-Signature"
            payload <- Stripe.verifyAndParse signature secret request.body
            config.toCommand payload |> emit
            Http.respond 200
    }
```

```haskell
module Integration.FileWatcher where

import Integration qualified

data WatchConfig command = WatchConfig
    { folder :: Path
    , pattern :: Text
    , toCommand :: FileChange -> command
    }

watch ::
    forall command.
    WatchConfig command ->
    Integration.Inbound command
watch config = Integration.inbound
    { run = \emit -> do
        FileSystem.watchForever config.folder config.pattern \file -> do
            config.toCommand file |> emit
    }
```

```haskell
module Integration.Kafka where

import Integration qualified

data ConsumeConfig command = ConsumeConfig
    { topic :: Text
    , groupId :: Text
    , toCommand :: KafkaMessage -> command
    }

consume ::
    forall command.
    ConsumeConfig command ->
    Integration.Inbound command
consume config = Integration.inbound
    { run = \emit -> do
        brokers <- Environment.require "KAFKA_BROKERS"
        Kafka.consumeForever brokers config.topic config.groupId \message -> do
            config.toCommand message |> emit
    }
```

What Nick owns:
- The forever-running worker loop
- Connection management and reconnection
- Authentication and verification
- Error handling and logging
- The `emit` callback dispatches commands to the domain

### Core Types

```haskell
module Service.Integration.Core where

-- Outbound: Event -> External -> Maybe Command
data Outbound entity event command = Outbound
    { execute :: entity -> event -> Task IntegrationError (Maybe command)
    }

-- Inbound: Worker that runs forever, emitting commands
-- The `run` function receives an `emit` callback to dispatch commands
data Inbound command = Inbound
    { run :: (command -> Task IntegrationError Unit) -> Task IntegrationError Void
    }

-- Combined type for registration
data Integration
    = OutboundIntegration (exists entity event command. Outbound entity event command)
    | InboundIntegration (exists command. Inbound command)

-- Emit a command from outbound integration logic
emitCommand :: command -> Task IntegrationError (Maybe command)
emitCommand cmd = Task.yield (Just cmd)

-- No command to emit (fire-and-forget outbound)
noCommand :: Task IntegrationError (Maybe command)
noCommand = Task.yield Nothing

-- Errors
data IntegrationError
    = NetworkError Text
    | AuthenticationError Text
    | ValidationError Text
    | RateLimited RetryAfter
    | PermanentFailure Text
```

The key insight for `Inbound`:
- `run` takes an `emit` callback that the worker uses to dispatch commands
- `run` returns `Task IntegrationError Void` - it runs forever (never returns)
- The Application spawns each inbound worker as a background task
- Workers can be HTTP servers, message consumers, file watchers, or anything else

### Module Structure

```text
core/service/
  Service/
    Integration.hs              -- Re-export wrapper
    Integration/
      Core.hs                   -- Outbound, Inbound, Integration types
      Outbound.hs               -- Outbound execution machinery
      Inbound.hs                -- Inbound webhook routing
      Registry.hs               -- Integration registration and lookup
```

Integration packages (future, separate repos):

```text
integration-sendgrid/
  Integration/
    Sendgrid.hs                 -- Sendgrid.outbound, email types
    Sendgrid/
      Types.hs                  -- WelcomeEmail, SendgridResponse, etc.

integration-stripe/
  Integration/
    Stripe.hs                   -- Stripe.webhook, event types
    Stripe/
      Types.hs                  -- PaymentSucceeded, etc.
      Verification.hs           -- Webhook signature verification
```

### Ordering Guarantees

**Outbound integrations guarantee in-order processing per entity**:

1. Events are processed in stream order (by `StreamPosition`)
2. Events for entity A are processed sequentially
3. Events for different entities may be processed concurrently

This prevents race conditions where email 2 sends before email 1 for the same user.

Implementation: The outbound processor groups events by entity and processes each entity's events sequentially while allowing cross-entity parallelism.

### Error Handling

**Two-level error handling**:

1. **Global handlers** - Application-wide defaults for all integrations
2. **Per-integration overrides** - Specific handling for individual integrations

```haskell
app :: Application
app =
    Application.new
        |> Application.withIntegration welcomeEmail
        |> Application.withIntegration paymentWebhook
        |> Application.onIntegrationError globalErrorHandler

-- Global handler
globalErrorHandler :: IntegrationError -> Task Void Unit
globalErrorHandler error = case error of
    RateLimited retryAfter ->
        -- Log and schedule retry
        Logger.warn [fmt|Rate limited, retry after {retryAfter}|]
    PermanentFailure reason ->
        -- Dead letter queue
        DeadLetterQueue.enqueue error
    _ ->
        -- Default: log and continue
        Logger.error [fmt|Integration error: {error}|]
```

Per-integration overrides are configured in Nick's package:

```haskell
outbound config = Integration.outbound
    { execute = ...
    , onError = \error -> case error of
        RateLimited _ -> Integration.retry { after = Seconds 60 }
        AuthenticationError _ -> Integration.halt  -- Don't retry auth failures
        _ -> Integration.useGlobalHandler
    }
```

### Application Registration

```haskell
app :: Application
app =
    Application.new
        |> Application.withService userService
        |> Application.withService orderService
        -- Outbound: subscribes to events, triggers external effects
        |> Application.withIntegration welcomeEmail
        |> Application.withIntegration orderConfirmation
        -- Inbound: registers webhook endpoints
        |> Application.withIntegration paymentWebhook
        |> Application.withIntegration shipmentWebhook
        |> Application.run
```

The Application layer:
- Subscribes outbound integrations to the EventStore
- Registers inbound integrations as HTTP endpoints
- Wires up error handlers

## Consequences

### Positive

1. **Clean separation of concerns**: Domain logic stays pure; integration complexity is isolated in packages.

2. **Two-persona design works**: Nick handles effectful complexity once; Jess reuses safely across many apps.

3. **Testable at both levels**:
   - Jess's pure mappers: Unit test with simple inputs/outputs
   - Nick's integrations: Integration test with mocked HTTP

4. **Reusable packages**: Common integrations (Sendgrid, Stripe, Twilio, Kafka) can be packaged and shared.

5. **Consistent patterns**: All integrations follow the same structure, making the codebase predictable.

6. **Ordering guarantees**: In-order processing per entity prevents subtle race conditions.

7. **Flexible error handling**: Global defaults with per-integration overrides covers most use cases.

### Negative

1. **Two layers to understand**: Contributors must grok both Nick's and Jess's APIs.

2. **Package proliferation**: Each external service needs its own package (though this is also a feature - clear boundaries).

3. **Eventual consistency**: Commands emitted from integrations are async, so reads immediately after may not reflect changes.

4. **Existential types in Integration**: The combined `Integration` type uses existentials, which can complicate debugging.

### Trade-offs

1. **Purity over flexibility**: Jess cannot do anything effectful. This is intentional - if she needs effects, she becomes Nick.

2. **Per-entity ordering over throughput**: Strict ordering per entity sacrifices some parallelism. This is the right default for correctness.

3. **Separate packages over monorepo**: Integration packages will eventually live outside core. This adds distribution complexity but enables independent versioning.

## Future Work

1. **Dead Letter Queue**: Standard DLQ integration for permanently failed events.

2. **Circuit Breaker**: Automatic circuit breaking when external services are down.

3. **Metrics/Observability**: Built-in metrics for integration latency, error rates.

4. **Batch Processing**: Support for batching multiple events into single external calls.

5. **Integration Testing Framework**: Helpers for testing integrations with mocked external services.

## References

- [ADR-0004: EventStore Abstraction](0004-eventstore-abstraction.md) - Event subscription infrastructure
- [ADR-0005: Service Module Reorganization](0005-service-module-reorganization.md) - Module structure patterns
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md) - Async subscriber pattern
- Reactive Manifesto: https://www.reactivemanifesto.org/ - Resilience patterns
- Enterprise Integration Patterns: https://www.enterpriseintegrationpatterns.com/
