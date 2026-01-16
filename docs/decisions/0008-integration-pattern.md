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

We will implement an `Integration` module with:

- **Per-entity integration functions**: One function per entity that pattern matches events and triggers integrations
- **Typeclass-based action conversion**: Nick defines config record types with `ToAction` instances; Jess instantiates these records
- **Array-based batching**: Collect multiple heterogeneous actions into an `Array` using `Integration.outbound` for type erasure

### Jess's API (Per-Entity Integration Function)

Jess writes ONE integration function per entity. It pattern matches on the event ADT and returns integration actions:

```haskell
-- User/Integrations.hs
module User.Integrations where

import User (User)
import User.Event (UserEvent(..))
import Notification.Commands (CreateWelcomeNotification(..))
import Audit.Commands (LogAuditEvent(..))
import Integration qualified
import Integration.Sendgrid qualified as Sendgrid
import Integration.Slack qualified as Slack

userIntegrations :: User -> UserEvent -> Integration.Outbound
userIntegrations user event = case event of
  UserRegistered info -> Integration.batch
    [ Integration.outbound Sendgrid.Email
        { to = user.email
        , templateId = "welcome-v2"
        , variables = [("name", info.name)]
        , onSuccess = \response -> CreateWelcomeNotification
            { userId = user.id
            , messageId = response.id
            }
        }
    , Integration.outbound Slack.Message
        { channel = "#signups"
        , text = [fmt|New user: {info.name} ({user.email})|]
        }
    ]

  EmailChanged info -> Integration.batch
    [ Integration.outbound Sendgrid.Email
        { to = info.oldEmail
        , templateId = "email-changed"
        , variables = [("newEmail", info.newEmail)]
        , onSuccess = \_ -> LogAuditEvent
            { entityType = "User"
            , entityId = user.id
            , action = "email_changed"
            }
        }
    ]

  _ -> Integration.none
```

**What Jess writes:**

- Pattern match on event ADT
- For each relevant event, create config records defined by Nick
- Wrap each config with `Integration.outbound` for type erasure
- Pass array to `Integration.batch`
- `Integration.none` for events with no integrations

**What Jess does NOT write:**

- HTTP calls, retries, authentication
- Worker loops, connection management
- Error handling

### Nick's API (Config Records + ToAction)

Nick builds packages that expose **config record types** with `ToAction` instances. The typeclass handles the conversion from pure config to effectful action:

```haskell
-- integration-sendgrid/Integration/Sendgrid.hs
module Integration.Sendgrid
  ( Email(..)
  , SendgridResponse(..)
  ) where

import Integration qualified

data SendgridResponse = SendgridResponse
  { id :: Text
  , status :: Text
  }

-- Config record that Jess instantiates
data Email command = Email
  { to :: Text
  , templateId :: Text
  , variables :: Array (Text, Text)
  , onSuccess :: SendgridResponse -> command
  }

-- Nick implements the effectful conversion
instance (ToJSON command, Typeable command) => Integration.ToAction (Email command) where
  toAction config = Integration.action do
    apiKey <- Environment.require "SENDGRID_API_KEY"
    response <- Http.post "https://api.sendgrid.com/v3/mail/send"
      |> Http.bearer apiKey
      |> Http.jsonBody config
      |> Http.send
      |> Task.retry { attempts = 3, backoff = Exponential }
    Integration.emitCommand (config.onSuccess response)
```

**Key insight**: Jess only sees the `Email` record type. The `ToAction` instance is invisible to her - she just instantiates the record and wraps it with `Integration.outbound`.

### Core Types

```haskell
module Service.Integration.Outbound where

-- Typeclass for converting config records to actions
class ToAction config where
  toAction :: config -> Action

-- Opaque collection of outbound actions (heterogeneous command types)
newtype Outbound = Outbound (Array Action)

-- Single action (type-erased)
newtype Action = Action ActionInternal

data ActionInternal = ActionInternal
  { execute :: Task IntegrationError (Maybe CommandPayload)
  }

data CommandPayload = CommandPayload
  { commandType :: Text
  , commandData :: Json.Value
  }

-- Convert config to type-erased action (used by Jess)
outbound :: forall config. (ToAction config) => config -> Action
outbound config = toAction config

-- Collect actions into an Outbound value
batch :: Array Action -> Outbound
batch actions = Outbound actions

-- No integrations for this event
none :: Outbound
none = Outbound []

-- Create an action from a Task (used by Nick in ToAction instances)
action :: Task IntegrationError (Maybe CommandPayload) -> Action
action task = Action (ActionInternal task)

-- Command emission helpers (used inside ToAction instances)
emitCommand :: forall command. (ToJSON command, Typeable command) => command -> Task IntegrationError (Maybe CommandPayload)
emitCommand cmd = Task.yield (Just payload)
  where
    payload = CommandPayload
      { commandType = Text.pack (show (typeRep (Proxy @command)))
      , commandData = Json.toValue cmd
      }

noCommand :: Task IntegrationError (Maybe CommandPayload)
noCommand = Task.yield Nothing

-- Errors
data IntegrationError
  = NetworkError Text
  | AuthenticationError Text
  | ValidationError Text
  | RateLimited RetryAfter
  | PermanentFailure Text
```

### Inbound Integrations

Inbound integrations are workers that run forever, listening to external sources. They remain record-based since they don't have entity/event context:

```haskell
-- Jess configures
paymentWebhook :: Integration.Inbound
paymentWebhook = Stripe.webhook
  { toCommand = \payment -> RecordPayment
      { orderId = payment.metadata.orderId
      , amount = payment.amount
      }
  }

fileWatcher :: Integration.Inbound
fileWatcher = FileWatcher.watch
  { folder = "/imports/invoices"
  , pattern = "*.csv"
  , toCommand = \file -> ImportInvoice { filePath = file.path }
  }
```

**Nick builds the worker:**

```haskell
-- integration-stripe/Integration/Stripe.hs
data WebhookConfig command = WebhookConfig
  { toCommand :: StripePayment -> command
  }

webhook ::
  forall command.
  (ToJSON command, Typeable command) =>
  WebhookConfig command ->
  Integration.Inbound
webhook config = Integration.inbound
  { run = \emit -> do
      secret <- Environment.require "STRIPE_WEBHOOK_SECRET"
      Http.serve "/webhooks/stripe" \request -> do
        signature <- request |> Http.header "Stripe-Signature"
        payload <- Stripe.verifyAndParse signature secret request.body
        emit (config.toCommand payload)
        Http.respond 200
  }
```

### Module Structure

```text
core/service/
  Integration.hs                    -- Re-export wrapper (top-level entry point)
  Integration/
    Command.hs                      -- Command integration (dispatching commands from integrations)
    Lifecycle.hs                    -- Lifecycle management (start/stop/health)
    Timer.hs                        -- Timer-based integrations (scheduled tasks)
  Service/
    Integration/
      Dispatcher.hs                 -- Core dispatching logic (routes commands/events)
      Types.hs                      -- Shared integration types
```

**Conceptual mapping from ADR to implementation:**

| ADR Concept | Implemented In |
|-------------|----------------|
| Outbound type, ToAction, batch | `Integration` (re-exports), `Service.Integration.Types` |
| Inbound type and helpers | `Integration.Command` |
| Event subscription / dispatching | `Service.Integration.Dispatcher` |
| Worker management / lifecycle | `Integration.Lifecycle` |
| Scheduled / timed integrations | `Integration.Timer` |

### Integration Store (Position Tracking)

Track last processed position **per entity per integration** for at-least-once semantics:

```haskell
data IntegrationStore = IntegrationStore
  { getPosition :: IntegrationName -> StreamId -> Task Error (Maybe StreamPosition)
  , setPosition :: IntegrationName -> StreamId -> StreamPosition -> Task Error Unit
  }
```

**Only store position for events where integrations actually ran** (not for skipped events).

### Application Registration

```haskell
app :: Application
app =
  Application.new
    |> Application.withService userService
    |> Application.withService orderService
    -- Outbound: per-entity integration functions (returns Integration.Outbound)
    |> Application.withOutbound @User userIntegrations
    |> Application.withOutbound @Order orderIntegrations
    -- Inbound: webhook/worker registrations
    |> Application.withInbound paymentWebhook
    |> Application.withInbound fileWatcher
    -- Error handling
    |> Application.onIntegrationError globalErrorHandler
```

### Ordering Guarantees

**Outbound integrations guarantee in-order processing per entity**:

1. Events for entity A are processed sequentially (A1 -> A2 -> A3)
2. Events for entity B are processed sequentially (B1 -> B2 -> B3)
3. A and B can be processed concurrently (cross-entity parallelism)

Implementation: Per-entity queues with dedicated workers.

### Error Handling

**Two-level error handling**:

1. **Global handlers** - Application-wide defaults
2. **Per-action overrides** - Nick can configure in ToAction instances

```haskell
app =
  Application.new
    |> Application.withOutbound @User userIntegrations
    |> Application.onIntegrationError \error -> case error of
        RateLimited retryAfter -> Logger.warn [fmt|Rate limited: {retryAfter}|]
        PermanentFailure reason -> DeadLetterQueue.enqueue error
        _ -> Logger.error [fmt|Integration error: {error}|]
```

### Cross-Service Commands

Integrations can emit commands to **any service**, not just the entity's own service:

```haskell
-- User/Integrations.hs
import Notification.Commands (CreateWelcomeNotification(..))  -- Different service!
import Audit.Commands (LogAuditEvent(..))                     -- Another service!

userIntegrations user event = case event of
  UserRegistered info -> Integration.batch
    [ Integration.outbound Sendgrid.Email
        { ...
        , onSuccess = \response -> CreateWelcomeNotification { ... }  -- -> Notification service
        }
    , Integration.outbound Slack.Message
        { ...
        , onSuccess = \_ -> LogAuditEvent { ... }  -- -> Audit service
        }
    ]
```

The command dispatcher routes based on command type name, not entity association.

## Consequences

### Positive

1. **Single source of truth**: All integrations for an entity are in one file. Easy to discover.

2. **Natural Haskell pattern**: Pattern matching on event ADT is idiomatic.

3. **Compiler-checked exhaustiveness**: With `-Wincomplete-patterns`, compiler warns about unhandled events.

4. **Explicit "nothing happens"**: `_ -> Integration.none` makes absence of integrations visible.

5. **Cross-service flexibility**: Commands can target any service via type-based routing.

6. **Two-persona design intact**: Nick builds config types with ToAction; Jess instantiates records.

7. **Testable**: Entity integration function is pure - test with different event inputs.

8. **Declarative config**: Jess writes plain record instantiation, no monadic syntax or function calls.

### Negative

1. **Domain imports infrastructure**: `User/Integrations.hs` imports `Integration.Sendgrid`. Creates dependency arrow.

2. **Modification required for new integrations**: Adding integration modifies existing file (vs adding new file).

3. **Type erasure complexity**: `Integration.Outbound` hides command types, which can complicate debugging.

### Trade-offs

1. **Discoverability over modularity**: Per-entity is easier to understand but harder to add to independently.

2. **Simplicity over type safety**: Type-erased actions enable heterogeneous collections at cost of compile-time command type checking.

## Future Work

1. **Dead Letter Queue**: Standard DLQ for permanently failed integrations.

2. **Circuit Breaker**: Automatic circuit breaking when external services are down.

3. **Metrics/Observability**: Built-in metrics for integration latency, error rates.

4. **Batch Processing**: Support for batching multiple events into single external calls.

## References

- [ADR-0004: EventStore Abstraction](0004-eventstore-abstraction.md) - Event subscription infrastructure
- [ADR-0005: Service Module Reorganization](0005-service-module-reorganization.md) - Module structure patterns
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md) - Async subscriber pattern
