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
- **Builder pattern**: Collect multiple heterogeneous actions into an opaque `Integration.Outbound` type
- **Action builders**: Nick's packages expose action builders that accept command mappers

### Jess's API (Per-Entity Integration Function)

Jess writes ONE integration function per entity. It pattern matches on the event ADT and returns integration actions:

```haskell
-- User/Integrations.hs
module User.Integrations where

import User (User)
import User.Event (UserEvent(..))
import User.Event qualified as UserEvent
import Notification.Commands (CreateWelcomeNotification(..))
import Audit.Commands (LogAuditEvent(..))
import Integration qualified
import Integration.Sendgrid qualified as Sendgrid
import Integration.Slack qualified as Slack

userIntegrations :: User -> UserEvent -> Integration.Outbound
userIntegrations user event = case event of
  UserRegistered data -> Integration.Outbound.batch do
    Sendgrid.email
      { to = user.email
      , templateId = "welcome-v2"
      , variables = [("name", data.name)]
      }
      (\response -> CreateWelcomeNotification
        { userId = user.id
        , messageId = response.id
        })

    Slack.message
      { channel = "#signups"
      , text = [fmt|New user: {data.name} ({user.email})|]
      }
      Integration.Outbound.noCommand

  EmailChanged data -> Integration.Outbound.batch do
    Sendgrid.email
      { to = data.oldEmail
      , templateId = "email-changed"
      , variables = [("newEmail", data.newEmail)]
      }
      (\_ -> LogAuditEvent
        { entityType = "User"
        , entityId = user.id
        , action = "email_changed"
        })

  _ -> Integration.Outbound.none
```

**What Jess writes:**
- Pattern match on event ADT
- For each relevant event, call Nick's action builders with payloads
- Provide command mapper: `response -> command` (can be from ANY service)
- `Integration.Outbound.noCommand` when no follow-up command needed
- `Integration.Outbound.none` for events with no integrations

**What Jess does NOT write:**
- HTTP calls, retries, authentication
- Worker loops, connection management
- Error handling

### Event Matchers (TH Generated)

Since events are ADTs, each constructor needs an associated data type and matcher function. TH generates these:

```haskell
-- User/Event.hs
data UserEvent
  = UserRegistered { email :: Text, name :: Text }
  | EmailChanged { oldEmail :: Text, newEmail :: Text }
  | ProfileUpdated { bio :: Text }
  deriving (Eq, Show, Generic)

-- TH generates data types and matchers
deriveEventMatchers ''UserEvent
```

**Generated code:**

```haskell
data UserRegisteredData = UserRegisteredData
  { email :: Text
  , name :: Text
  }
  deriving (Eq, Show, Generic)

data EmailChangedData = EmailChangedData
  { oldEmail :: Text
  , newEmail :: Text
  }
  deriving (Eq, Show, Generic)

matchUserRegistered :: UserEvent -> Maybe UserRegisteredData
matchUserRegistered event = case event of
  UserRegistered {email, name} -> Just UserRegisteredData {email, name}
  _ -> Nothing

matchEmailChanged :: UserEvent -> Maybe EmailChangedData
matchEmailChanged event = case event of
  EmailChanged {oldEmail, newEmail} -> Just EmailChangedData {oldEmail, newEmail}
  _ -> Nothing
```

**Usage with qualified imports:**
```haskell
import User.Event qualified as UserEvent

case UserEvent.matchUserRegistered event of
  Just data -> -- use data.email, data.name
  Nothing -> -- not this event type
```

### Nick's API (Action Builders)

Nick builds packages that expose **action builders**. Each builder:
- Takes a payload (what to send externally)
- Takes a command mapper (what command to emit on success)
- Returns a type-erased `Integration.Action`

```haskell
-- integration-sendgrid/Integration/Sendgrid.hs
module Integration.Sendgrid
  ( EmailPayload(..)
  , SendgridResponse(..)
  , email
  ) where

import Integration qualified

data EmailPayload = EmailPayload
  { to :: Text
  , templateId :: Text
  , variables :: Array (Text, Text)
  }

data SendgridResponse = SendgridResponse
  { id :: Text
  , status :: Text
  }

-- Action builder: payload + command mapper -> Action
email ::
  forall command.
  (ToJSON command, Typeable command) =>
  EmailPayload ->
  (SendgridResponse -> command) ->
  Integration.Action
email payload toCommand = Integration.action do
  apiKey <- Environment.require "SENDGRID_API_KEY"
  response <- Http.post "https://api.sendgrid.com/v3/mail/send"
    |> Http.bearer apiKey
    |> Http.jsonBody payload
    |> Http.send
    |> Task.retry { attempts = 3, backoff = Exponential }
  Integration.emitCommand (toCommand response)
```

**Key insight**: The `command` type parameter is erased inside `Integration.Outbound.Action`. Nick's code serializes the command to JSON immediately, so the `Outbound` collection can hold heterogeneous command types.

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

### Core Types

```haskell
module Service.Integration.Outbound where

-- Opaque collection of outbound actions (heterogeneous command types)
newtype Outbound = Outbound (Array ActionInternal)

data ActionInternal = ActionInternal
  { execute :: Task IntegrationError (Maybe CommandPayload)
  }

data CommandPayload = CommandPayload
  { commandType :: Text
  , commandData :: Json.Value
  }

-- Single action (type-erased)
newtype Action = Action ActionInternal

-- Builder for collecting outbound actions
batch :: Builder () -> Outbound
none :: Outbound

-- Inside Builder monad, add actions
action :: Task IntegrationError (Maybe CommandPayload) -> Action

-- Command emission helpers
emitCommand :: (ToJSON command, Typeable command) => command -> Task IntegrationError (Maybe CommandPayload)
noCommand :: Task IntegrationError (Maybe CommandPayload)

-- Inbound worker
data Inbound = Inbound
  { run :: (CommandPayload -> Task IntegrationError Unit) -> Task IntegrationError Void
  }

-- Errors
data IntegrationError
  = NetworkError Text
  | AuthenticationError Text
  | ValidationError Text
  | RateLimited RetryAfter
  | PermanentFailure Text
```

### Module Structure

```text
core/service/
  Service/
    Integration.hs              -- Re-export wrapper
    Integration/
      Outbound.hs               -- Outbound type, batch, none, action builders
      Inbound.hs                -- Inbound type and helpers
      Subscriber.hs             -- Outbound event subscription
      Worker.hs                 -- Inbound worker management
      Store.hs                  -- Position tracking per entity
```

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
2. **Per-action overrides** - Nick can configure in action builders

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
  UserRegistered data -> Integration.Outbound.batch do
    Sendgrid.email { ... }
      (\response -> CreateWelcomeNotification { ... })  -- -> Notification service

    Slack.message { ... }
      (\_ -> LogAuditEvent { ... })  -- -> Audit service
```

The command dispatcher routes based on command type name, not entity association.

## Consequences

### Positive

1. **Single source of truth**: All integrations for an entity are in one file. Easy to discover.

2. **Natural Haskell pattern**: Pattern matching on event ADT is idiomatic.

3. **Compiler-checked exhaustiveness**: With `-Wincomplete-patterns`, compiler warns about unhandled events.

4. **Explicit "nothing happens"**: `_ -> Integration.Outbound.none` makes absence of integrations visible.

5. **Cross-service flexibility**: Commands can target any service via type-based routing.

6. **Two-persona design intact**: Nick builds action builders; Jess composes them.

7. **Testable**: Entity integration function is pure - test with different event inputs.

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
