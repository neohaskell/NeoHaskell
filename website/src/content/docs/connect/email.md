---
title: Send email
description: Record provider acceptance and handle notification failure visibly.
sidebar:
  order: 4
---

Email gives people a result they can inspect outside your application: a notification, an invitation, or a confirmation. It also introduces an important distinction: a provider accepting a message does not prove the recipient received or read it.

NeoHaskell includes Brevo and Azure Communication Services (ACS) request types. We will use an order confirmation from the practice project to learn the request and callback pattern. Provider credentials, verified sender setup, and live delivery are separate setup work; start with a test recipient you control.

## Define the outcome first

Use an application command capable of representing provider acceptance and failure. It should carry the notification identifier and any identifier needed to connect it to the originating action. Its successful branch records the provider's message/operation ID; its failure branch records a safe explanation that the application can display.

Trigger email from a committed event. In the example, a notification failure should not make the accepted order disappear. To resend safely, model the notification attempt and decide how duplicate sends are handled.

## Configure a Brevo request

This **partial builder** uses actual fields. `emailKey` is a `Redacted Text` credential value; `recordAccepted` and `recordFailed` return the same command type. The addresses are fictional examples.

```haskell
import Integration qualified
import Integration.Brevo qualified as Brevo
import Integration.Brevo.Internal qualified as BrevoInternal
import Integration.Http ()

Brevo.Request
  { sender = Brevo.sender "orders@example.com"
  , to = [Brevo.recipient customerEmail]
  , subject = "Your mug order"
  , body = Brevo.TextBody "We have received your order."
  , cc = []
  , bcc = []
  , replyTo = Nothing
  , tags = []
  , apiKey = emailKey
  , onSuccess = recordAccepted
  , onError = recordFailed
  }
  |> BrevoInternal.toHttpRequest
  |> Integration.outbound
```

For an event handler with no configuration parameter, one supported runtime pattern is to set `emailKey` to `Redacted.wrap "${SHOP_BREVO_API_KEY}"` (with `import Redacted qualified`). This stores a placeholder in the request; the shared HTTP authentication layer expands it from the server environment when executing. Set that environment variable through your deployment's secret configuration. Do not put the actual key in the event or the source file.

The explicit conversion is needed for the current source: the Brevo facade exposes its request builder but does not supply a direct `ToAction (Brevo.Request command)` instance. `Integration.Brevo.Internal` is exposed by the package; keeping this conversion in one application helper makes that implementation detail easy to replace later.

Use `HtmlBody`, `TextBody`, or `Template`; the body type permits one alternative at a time. A template carries `templateId` and a `Map Text Text` of parameters. `Sender` and `Recipient` are distinct types, which helps prevent accidentally reversing them.

The shorter `Brevo.send` constructor reads `?config.brevoApiKey`. Use it only where that implicit configuration value is actually bound. Merely registering application configuration does not add an implicit parameter to the pure typed handler signature. The explicit request above makes credential plumbing visible.

## Read the response precisely

Brevo's adapter recognises HTTP 201 and decodes `messageId`. Invalid response data follows the error callback. It maps authentication, account-credit, rate-limit, client, and server statuses to error text.

ACS uses `Acs.Request`, with `endpoint`, `sender`, `to`, `subject`, `body`, `accessToken`, and the two callbacks. Its public facade includes the execution instance, so it can be passed to `Integration.outbound` directly. ACS's accepted response exposes `operationId`; it is an asynchronous send operation, not delivery confirmation. The token is `Redacted Text`. Supply its acquisition and renewal strategy separately.

Keep ACS endpoints in trusted configuration. Its adapter enforces HTTPS; that check alone is not a business-specific host allowlist.

Both adapters use the shared HTTP machinery. Read the [current retry limitation](/connect/http-and-payments/#understand-the-current-retry-boundary) before assuming one send attempt.

## Check what Jess can trust

First test the request and response mapping without sending mail. The public Brevo tests exercise JSON body alternatives and malformed accepted responses. Then send one message in a controlled provider environment and inspect both the application outcome and the recipient mailbox.

**Exercise:** the provider accepts the email, but recording acceptance in your application fails. Explain what a “resend” button should do.

<details>
<summary>Suggested reasoning</summary>

Treat the local outcome as unresolved. Keep the notification's stable identity and provider evidence where available, define how you would investigate it, and decide whether the risk of duplicate email is acceptable. Test duplicate triggering events and delayed results as well as the ordinary accepted/failed paths.

</details>

Next, learn how [file attachments](/connect/files/) connect stored bytes to application actions.

## Implementation and examples

- [integrations/Integration/Brevo.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Brevo.hs)
- [integrations/Integration/Brevo/Request.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Brevo/Request.hs)
- [integrations/Integration/Brevo/Response.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Brevo/Response.hs)
- [integrations/Integration/Brevo/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Brevo/Internal.hs)
- [integrations/test/Integration/Brevo/InternalSpec.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/test/Integration/Brevo/InternalSpec.hs)
- [integrations/Integration/Acs/Request.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Acs/Request.hs)
- [integrations/Integration/Acs/Response.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Acs/Response.hs)
- [integrations/Integration/Acs/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Acs/Internal.hs)
- [core/core/Redacted.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/core/Redacted.hs)
- [integrations/Integration/Http/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Internal.hs)
- [integrations/nhintegrations.cabal](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/nhintegrations.cabal)
