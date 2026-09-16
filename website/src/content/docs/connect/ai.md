---
title: Add AI assistance to the application
description: Make generated suggestions useful, bounded, and reviewable.
sidebar:
  order: 7
---

Using an agent to write code and adding an AI feature to that code are different relationships. An application feature might summarise notes, draft text, or help interpret a document. It needs its own inputs, permissions, spending limits, failure states, and acceptance rules.

A useful first feature proposes text for review. In the ecommerce practice project, Jess will generate a product-description draft from supplied facts. She can inspect and approve the draft while keeping the rest of the application usable if the provider is unavailable.

## Model a draft before making a request

A draft workflow records a request, calls the provider, and records either generated text or a failure. Acceptance is a separate command. In the example, keep the product identifier and request identity so that a delayed reply does not overwrite a newer draft.

The provider callback tells you that a response decoded successfully. It does not establish factual accuracy, suitability for publication, or compliance with the application's rules.

## Build an OpenRouter request

This **partial builder** uses the real API. `modelName` comes from your chosen provider configuration; `productFacts` contains only approved input. `recordDraftResponse` must inspect the response and produce the same command type as `recordDraftFailure`.

```haskell
import Integration qualified
import Integration.OpenRouter qualified as OpenRouter

Integration.outbound OpenRouter.Request
  { messages =
      [ OpenRouter.system
          "Draft a short product description using only the supplied facts."
      , OpenRouter.user productFacts
      ]
  , model = modelName
  , config = OpenRouter.defaultConfig
      { OpenRouter.maxTokens = Just 300
      , OpenRouter.timeoutSeconds = 30
      }
  , onSuccess = recordDraftResponse
  , onError = recordDraftFailure
  }
```

The adapter obtains its bearer token from `OPENROUTER_API_KEY`. Choose and verify a currently available model independently; model identifiers in older examples are not a promise of current availability.

The response contains `choices` and optional `usage`. Handle an empty choices array. A choice includes its message and finish reason: truncation or filtering can mean the text is unsuitable even when the HTTP request succeeded. Message content can be plain text or multiple content parts.

This integration makes a non-streaming request. It is appropriate for a background draft workflow; it does not by itself implement a streaming chat interface, conversation storage, or a retrieval system.

## Use Azure AI where appropriate

Azure requests have an explicitly validated endpoint and a redacted API key. First call:

```haskell
AzureAI.azureEndpoint endpointText
```

This returns `Result Text AzureEndpoint`; handle an invalid endpoint as a configuration problem. `azureEndpointAllowing` permits additional trusted host suffixes for your deployment. Keep those suffixes under operator control.

An **expression fragment**, after validation and with an actual implicit configuration binding, is:

```haskell
AzureAI.chatCompletion
  validatedEndpoint
  [AzureAI.system "Use only supplied product facts.", AzureAI.user productFacts]
  deploymentName
  recordDraftResponse
  recordDraftFailure
```

The helper reads `?config.azureAiApiKey :: Redacted Text`. For explicit credential plumbing, build `AzureAI.Request` with `apiKey` and a configuration whose `endpoint` is the validated value. Do not use the bare default configuration as a complete endpoint setup. The source pins an API-version default; verify compatibility with your deployment.

## Give Jess evidence beyond a nice paragraph

> **Agent:** “The model returned a description, so I publish it.”
>
> **Jess:** “Show me the command that approves publication. Generated text should remain a draft until I accept it.”

Test response handling with fixed fixtures before testing a live model. Check missing choices, unwanted claims, truncation, provider refusal, and a response arriving after the product facts changed. Build a small evaluation set of product facts and unacceptable outputs. Live provider calls verify connectivity and suitability for your workload, while unit tests verify your deterministic rules.

The shared [HTTP retry caveat](/connect/http-and-payments/#understand-the-current-retry-boundary) also applies to provider calls. A timeout is not proof that no billable work happened. Set an application spending policy and align provider request timeouts with the [dispatcher budget](/connect/documents/#budget-the-entire-operation).

**Exercise:** add a regenerate action to the practice project. Decide whether an older in-flight response may replace it, then test replies arriving in reverse order.

Continue to [AI tools](/connect/ai-tools/) only when you are ready for the model to propose structured actions.

## Implementation and examples

- [integrations/Integration/OpenRouter/Request.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/OpenRouter/Request.hs)
- [integrations/Integration/OpenRouter/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/OpenRouter/Internal.hs)
- [integrations/Integration/OpenRouter/Response.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/OpenRouter/Response.hs)
- [integrations/Integration/AzureAI/Request.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/AzureAI/Request.hs)
- [integrations/test/Integration/AzureAI/RequestSpec.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/test/Integration/AzureAI/RequestSpec.hs)
