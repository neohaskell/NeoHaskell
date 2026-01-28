# Feature Progress: OpenRouter Integration

## Current Phase: 5-8 (Implementation Loop - Tests)

## Phase Checklist
- [x] Phase 1: API Design
- [x] Phase 2: Testbed Implementation
- [x] Phase 3: Integration Test Spec
- [x] Phase 4: Architecture Plan
- [ ] Phase 5-8: Implementation Loop
  - [ ] Layer 1: Public API (Message, Request, Response types)
  - [ ] Layer 2: Core Logic (toHttpRequest transformation)
  - [ ] Layer 3: nhcore extensions (Json module)
- [ ] Phase 9: Validation
- [ ] Phase 10: PR Review

## Loop History
- Loop 1: Initial implementation (in progress)

## Blockers
(none)

---

## Phase 1: API Design (COMPLETE)

### Jess's API

```haskell
-- What Jess imports
import Integration qualified
import Integration.OpenRouter qualified as OpenRouter
import Integration.OpenRouter.Message qualified as Message

-- What Jess writes (simple case)
askAi :: Text -> Integration.Action
askAi question = do
  let request = OpenRouter.chatCompletion
        [Message.user question]
        "anthropic/claude-3.5-sonnet"
        (\response -> GotAiResponse {response})
        (\err -> AiError {err})
  Integration.outbound (OpenRouter.toHttpRequest request)

-- What Jess writes (advanced config)
OpenRouter.Request
  { messages = [Message.system "Be concise.", Message.user question]
  , model = "openai/gpt-4o"
  , config = OpenRouter.defaultConfig
      { temperature = Just 0.9
      , maxTokens = Just 2000
      }
  , onSuccess = \response -> GotAnswer {response}
  , onError = \err -> AiError {err}
  }
```

### Message Building

```haskell
-- Smart constructors hide Role enum
Message.user "Hello"        -- User message
Message.system "Be brief."  -- System prompt
Message.assistant "Hi!"     -- For conversation history
```

### Response Handling

```haskell
onSuccess = \response ->
  case response.choices |> Array.first of
    Just choice -> GotAnswer { content = choice.message.content }
    Nothing -> NoAnswer
```

### Nick's Implementation (Hidden from Jess)

- `Integration.OpenRouter.Internal.toHttpRequest` - Transforms Request to Http.Request
- `Integration.OpenRouter.Message` - JSON encoding/decoding for messages
- `Integration.OpenRouter.Response` - JSON decoding for API responses
- Uses `${OPENROUTER_API_KEY}` environment variable expansion

### Design Decisions

1. **Piggyback pattern**: OpenRouter.Request → Http.Request via toHttpRequest
2. **No streaming v1**: Simpler implementation, streaming in v2
3. **Environment-based auth**: `${OPENROUTER_API_KEY}` follows Http pattern
4. **Callbacks over Task**: Matches existing Http integration pattern

---

## Next Action
Phase 2: Create testbed example in `testbed/src/Testbed/Examples/OpenRouterIntegration.hs`
