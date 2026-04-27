---
id: qa_rubric
target_max_tokens: 2000
---

# NeoHaskell QA Rubric

## Methodology: Outside-In TDD

Write tests before implementation. Tests describe the public API contract, not internal mechanics. Never test private functions directly — test through the public interface.

Order of work:
1. Write the spec module with all test cases (red)
2. Implement just enough to make tests pass (green)
3. Refactor without breaking tests (refactor)

---

## Coverage Requirements

Every public function requires tests across these coverage axes:

| Axis | Description | Min Cases |
|------|-------------|-----------|
| Happy path | Normal, expected inputs | 1 |
| Edge cases | Boundaries, empty, maximal | ≥ 3 |
| Error cases | Invalid input, failure modes | ≥ 2 |
| Serialization | JSON round-trip if applicable | 1 |
| Property-based | Invariant holds for random inputs | 1 |
| Concurrency | Race conditions if stateful | 1 (if applicable) |

**Quality gate:** ≥ 3 total test cases per public function, with at least 3:1 edge-to-happy ratio.

---

## Input Analysis Table

For each parameter of a function under test:

| Parameter Type | Cases to Cover |
|----------------|----------------|
| `Text` | empty string, single char, max length, unicode, whitespace-only |
| `Int` / `Word` | 0, 1, -1, minBound, maxBound |
| `Array a` | empty, singleton, large (>1000 elements) |
| `Option a` | `Option.none`, `Option.some value` |
| `Result e a` | `Result.ok value`, `Result.err error` |
| Custom data type | each constructor, each field at boundary |
| `EntityId` | valid format, invalid format, empty |
| `Json.Value` | missing fields, extra fields, wrong types, null |

---

## Output Analysis Table

For each possible output of a function under test:

| Output Type | Cases to Verify |
|-------------|-----------------|
| `Result.Ok value` | Verify the value is correct |
| `Result.Err error` | Verify the error type and message |
| `Option.some value` | Verify the wrapped value |
| `Option.none` | Assert none was returned |
| `Array element` | Length, order, contents |
| `Text` | Exact value or pattern |
| Side effects | Events emitted, state transitions |

---

## Quality Gates

A test suite passes the QA rubric if and only if:

1. **≥ 3 test cases** for every exported function
2. **≥ 3:1 edge-to-happy ratio** (edge + error cases ≥ 3× happy cases)
3. **All serialization round-trips** pass for any type with `Json.FromJson`/`Json.ToJson`
4. **At least one property-based test** per module with non-trivial logic
5. **No `error` or `undefined`** in test code
6. **All tests are deterministic** (no `getCurrentTime` without mocking, no random without fixed seed)

---

## Anti-Patterns

```haskell
-- ANTI: Testing implementation details
it "calls the internal helper" \_ -> do
  -- Never test private functions

-- ANTI: Only happy path
describe "User.create" do
  it "creates a user" \_ -> do  -- Only 1 test, no edges

-- ANTI: Vague test names
it "works correctly" \_ -> do
it "handles the case" \_ -> do

-- ANTI: Shared mutable state between tests
-- Use fresh fixtures in every `it` block

-- ANTI: Ignoring the error case
case result of
  Result.Ok v -> v |> shouldBe expected
  Result.Err _ -> pure unit  -- Must fail, not ignore

-- ANTI: Hardcoded timing
-- Never use threadDelay in tests; use deterministic event simulation
```

---

## Test Naming Convention

```
describe "<Module.function>" do
  context "with <condition>" do
    it "<expected behavior>" \_ -> do
```

Examples:
- `describe "User.create"` → `context "with empty email"` → `it "returns Err EmailRequired"`
- `describe "Array.chunk"` → `context "with chunk size larger than array"` → `it "returns singleton array"`
- `describe "EventStore.append"` → `context "with wrong expected version"` → `it "returns Err WrongVersion"`
