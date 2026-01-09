---
name: Fix flaky subscriber tests
about: Replace fixed sleep calls with bounded polling
title: 'fix(test): Replace fixed sleeps with polling in subscriber tests'
labels: 'type: fix, type: testing, package: core, priority: important, effort: 3, work: obvious, good first issue'
assignees: ''
---

## Summary

The subscriber tests in `core/test/Service/Query/SubscriberSpec.hs` use fixed `AsyncTask.sleep` calls to wait for asynchronous operations. This causes test flakiness because timing assumptions may not hold on different machines or under load.

## User Story

As a developer running tests, I want tests to be deterministic and fast, so that I can trust test results and have a quick feedback loop during development.

## Context

Lines 201-234 and 236-281 contain tests that:
1. Start a subscription with `Subscriber.start`
2. Sleep for 50ms hoping the subscription is active
3. Insert an event with `insertTestEvent`
4. Sleep for 100ms hoping the event is processed
5. Assert the expected count

This approach is problematic because:
- CI/CD environments may be slower than development machines
- Under load, 50ms or 100ms may not be enough
- Unnecessarily long waits slow down test suites
- Tests fail intermittently when timing assumptions break

## Current Flaky Code

```haskell
-- Give subscription time to activate
AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

-- Insert event after subscription started
insertTestEvent eventStore entityName

-- Give time for event to be processed
AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

count <- ConcurrentVar.peek processedCount
count |> shouldBe 1
```

## Acceptance Criteria

- [ ] Create a bounded polling helper function that:
  - Repeatedly reads from a `ConcurrentVar` using `ConcurrentVar.peek` or `ConcurrentVar.tryRead`
  - Checks if an expected value/condition is met
  - Has a configurable timeout (e.g., 5000ms)
  - Returns success immediately when condition is met
  - Fails with a clear error message if timeout is reached
  - Uses small sleep intervals between polls (e.g., 10ms)

- [ ] Replace fixed sleeps in test "receives events inserted after subscription starts" (lines 201-234)

- [ ] Replace fixed sleeps in test "starts from correct position after rebuild" (lines 236-281)

- [ ] Both tests should:
  - Use the polling helper after `Subscriber.start` to wait for subscription activation
  - Use the polling helper after `insertTestEvent` to wait for event processing
  - Keep existing assertion patterns with `shouldBe`
  - Complete faster when conditions are met early
  - Still fail clearly if expected behavior doesn't occur within timeout

- [ ] All tests pass consistently: `cabal test nhcore-test`

## Implementation Hints

### Location
Add the polling helper to the test file `core/test/Service/Query/SubscriberSpec.hs`

### Example Implementation Pattern

```haskell
-- | Wait until a ConcurrentVar contains the expected value, or timeout.
-- Polls every 10ms up to the specified timeout.
waitUntilCount ::
  forall error.
  ConcurrentVar Int ->
  Int ->
  Int ->
  Task error Unit
waitUntilCount var expectedValue timeoutMs = do
  let pollIntervalMs = 10
  let maxAttempts = timeoutMs / pollIntervalMs
  waitLoop 0 maxAttempts
  where
    waitLoop currentAttempt maxAttempts = do
      currentValue <- ConcurrentVar.peek var
      case currentValue == expectedValue of
        True -> Task.yield unit
        False -> do
          case currentAttempt >= maxAttempts of
            True -> Task.throw "Timeout waiting for expected count"
            False -> do
              AsyncTask.sleep pollIntervalMs
                |> Task.mapError (\_ -> "sleep error" :: Text)
              waitLoop (currentAttempt + 1) maxAttempts
```

### Files to Modify
- `core/test/Service/Query/SubscriberSpec.hs` - Add helper and update tests

### Related Types/Modules
- `ConcurrentVar.peek` or `ConcurrentVar.tryRead` - for polling the counter
- `AsyncTask.sleep` - for small intervals between polls (not fixed waits)
- `Subscriber.start` - subscription activation point
- `insertTestEvent` - event insertion helper

### Testing
After implementation:
```bash
# Run tests multiple times to verify no flakiness
for i in {1..10}; do
  cabal test nhcore-test --test-option='--match="/receives events inserted after subscription starts/"'
done

for i in {1..10}; do
  cabal test nhcore-test --test-option='--match="/starts from correct position after rebuild/"'
done
```

## Effort Estimate

**3 points** - Straightforward implementation with clear requirements. Involves:
- Writing a polling helper function (~20 lines)
- Replacing sleep calls in 2 test cases
- Running tests to verify determinism

Most of the work is understanding the pattern and applying it consistently. A developer familiar with the codebase should complete this in 2-3 hours.

## What Success Looks Like

After this fix:
- Tests complete in ~50-200ms instead of 150ms+ fixed waits
- Tests pass consistently across different machines and load conditions
- Clear timeout errors if subscription/processing actually fails
- Pattern can be reused for other async tests in the future

---

Got stuck for more than 15 minutes? [Join our Discord](https://discord.gg/neohaskell) - we'd love to help!
