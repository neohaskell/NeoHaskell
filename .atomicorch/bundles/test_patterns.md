---
id: test_patterns
target_max_tokens: 4000
---

# NeoHaskell Test Conventions

All tests use Hspec with NeoHaskell conventions. Tests are co-located in `tests/` directories within each package and registered via hspec-discover or explicit `other-modules` entries.

## Spec Signature

Every test module must export a `spec` function with this exact signature:

```haskell
spec :: Spec Unit
```

Note: `Unit` not `()`. This is the NeoHaskell alias.

## It Callback Style

All `it` callbacks use a lambda with `\_ ->` (discarding the unit argument):

```haskell
it "parses a valid email" \_ -> do
  let result = Email.parse "user@example.com"
  result |> shouldSatisfy Result.isOk
```

Never use `\() ->` or just a `do` block directly.

## Pipe-First Assertions

Assertions are written pipeline-style, subject first:

```haskell
-- CORRECT
result |> shouldBe (Result.ok expectedUser)
value |> shouldSatisfy (\x -> x > 0)
list |> shouldSatisfy Array.isEmpty

-- WRONG
shouldBe result (Result.ok expectedUser)
result `shouldBe` Result.ok expectedUser
```

## Result Assertions

Use `Result.isOk` and `Result.isErr` with `shouldSatisfy` for Result-typed values:

```haskell
-- For success cases
result |> shouldSatisfy Result.isOk

-- For error cases
result |> shouldSatisfy Result.isErr

-- For specific success values
result |> shouldBe (Result.ok expectedValue)

-- For specific error values
result |> shouldBe (Result.err expectedError)
```

## Custom Failure Messages

Use `Test.fail` with `[fmt|...|]` for descriptive failures:

```haskell
case result of
  Result.Err err ->
    Test.fail [fmt|Expected success but got error: {err}|]
  Result.Ok value ->
    value |> shouldBe expected
```

Never use `error` or `undefined` in tests.

## Test Suite Registration

### hspec-discover (preferred)

Add `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` to the `Spec.hs` entrypoint. All modules ending in `Spec.hs` are automatically discovered.

### Manual registration

When hspec-discover is not used, register each spec module in `cabal.other-modules`:

```cabal
test-suite nhcore-test-core
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  other-modules:
    Core.ArraySpec
    Core.TextSpec
    Core.ResultSpec
    Core.TaskSpec
  build-depends:
    nhcore,
    hspec,
    ...
```

## Test Suites Table

| Suite Name | Package | Covers |
|------------|---------|--------|
| `nhcore-test-core` | core/core | Array, Text, Result, Task, Option |
| `nhcore-test-auth` | core/auth | Authentication, JWT, session |
| `nhcore-test-service` | core/service | Service, Command, Event dispatch |
| `nhcore-test-http` | core/http | Request parsing, response building |
| `nhcore-test-crypto` | core/crypto | Hashing, encryption, signing |
| `nhcore-test-eventstore` | core/eventstore | Event persistence, replay |
| `nhcore-test-integration` | testbed | Cross-package integration |

## Spec Structure

Organize specs hierarchically matching module structure:

```haskell
module Core.UserSpec (spec) where

import Core.User qualified as User
import Test.Hspec

spec :: Spec Unit
spec = do
  describe "User.create" do
    context "with valid input" do
      it "returns Ok with created user" \_ -> do
        let result = User.create validInput
        result |> shouldSatisfy Result.isOk

    context "with empty name" do
      it "returns Err with validation message" \_ -> do
        let result = User.create (validInput { name = "" })
        result |> shouldSatisfy Result.isErr

    context "with name exceeding 255 chars" do
      it "returns Err" \_ -> do
        let longName = Text.replicate 256 "a"
        let result = User.create (validInput { name = longName })
        result |> shouldSatisfy Result.isErr
```

## Cabal Configuration Rules

1. Test suites must be declared in the package's `.cabal` file under `test-suite` stanzas.
2. All test modules must be listed in `other-modules` or discoverable via hspec-discover.
3. Test suites depend on `nhcore` or their target package — never import internal modules directly.
4. Use `build-tool-depends: hspec-discover:hspec-discover` when using auto-discovery.

```cabal
test-suite nhcore-test-auth
  type:             exitcode-stdio-1.0
  hs-source-dirs:   tests
  main-is:          Spec.hs
  other-modules:    {}
  ghc-options:      -F -pgmF hspec-discover
  build-tool-depends: hspec-discover:hspec-discover
  build-depends:
    base,
    nhcore,
    hspec >= 2.9,
  default-language: GHC2021
  default-extensions:
    NoImplicitPrelude
    OverloadedStrings
```

## Property-Based Tests

Use `QuickCheck` via hspec integration for property tests:

```haskell
import Test.Hspec.QuickCheck (prop)

spec :: Spec Unit
spec = do
  describe "Array.reverse" do
    prop "reversing twice is identity" \_ xs ->
      (xs |> Array.reverse |> Array.reverse) `shouldBe` xs
```

## Anti-Patterns

```haskell
-- ANTI: Wrong spec signature
spec :: Spec ()

-- ANTI: Non-pipe assertion
shouldBe result expected

-- ANTI: error in tests
case result of
  Left _ -> error "unexpected"

-- ANTI: IO in test body (use Task.runSync or similar)
it "does something" $ do
  x <- someIO
  x `shouldBe` expected
```
