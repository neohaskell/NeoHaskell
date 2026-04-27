---
id: devex_jess
target_max_tokens: 2000
---

# DevEx Review: Jess Persona

Every API, module, and CLI command in NeoHaskell is evaluated from the perspective of **Jess** before release.

---

## Who Is Jess

**Jess** is a junior developer who:

- Has 1–2 years of TypeScript or Java experience
- Is excited about NeoHaskell but not yet fluent in Haskell
- Contributes in **15–30 minute sessions** — between meetings, during lunch
- Does not have time to read a chapter of documentation to understand one function
- Will give up and write a workaround if something is too confusing
- Thrives with good error messages and predictable naming

Jess represents the majority of future NeoHaskell contributors. If Jess can't use it in 15 minutes, it's too hard.

---

## The 15-Minute Rule

Any feature Jess needs to use must be:

1. **Discoverable** in 5 minutes — Jess finds the function without reading the whole module
2. **Understandable** in 5 minutes — Jess reads the signature and one example and knows what to do
3. **Working** in 5 minutes — Jess writes the code, it compiles, it runs

If any step takes longer, the API has failed the 15-minute rule.

---

## 3 Core Principles

### 1. Principle of Least Astonishment

The API should do exactly what its name suggests. Surprises are bugs.

```haskell
-- ASTONISHING (bad): Array.drop drops from the END
-- EXPECTED: Array.drop n drops the first n elements (matches every other language)

-- ASTONISHING (bad): Text.split includes the delimiter in results
-- EXPECTED: Text.split discards the delimiter
```

### 2. Developer Happiness

Functions should feel good to use. Pipeline-friendly signatures put the subject last. Error messages tell you what to do next.

```haskell
-- HAPPY: subject last, pipeable
Array.map :: (element -> result) -> Array element -> Array result
-- Usage: users |> Array.map toDto

-- UNHAPPY: subject first, can't pipe without flip
Array.map :: Array element -> (element -> result) -> Array result
-- Usage: Array.map users toDto  -- breaks pipe pattern
```

### 3. Principle of Least Effort

The most common use case should be the easiest. Defaults should be correct for 80% of use cases. Advanced options should not pollute the simple interface.

```haskell
-- LEAST EFFORT: simple case is simple
Http.get :: Url -> Task HttpError Json.Value

-- ADVANCED: options available but not required
Http.getWith :: HttpOptions -> Url -> Task HttpError Json.Value
```

---

## 6 Criteria Checklist

For every new API surface, evaluate all 6:

### 1. Naming

- [ ] Function name is a verb or verb phrase that matches what it does
- [ ] No Haskell jargon in public names (no `fmap`, `liftA2`, `mconcat`)
- [ ] Names match TypeScript/Java equivalents where possible (`map`, `filter`, `find`, `reduce`)
- [ ] Type names are nouns (`User`, `Config`, `Result`)
- [ ] Error types are descriptive (`UserNotFound`, not `Error1`)

### 2. Pipe-friendliness

- [ ] The "subject" (thing being operated on) is the last argument
- [ ] Partially applying all arguments except the last produces a useful function
- [ ] Works in a `|>` chain without `flip` or lambda wrapping

### 3. Discoverability

- [ ] Grouped under a sensible qualified module (`Array.`, `Text.`, `User.`)
- [ ] Haddock documentation present on all exported symbols
- [ ] At least one usage example in the docs
- [ ] Related functions linked (`See also: Array.filterMap`)

### 4. Error Messages

- [ ] Compile errors name the specific constraint that failed
- [ ] Runtime errors (Result.Err values) say what was wrong AND what to do
- [ ] No "impossible" errors exposed to the user (handle internally)

Example of a good error value:
```
UserNotFound { searched_by = Email "jess@example.com" }
-- Message: "No user found with email jess@example.com. 
--           Check that the email is correct or create the user first."
```

### 5. Defaults

- [ ] Functions work without options for the 80% case
- [ ] Optional configuration uses a record with meaningful defaults
- [ ] No required arguments that most callers would pass the same value to

### 6. Consistency

- [ ] Follows the same pattern as sibling functions in the same module
- [ ] Uses the same error type as similar operations
- [ ] Naming convention matches the rest of the module

---

## Anti-Patterns

```haskell
-- ANTI: Tuple arguments (use a record)
createUser :: (Text, Email, UserId) -> Result Error User

-- ANTI: Bool arguments (use a dedicated type)
fetchUser :: Bool -> UserId -> Task Error User
-- (what does Bool mean? isAdmin? includeDeleted?)

-- ANTI: Magic numbers
Array.chunk 256 largeArray
-- (where does 256 come from? use a named constant)

-- ANTI: Jargon in error messages
Result.err "aeson: expected Object, got Array at path $.users[0]"

-- ANTI: Functions that do two things
parseAndValidateUser :: Text -> Result Error User
-- (split into parse and validate for composability)
```

---

## Red Lines

These patterns are automatic DevEx failures:

| Pattern | Reason |
|---------|--------|
| Subject-first arguments | Breaks pipe ergonomics |
| `String` in public API | Forces conversion, surprises JS/TS devs |
| Unexplained `forall` in public types | Confusing to non-Haskell devs |
| `IO` in public API | Contradicts NeoHaskell model |
| Unnamed tuple returns | Jess can't remember which element is which |
| Required `Options` record for simple case | Violates Least Effort |
