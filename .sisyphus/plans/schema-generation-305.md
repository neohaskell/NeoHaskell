# Automatic Schema Generation (Issue #305)

## Context

**Issue**: https://github.com/neohaskell/NeoHaskell/issues/305
**ADR**: ADR-0013: Automatic Schema Generation
**Workflow Path**: Standard (no security surface)

### Summary
Enable automatic extraction of schema information from Haskell types so NeoHaskell can generate OpenAPI specs, JSON Schema, and interactive documentation without manual schema writing.

### Justification for Standard Workflow
Security review skipped: internal infrastructure feature with no user input, no auth, no external data processing. Schema generation is pure type-level computation.

---

## Work Objectives

### Deliverables
- `core/schema/Schema.hs` - Schema ADT and FieldSchema
- `core/schema/ToSchema.hs` - ToSchema typeclass with Generic derivation
- `core/traits/Documented.hs` - Documented typeclass
- Re-exports in `Core.hs`
- Unit tests for all schema types

### Definition of Done
- [ ] All acceptance criteria from issue #305 met
- [ ] Build passes (`cabal build all`)
- [ ] Tests pass (`cabal test`)
- [ ] Lint clean (`hlint .`)
- [ ] Doctests pass (`./scripts/run-doctest`)

---

## TODOs

### Step 1: Specification (Already Done)
- [x] Issue #305 provides complete specification
- [x] ADR-0013 accepted in #304
- [x] Acceptance criteria defined

### Step 2: SKIPPED (Security Pre-Review)
Justification: Internal infrastructure feature, pure type-level computation, no security surface.

---

### Step 3: Implement Testbed Usage (As-If Existing)

- [ ] 3.1 Create example usage in testbed showing schema derivation
  - File: `testbed/src/Testbed/Schema/Example.hs`
  - Show: `deriving (Generic)` + `instance ToSchema` pattern
  - Example types: simple record, record with Maybe, sum type

- [ ] 3.2 Verify testbed compiles with stubs
  - Run: `cabal build nhtestbed`
  - Expected: Compiles (stubs can use `error "not implemented"`)

---

### Step 4: Implement Unit Tests

**Note**: This is a core library feature, not an HTTP endpoint, so Hurl tests are not applicable. Unit tests serve as the executable specification.

- [ ] 4.1 Create test file `core/test/Test/Schema/ToSchemaSpec.hs`

- [ ] 4.2 Test primitive type schemas
  - `toSchema @Bool` → `SBool`
  - `toSchema @Int` → `SInt`
  - `toSchema @Text` → `SText`
  - `toSchema @Double` → `SNumber`

- [ ] 4.3 Test container type schemas
  - `toSchema @(Array Int)` → `SArray SInt`
  - `toSchema @(Maybe Text)` → `SOptional SText`

- [ ] 4.4 Test record type schemas
  - Define: `data Person = Person { name :: Text, age :: Int }`
  - Verify: `SObject` with correct field names
  - Verify: Field order preserved

- [ ] 4.5 Test Maybe fields in records
  - Define: `data User = User { email :: Text, nickname :: Maybe Text }`
  - Verify: `nickname` has `fieldRequired = False`
  - Verify: `email` has `fieldRequired = True`

- [ ] 4.6 Test sum types (enum)
  - Define: `data Status = Active | Inactive | Pending`
  - Verify: `SEnum ["Active", "Inactive", "Pending"]`

- [ ] 4.7 Test sum types (union)
  - Define: `data Shape = Circle Double | Rectangle Double Double`
  - Verify: `SUnion [("Circle", ...), ("Rectangle", ...)]`

- [ ] 4.8 Register tests in cabal file
  - Add test module to `nhcore.cabal`

---

### Step 5: SKIPPED (Unit Test Specs)
Combined with Step 4 - unit tests serve as both specification and verification for this library feature.

---

### Step 6: Outside-In TDD Implementation

#### 6.0 Create Module Structure
- [ ] Create `core/schema/` directory
- [ ] Add `Schema` to `hs-source-dirs` in `nhcore.cabal`

#### 6.1 Core Schema Types
- [ ] Create `core/schema/Schema.hs`
- [ ] Define `Schema` ADT:
  ```haskell
  data Schema
    = SNull
    | SBool
    | SInt
    | SNumber
    | SText
    | SArray Schema
    | SOptional Schema
    | SObject (Array FieldSchema)
    | SEnum (Array Text)
    | SUnion (Array (Text, Schema))
    | SRef Text
  ```
- [ ] Define `FieldSchema`:
  ```haskell
  data FieldSchema = FieldSchema
    { fieldName :: Text
    , fieldSchema :: Schema
    , fieldRequired :: Bool
    , fieldDescription :: Text
    }
  ```

#### 6.2 ToSchema Typeclass
- [ ] Create `core/schema/ToSchema.hs`
- [ ] Define `ToSchema` class:
  ```haskell
  class ToSchema value where
    toSchema :: Schema
    default toSchema :: (Generic value, GToSchema (Rep value)) => Schema
    toSchema = gToSchema @(Rep value)
  ```

#### 6.3 Primitive Instances
- [ ] Instance for `Bool` → `SBool`
- [ ] Instance for `Int` → `SInt`
- [ ] Instance for `Integer` → `SInt`
- [ ] Instance for `Double` → `SNumber`
- [ ] Instance for `Float` → `SNumber`
- [ ] Instance for `Text` → `SText`
- [ ] Instance for `String` → `SText`

#### 6.4 Container Instances
- [ ] Instance for `Array a` → `SArray (toSchema @a)`
- [ ] Instance for `[a]` → `SArray (toSchema @a)`
- [ ] Instance for `Maybe a` → `SOptional (toSchema @a)`

#### 6.5 GToSchema (Generic Derivation)
- [ ] Create `GToSchema` class for Generic reps
- [ ] Handle `M1` (metadata wrappers)
- [ ] Handle `K1` (field values)
- [ ] Handle `:*:` (products/records)
- [ ] Handle `:+:` (sums)
- [ ] Handle `U1` (unit/nullary constructors)
- [ ] Extract field names via `selName`
- [ ] Detect `Maybe` fields for `fieldRequired = False`

#### 6.6 Documented Typeclass
- [ ] Create `core/traits/Documented.hs`
- [ ] Define `Documented` class:
  ```haskell
  class (TypeName.Inspectable value) => Documented value where
    name :: Text
    name = TypeName.reflect @value
    
    description :: Text
    description = ""
    
    examples :: Array value
    examples = []
    
    deprecated :: Bool
    deprecated = False
  ```

#### 6.7 Core.hs Re-exports
- [ ] Add `Schema` module to Core.hs exports
- [ ] Add `ToSchema`, `toSchema` to Core.hs exports
- [ ] Add `Documented` to Core.hs exports

#### 6.8 Build Verification (ALL must pass)
- [ ] 6a. Unit tests pass: `cabal test nhcore-test`
- [ ] 6b. Integration tests pass: `./testbed/scripts/run-tests.sh`
- [ ] 6c. Build succeeds: `cabal build all`
- [ ] 6d. Lint clean: `hlint .`
- [ ] 6e. Doctests pass: `./scripts/run-doctest`

---

### Step 7: SKIPPED (Security Post-Review)
Justification: No security surface in schema generation.

### Step 8: SKIPPED (Fix Security Issues)
Justification: Step 7 skipped.

---

### Step 9: Generate PR

- [ ] 9.1 Commit with descriptive message
- [ ] 9.2 Push to remote
- [ ] 9.3 Create PR referencing issue #305
- [ ] 9.4 Verify CI passes

---

## References

- Issue: https://github.com/neohaskell/NeoHaskell/issues/305
- ADR: `docs/decisions/0013-automatic-schema-generation.md`
- Related: `core/meta/TypeName.hs` (existing type reflection)
- Related: `core/traits/Default.hs` (default typeclass pattern)
- Workflow: `.sisyphus/plans/secure-feature-workflow.md` (Standard Workflow)
