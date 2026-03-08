# NeoQL MVP: Field Access + Equality Filtering

> **Pipeline**: neohaskell-feature-pipeline (17-phase)
> **Issue**: [#447](https://github.com/neohaskell/NeoHaskell/issues/447)
> **Full Spec**: [#391](https://github.com/neohaskell/NeoHaskell/issues/391)

## Pipeline Variables

| Variable | Value |
|----------|-------|
| `FEATURE_NAME` | NeoQL MVP: Field Access + Equality Filtering |
| `ISSUE_NUMBER` | 447 |
| `ISSUE_URL` | `https://github.com/neohaskell/NeoHaskell/issues/447` |
| `MODULE_PATH` | `core/neoql/NeoQL.hs`, `core/neoql/NeoQL/Types.hs`, `core/neoql/NeoQL/Parser.hs`, `core/neoql/NeoQL/Execute.hs` |
| `TEST_PATH` | `core/test-core/NeoQL/ParserSpec.hs`, `core/test-core/NeoQL/ExecuteSpec.hs` |
| `ADR_TITLE` | NeoQL MVP — Field Access and Equality Filtering |
| `BRANCH_NAME` | `feature/neoql-mvp` |

## TL;DR

> **Quick Summary**: Implement the minimal viable NeoQL — a `.fieldName` field-access and `.fieldName == value` equality-filter language parsed by Megaparsec, executed against Aeson.Value, and integrated into the WebTransport `?q=` query parameter for HTTP query endpoints.
>
> **Deliverables**:
> - ADR-0040: NeoQL MVP design document
> - `core/neoql/` — NeoQL package (Types, Parser, Execute modules)
> - `core/test-core/NeoQL/` — Parser and Executor test suites
> - Modified `core/service/Service/Query/Endpoint.hs` — filtering integration
> - Modified `core/service/Service/Transport/Web.hs` — `?q=` param extraction
> - Modified `core/nhcore.cabal` — new source dir, megaparsec dependency, test registration
>
> **Estimated Effort**: Medium (~7.5 hours)
> **Critical Path**: ADR → Cabal Setup → Types → Parser → Executor → WebTransport Integration → Tests

---

## Scope Contract (STRICT)

### In Scope (MVP Only)

- `.fieldName` — field access / projection on JSON objects
- `.fieldName == "string"` — string equality filtering
- `.fieldName == 42` — integer/number equality filtering
- `?q=` query parameter support in WebTransport query endpoints
- Megaparsec parser for MVP grammar
- In-memory executor on `Aeson.Value`
- Parser error → HTTP 400 mapping
- Comprehensive parser + executor tests

### Out of Scope (Explicitly Excluded — from #391)

- **Boolean operators**: `and`, `or`, `not` — deferred
- **Comparison operators**: `>`, `<`, `>=`, `<=` — deferred
- **Nested field access**: `.address.city` — deferred
- **Array operations**: `any`, `all`, `length`, `contains` — deferred
- **String matching**: `contains`, `startsWith`, `endsWith`, `matches` — deferred
- **Membership**: `in`, `not in` — deferred
- **Null checks**: `== null`, `!= null`, `exists()` — deferred
- **Sort/limit/offset**: `sort=`, `limit=`, `offset=` params — deferred
- **`[ql|...|]` quasiquoter**: deferred to follow-up PR (riskiest component)
- **DB-level pushdown**: No `QueryObjectStore` API changes — in-memory only
- **Type validation/hints**: No "did you mean?" — silent non-match for unknown fields
- **Complexity warnings**: No `X-NeoQL-Warning` headers

### MVP Grammar (Frozen)

```
expr        = fieldAccess ( "==" value )?
fieldAccess = "." identifier
value       = string | number
identifier  = letter (letter | digit | "_")*
string      = '"' { char } '"'
number      = [ "-" ] digit { digit } [ "." digit { digit } ]
```

### MVP Error Contract

| Condition | HTTP Status | Response |
|-----------|-------------|----------|
| Valid `?q=`, matches found | 200 | Filtered JSON array |
| Valid `?q=`, no matches | 200 | Empty JSON array `[]` |
| No `?q=` param | 200 | All results (existing behavior) |
| Invalid `?q=` syntax | 400 | `{"error":"parse_error","message":"..."}` |
| Unknown field in `.fieldName` | 200 | Empty array (silent non-match) |
| Auth: unauthenticated | 401 | Existing auth error (unchanged) |
| Auth: forbidden | 403 | Existing auth error (unchanged) |
| Storage error | 500 | Existing storage error (unchanged) |

### MVP Response Shape

- **Filtering** (`.status == "pending"`): Returns full objects that match → `[{...}, {...}]`
- **Projection** (`.status` alone): Returns full objects (projection deferred to full spec)

### Security Ordering (Non-Negotiable)

NeoQL filtering is applied **AFTER** both authorization phases:
```
canAccessImpl (pre-fetch) → getAll → canViewImpl (post-fetch) → NeoQL filter → JSON encode
```
NeoQL NEVER reveals existence of unauthorized records.

---

## Pipeline Phases

### PHASE 1: ADR Draft

**Agent**: `category="writing"` with NeoHaskell domain context

**What to do**:
- Determine next ADR number: `ls docs/decisions/*.md | tail -1` → next is **0040**
- Create `docs/decisions/0040-neoql-mvp.md` following the ADR template
- Content covers: MVP grammar, Aeson.Value execution model, WebTransport integration point, security ordering, and why megaparsec was chosen over lighter parsers
- Update `docs/decisions/README.md` to add row: `[0040](0040-neoql-mvp.md) | NeoQL MVP — Field Access and Equality Filtering | Proposed`

**References**:
- `docs/decisions/0022-decimal-type.md` — Example of a recent type-addition ADR
- `docs/decisions/README.md` — ADR index table (last entry is 0039)
- Issue #447 — Requirements source
- Issue #391 — Full NeoQL spec (for "out of scope" section)

**Acceptance Criteria**:
- [ ] `docs/decisions/0040-neoql-mvp.md` exists with all ADR sections
- [ ] `docs/decisions/README.md` contains row for 0040
- [ ] ADR explicitly documents MVP grammar, security ordering, and deferred features

---

### PHASE 2: Security Review

**Agent**: `category="deep"` playing `neohaskell-security-architect` role

**What to do**:
Review ADR-0040 against:

**OWASP Assessment**:
- A03 Injection: Parser must reject all input outside MVP grammar — no eval, no code injection via `?q=`
- A04 Insecure Design: Filtering must never leak unauthorized data existence
- A08 Data Integrity: Filter results must be a subset of authorized results

**NIST Assessment**:
- Input validation: Megaparsec parser is the validation boundary — reject early, reject clearly
- Input length limits: Cap `?q=` at 500 chars to prevent parser abuse

**Specific Checks**:
- Can an attacker enumerate field names via timing differences? (No — in-memory filter on already-fetched data)
- Can `?q=` bypass `canAccessImpl` or `canViewImpl`? (No — applied after both)
- Can malformed queries cause server errors? (Must be 400, never 500)

**Output**: Security notes recorded in `.sisyphus/notepads/neoql-security-notes.md`

**References**:
- `core/service/Service/Query/Endpoint.hs` — Two-phase auth flow (lines 44-61)
- `core/service/Service/Transport/Web.hs:582-636` — Query handler dispatch
- ADR-0036 (`docs/decisions/0036-wave1-security-hardening.md`) — Previous security hardening decisions

**Acceptance Criteria**:
- [ ] OWASP A03/A04/A08 assessment documented
- [ ] Input length cap recommendation documented
- [ ] Auth ordering verified as safe
- [ ] No blocking security concerns

---

### PHASE 3: Performance Review

**Agent**: `category="deep"` playing `neohaskell-performance-lead` role

**What to do**:
Review ADR-0040 for 50k req/s target impact:

- **Serialization**: Parser runs per-request on `?q=` text — must be sub-millisecond for MVP grammar
- **Hot path**: `getAll` + `Array.takeIf` is already O(n) — adding NeoQL filter is O(n) again, but on already-fetched data
- **Memory**: No additional allocations beyond parsed AST (2-3 constructors) and Aeson.Value traversal
- **Caching**: Parser output could be cached per unique `?q=` string (defer to optimization phase)

**Output**: Performance notes in `.sisyphus/notepads/neoql-performance-notes.md`

**References**:
- `core/service/Service/Query/Endpoint.hs:50-57` — Current `getAll` + `Array.takeIf` pattern
- `core/core/Array.hs:363-364` — `takeIf` implementation (Data.Vector.filter)

**Acceptance Criteria**:
- [ ] Parse cost assessment documented (expected sub-ms for MVP grammar)
- [ ] Filtering cost assessment documented (linear scan acceptable for MVP)
- [ ] No INLINE pragma requirements identified for MVP
- [ ] No blocking performance concerns

---

### PHASE 4: DevEx Lead Review — PAUSE

**Agent**: `category="deep"` playing `neohaskell-devex-lead` role

**What to do**:
Review ADR-0040 with checklist:
- [ ] API is intuitive for NeoHaskell users (`.fieldName == value` is natural, jq-inspired)
- [ ] Consistent with existing patterns (pipe operator, qualified imports, NeoQL.parse)
- [ ] No breaking changes to existing query endpoints (additive `?q=` only)
- [ ] Error messages are clear ("NeoQL parse error: unexpected '>' at position 12")
- [ ] Migration path: none needed (additive)

Update ADR with refinements.

**PAUSE: Report ADR status and wait for human approval before continuing.**

**References**:
- `core/core/Basics.hs:960-961` — `fmt` quasiquoter pattern (for future `[ql|...|]` reference)
- `core/core/Version.hs:58-65` — `version` quasiquoter pattern (compile-time validation)
- `.cursor/rules/neohaskell-style.mdc` — NeoHaskell style rules

**Acceptance Criteria**:
- [ ] DevEx checklist passes
- [ ] ADR updated with DevEx feedback
- [ ] Human approves ADR

---

### PHASE 5: Architecture Design — PAUSE

**Agent**: `category="deep"` playing `neohaskell-devex-lead` role

**What to do**:
Create architecture document in `.sisyphus/notepads/neoql-arch-doc.md`:

**Module Placement**:
```
core/
  neoql/                          -- NEW hs-source-dir
    NeoQL.hs                      -- Re-exports (NeoQL.parse, NeoQL.execute, NeoQL.project)
    NeoQL/
      Types.hs                    -- AST: NeoQLExpr, Value
      Parser.hs                   -- Megaparsec parser
      Execute.hs                  -- Filter execution on Aeson.Value
```

**Public API** (NeoHaskell style):
```haskell
-- NeoQL.hs re-exports
NeoQL.parse    :: Text -> Result Text NeoQLExpr
NeoQL.execute  :: NeoQLExpr -> Aeson.Value -> Bool
```

**Integration Points**:
- `core/nhcore.cabal` — Add `neoql` to hs-source-dirs, add `megaparsec` dependency, expose modules
- `core/service/Service/Query/Endpoint.hs` — Add `Maybe NeoQLExpr` parameter, apply filter after auth
- `core/service/Service/Transport/Web.hs` — Extract `?q=`, parse, pass to endpoint handler
- `core/service/Service/Transport.hs` — Update `QueryEndpointHandler` type signature

**Dependencies**:
- `megaparsec >= 9.0 && < 10` — Parser combinator library
- All existing nhcore modules (Text, Result, Array, Json)

**PAUSE: Report architecture and wait for human approval.**

**References**:
- `core/nhcore.cabal` — Current hs-source-dirs (9 dirs), dependency list, module registration
- `core/json/Json.hs` — Wrapper module pattern (re-exports Aeson with NeoHaskell API)
- `core/core/Core.hs` — Re-export pattern (`module Reexported`)
- `core/service/Service/Transport.hs:46` — `QueryEndpointHandler` type definition

**Acceptance Criteria**:
- [ ] Architecture document created
- [ ] Module placement approved
- [ ] Public API signatures approved
- [ ] Human approves architecture

---

### PHASE 6: Test Suite Definition

**Agent**: `category="deep"`

**What to do**:
Create test files. **Tests are IMMUTABLE once written.**

**File 1**: `core/test-core/NeoQL/ParserSpec.hs`
```
Test categories:
- Valid field access: ".status", ".createdAt", ".customer_name"
- Valid equality (string): '.status == "pending"', '.name == "Alice"'
- Valid equality (number): ".count == 42", ".price == 99.99", ".balance == -10"
- Invalid syntax rejection: "status" (no dot), ".123" (no digit start), '.status =' (single equals), '.status == ' (trailing operator), empty string
- Out-of-scope syntax rejection: '.a and .b', '.x > 5', '.a.b' (nested), '.status != "x"'
- Whitespace handling: ". status == \"pending\"" (spaces around dot/operator)
- Edge cases: very long identifiers, unicode in strings, empty string value '""'
```

**File 2**: `core/test-core/NeoQL/ExecuteSpec.hs`
```
Test categories:
- Field access match: filter {"status":"pending"} with .status == "pending" → True
- Field access no match: filter {"status":"active"} with .status == "pending" → False
- Number equality: filter {"count":42} with .count == 42 → True
- Number inequality: filter {"count":99} with .count == 42 → False
- Unknown field: filter {"status":"pending"} with .unknown == "x" → False (silent non-match)
- Type mismatch: filter {"count":"42"} (string) with .count == 42 (number) → False (strict typing)
- Array of objects: filter array keeping only matching objects
- Empty array: filter empty array → empty array
- Non-object values: filter on non-object Aeson.Value → False
```

**Registration**:
- Add `NeoQL.ParserSpec` and `NeoQL.ExecuteSpec` to `core/test-core/Main.hs` imports and describe blocks
- Add both modules to `other-modules` in `nhcore-test-core` test suite in `core/nhcore.cabal`

**References**:
- `core/test-core/Main.hs` — Manual test registration pattern (import qualified + describe block)
- `core/test/DecimalSpec.hs` — JSON round-trip test patterns
- `core/test/SetSpec.hs` — Collection filtering tests with `takeIf`
- `core/nhcore.cabal` — `nhcore-test-core` test suite, `other-modules` list

**Acceptance Criteria**:
- [ ] `core/test-core/NeoQL/ParserSpec.hs` exists with all test categories
- [ ] `core/test-core/NeoQL/ExecuteSpec.hs` exists with all test categories
- [ ] Both registered in `core/test-core/Main.hs`
- [ ] Both listed in `core/nhcore.cabal` under `nhcore-test-core` other-modules
- [ ] `cabal build nhcore-test-core` compiles (tests will fail — not yet implemented)

---

### PHASE 7: Implementation

**Agent**: `category="deep"`

**What to do** (in order):

**Step 1: Cabal Changes** (`core/nhcore.cabal`):
- Add `neoql` to `hs-source-dirs` list
- Add `megaparsec >= 9.0 && < 10` to `build-depends` in `common_cfg`
- Add `NeoQL`, `NeoQL.Types`, `NeoQL.Parser`, `NeoQL.Execute` to `exposed-modules`

**Step 2: Types** (`core/neoql/NeoQL/Types.hs`):
```haskell
-- ~30 lines
data NeoQLExpr
  = FieldAccess Text
  | Equals Text Value

data Value
  = VString Text
  | VNumber Scientific
```
Follow NeoHaskell conventions: descriptive type params, no `let..in`, no `where`.

**Step 3: Parser** (`core/neoql/NeoQL/Parser.hs`):
```haskell
-- ~60 lines
-- Megaparsec parser for MVP grammar
-- parse :: Text -> Result Text NeoQLExpr
-- Input length cap: 500 chars (security requirement)
-- Error formatting: user-friendly via errorBundlePretty
```
Use `Text.Megaparsec`, `Text.Megaparsec.Char`, `Text.Megaparsec.Char.Lexer`.
Import base Megaparsec modules with `Ghc` prefix per NeoHaskell convention.

**Step 4: Executor** (`core/neoql/NeoQL/Execute.hs`):
```haskell
-- ~40 lines
-- execute :: NeoQLExpr -> Aeson.Value -> Bool
-- For FieldAccess: always returns True (projection, no filtering)
-- For Equals: lookup field in JSON object, compare with value
-- Non-object values: return False
-- Unknown fields: return False
```
Operates on `Aeson.Value` via pattern matching on `Aeson.Object`, `Aeson.String`, `Aeson.Number`.

**Step 5: Re-export Module** (`core/neoql/NeoQL.hs`):
```haskell
-- Re-export public API
module NeoQL (
  parse,
  execute,
  NeoQLExpr (..),
  Value (..),
) where
```

**Step 6: WebTransport Integration** (`core/service/Service/Transport/Web.hs`):
- In the query handler block (lines 582-636):
  - Extract `?q=` from `Wai.queryString request` using existing pattern (lines 687-692)
  - Parse with `NeoQL.parse`
  - On parse failure: return HTTP 400 with error JSON
  - On success: pass `Maybe NeoQLExpr` to endpoint

**Step 7: Endpoint Integration** (`core/service/Service/Query/Endpoint.hs`):
- Modify `createQueryEndpoint` to accept `Maybe NeoQLExpr` parameter
- After `canViewImpl` filtering (line 57), apply NeoQL filter:
  ```haskell
  let filteredQueries = case maybeExpr of
        Nothing -> authorizedQueries
        Just expr -> authorizedQueries
          |> Array.takeIf (\query -> NeoQL.execute expr (Json.encode query))
  ```

**Step 8: Transport Type** (`core/service/Service/Transport.hs`):
- Update `QueryEndpointHandler` type if needed (or pass filter through existing handler)

**Must NOT do**:
- Add boolean operators, comparison operators, nested fields, or any #391 features
- Modify `QueryObjectStore` interface
- Change `canAccessImpl` or `canViewImpl` behavior
- Add `[ql|...|]` quasiquoter
- Add sort/limit/offset parameters
- Use `let..in`, `where`, point-free style, or single-letter type params

**References**:
- `core/nhcore.cabal` — hs-source-dirs list, build-depends, exposed-modules
- `core/json/Json.hs` — Json module API (`Json.encode`, `Json.encodeText`, `Aeson.Value`)
- `core/core/Array.hs:363-364` — `takeIf` implementation
- `core/service/Service/Query/Endpoint.hs:44-61` — Two-phase auth + filtering pattern
- `core/service/Service/Transport/Web.hs:582-636` — Query handler dispatch
- `core/service/Service/Transport/Web.hs:687-692` — Query parameter extraction pattern
- `core/service/Service/Transport.hs:46` — `QueryEndpointHandler` type
- `.cursor/rules/neohaskell-style.mdc` — NeoHaskell code style rules

**Acceptance Criteria**:
- [ ] All 4 NeoQL modules exist and compile
- [ ] `cabal build all` succeeds
- [ ] WebTransport extracts `?q=` and passes to endpoint
- [ ] Endpoint applies NeoQL filter after authorization
- [ ] No `#391` features implemented

---

### PHASE 8: Build & Test Loop

**Agent**: `category="deep"`

**What to do**:
Run until all tests pass (max 10 iterations):

```bash
cabal build all
cabal test nhcore-test-core
```

**If tests fail**: Fix implementation. **NEVER modify tests.**

**References**:
- `core/test-core/NeoQL/ParserSpec.hs` — Parser tests
- `core/test-core/NeoQL/ExecuteSpec.hs` — Executor tests

**Acceptance Criteria**:
- [ ] `cabal build all` → success
- [ ] `cabal test nhcore-test-core` → all NeoQL tests pass, 0 failures

---

### PHASE 9: Security Implementation Review

**Agent**: `category="deep"` playing `neohaskell-security-architect` role

**What to do**:
Review implemented code against security notes:

- [ ] Input validation: Parser rejects all non-MVP syntax
- [ ] No injection vectors: Megaparsec is a pure parser, no eval
- [ ] Input length cap: `?q=` longer than 500 chars returns 400
- [ ] Auth ordering preserved: NeoQL applied after both canAccessImpl and canViewImpl
- [ ] No information disclosure: Unknown fields silently return empty results
- [ ] Error messages don't reveal internal structure

**Output**: `security-impl-notes.md` in `.sisyphus/notepads/`

**References**:
- `core/neoql/NeoQL/Parser.hs` — Parser implementation
- `core/service/Service/Query/Endpoint.hs` — Modified auth + filter flow
- `core/service/Service/Transport/Web.hs` — `?q=` extraction and error handling
- `.sisyphus/notepads/neoql-security-notes.md` — Phase 2 security notes

**Acceptance Criteria**:
- [ ] All security checklist items pass
- [ ] No blocking security issues

---

### PHASE 10: Performance Implementation Review

**Agent**: `category="deep"` playing `neohaskell-performance-lead` role

**What to do**:
Review implemented code against performance notes:

- [ ] Parser is not called in a loop (once per request)
- [ ] No unnecessary Aeson.Value copies during filtering
- [ ] `Array.takeIf` is used (single pass, not nested loops)
- [ ] No space leaks in parser or executor
- [ ] Strict fields on NeoQL AST types where appropriate

**Output**: `performance-impl-notes.md` in `.sisyphus/notepads/`

**References**:
- `core/neoql/NeoQL/Types.hs` — AST types (check strict fields)
- `core/neoql/NeoQL/Execute.hs` — Executor (check allocation patterns)
- `.sisyphus/notepads/neoql-performance-notes.md` — Phase 3 performance notes

**Acceptance Criteria**:
- [ ] No blocking performance issues
- [ ] INLINE pragmas added if needed (likely not for MVP)

---

### PHASE 11: Fix Review Notes

**Agent**: `category="deep"`

**What to do**:
Apply fixes from security (Phase 9) and performance (Phase 10) reviews.
**Tests remain IMMUTABLE.**

**Acceptance Criteria**:
- [ ] All security review items addressed
- [ ] All performance review items addressed
- [ ] Tests still pass

---

### PHASE 12: Final Build & Test

**Agent**: `category="deep"`

**What to do**:
```bash
cabal build all
cabal test nhcore-test-core
hlint core/neoql/
hlint core/service/Service/Query/Endpoint.hs
hlint core/service/Service/Transport/Web.hs
```

All tests must pass. All hlint warnings resolved.

**Acceptance Criteria**:
- [ ] `cabal build all` → success
- [ ] `cabal test nhcore-test-core` → 0 failures
- [ ] `hlint` → 0 warnings on all changed files

---

### PHASE 13: Create PR — PAUSE

**Agent**: Uses `git-master` skill

**What to do**:
```bash
git checkout -b feature/neoql-mvp
git add --all
git commit -m "feat(core): NeoQL MVP — field access + equality filtering

Closes #447

- Add NeoQL package (Types, Parser, Execute) with Megaparsec
- Add ?q= query parameter support to WebTransport query endpoints
- Add ADR-0040 documenting NeoQL MVP design
- Parser supports .fieldName and .fieldName == value syntax
- Executor filters Aeson.Value in-memory after authorization
- Security: filtering applied after both canAccessImpl and canViewImpl"

git push -u origin feature/neoql-mvp
gh pr create --title "feat(core): NeoQL MVP — field access + equality filtering" --body "$(cat <<'EOF'
Closes #447

## Summary
- Implements minimal NeoQL: `.fieldName` access and `.fieldName == value` equality filtering
- Adds `?q=` query parameter to WebTransport query endpoints
- Pure Megaparsec parser, in-memory Aeson.Value executor
- Security-safe: filtering applied strictly after two-phase authorization

## Changes
- **New**: `core/neoql/` — NeoQL package (Types, Parser, Execute)
- **New**: `docs/decisions/0040-neoql-mvp.md` — ADR
- **New**: `core/test-core/NeoQL/` — Parser and Executor tests
- **Modified**: `core/nhcore.cabal` — neoql source dir, megaparsec dep
- **Modified**: `core/service/Service/Query/Endpoint.hs` — filter integration
- **Modified**: `core/service/Service/Transport/Web.hs` — ?q= extraction

## Checklist
- [x] ADR created (0040)
- [x] Security review passed
- [x] Performance review passed
- [x] Tests written and passing
- [x] hlint clean
EOF
)"
```

**PAUSE: Report PR URL and wait for bot reviews.**

**Acceptance Criteria**:
- [ ] PR created with correct title and body
- [ ] All CI checks initiated

---

### PHASE 14: Wait for Bots — PAUSE

Wait for:
- CodeRabbit AI review
- CI checks (Linux + macOS workflows)

**PAUSE: Report bot comments, ask if human wants to review before fixing.**

---

### PHASE 15: Fix Bot Comments

**Agent**: `category="deep"`

Address issues raised by CI and CodeRabbit. **Tests remain IMMUTABLE.**

**Acceptance Criteria**:
- [ ] All bot comments addressed
- [ ] CI passes

---

### PHASE 16: Final Review

**Agent**: `category="deep"`

Verify:
- [ ] All bot comments addressed
- [ ] `cabal build all && cabal test nhcore-test-core` passes
- [ ] No security regressions (auth ordering intact)
- [ ] Performance acceptable (no new allocations in hot path)
- [ ] NeoHaskell style compliance (pipes, qualified imports, do blocks, no `let..in`)
- [ ] Out-of-scope features not accidentally included

---

### PHASE 17: Merge — PAUSE

Wait for CI to pass.

**PAUSE: "CI passed. Merge PR? [y/n]"**

On approval:
```bash
gh pr merge --squash
```

---

## Deliverables Checklist

- [ ] `docs/decisions/0040-neoql-mvp.md` — ADR
- [ ] `core/neoql/NeoQL.hs` — Re-export module
- [ ] `core/neoql/NeoQL/Types.hs` — AST types
- [ ] `core/neoql/NeoQL/Parser.hs` — Megaparsec parser
- [ ] `core/neoql/NeoQL/Execute.hs` — Filter executor
- [ ] `core/test-core/NeoQL/ParserSpec.hs` — Parser tests
- [ ] `core/test-core/NeoQL/ExecuteSpec.hs` — Executor tests
- [ ] `core/nhcore.cabal` — Updated (source dir, dependency, modules, test registration)
- [ ] `core/service/Service/Query/Endpoint.hs` — Modified (NeoQL filter integration)
- [ ] `core/service/Service/Transport/Web.hs` — Modified (?q= extraction)
- [ ] `core/test-core/Main.hs` — Modified (test registration)
- [ ] PR merged to main
- [ ] Issue #447 closed

## Agent Coordination Reference

| Agent | Phases | Role |
|-------|--------|------|
| `neohaskell-devex-lead` | 1, 4, 5 | API design, naming, module structure, ADR |
| `neohaskell-security-architect` | 2, 9 | Security review (OWASP/NIST) |
| `neohaskell-performance-lead` | 3, 10 | Performance review (50k req/s target) |
| `git-master` (skill) | 13, 17 | Git operations, PR creation, merge |

## NeoHaskell Style Quick Reference

| Rule | Example |
|------|---------|
| Pipe over nesting | `x \|> foo \|> bar` not `bar $ foo x` |
| Do blocks only | `do let y = ...` not `let y = ... in ...` |
| Case only | `case x of ...` not pattern matching in function defs |
| Descriptive types | `forall element result.` not `forall a b.` |
| Qualified imports | `Module.function` design |
| String interpolation | `[fmt\|Hello {name}!\|]` not `<>` |
| Result not Either | `Result error value` |
| Task not IO | `Task err val` |
| Strict hot paths | `!` fields, `{-# INLINE fn #-}` |
| Base imports | `import Data.Foo qualified as GhcFoo` |

## Success Criteria

### Verification Commands
```bash
cabal build all                    # Expected: Build succeeds
cabal test nhcore-test-core        # Expected: All NeoQL tests pass
hlint core/neoql/                  # Expected: 0 hints
```

### Final Checklist
- [ ] `.fieldName` parsing works
- [ ] `.fieldName == "string"` parsing works
- [ ] `.fieldName == 42` parsing works
- [ ] Invalid syntax returns parse error
- [ ] Out-of-scope syntax rejected (no `and`, `or`, `>`, nested paths)
- [ ] `?q=` param extracted from HTTP request
- [ ] Filter applied after authorization (security ordering)
- [ ] Unknown fields return empty results (no error)
- [ ] No breaking changes to existing query endpoints
- [ ] All tests pass
- [ ] hlint clean
