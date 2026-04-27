```yaml
version: 1
adr: docs/decisions/0055-uuid-v5-support.md

modules:
  - name: Uuid
    path: core/core/Uuid.hs
    exposed: true
    purpose: Wraps Data.UUID to provide the Uuid primitive type with v4 random and v5 deterministic generation.
    public_api:
      - signature: |
          generateV5 :: Uuid -> Text -> Uuid
        purpose: Generate a deterministic UUID v5 by hashing a UTF-8-encoded name under a namespace UUID via SHA-1 (RFC 4122 §4.3). Subject argument last for pipe chains.
        reuses: []
      - signature: |
          generate :: Task _ Uuid
        purpose: Generate a random v4 UUID via IO; subject argument last.
        reuses: []
      - signature: |
          fromText :: Text -> Maybe Uuid
        purpose: Parse a UUID from its canonical hyphenated text representation; returns Nothing on invalid input.
        reuses: []
      - signature: |
          toText :: Uuid -> Text
        purpose: Render a UUID to its canonical hyphenated text form; subject last for pipe chains.
        reuses: []
      - signature: |
          nil :: Uuid
        purpose: The all-zeros nil UUID; useful as a well-known namespace sentinel or default.
        reuses: []
    types:
      - name: Uuid
        kind: newtype
        definition: |
          newtype Uuid = Uuid UUID.UUID
            deriving (Eq, Ord, Generic)
    instances:
      - of: ToJSON
        for: Uuid
        location: same-module-as-type
      - of: FromJSON
        for: Uuid
        location: same-module-as-type
      - of: Show
        for: Uuid
        location: same-module-as-type
      - of: Default
        for: Uuid
        location: same-module-as-type
    imports:
      nhcore: [Text, Task]
      project: []
      base: []

  - name: Decider
    path: core/service/Decider.hs
    exposed: true
    purpose: Provides the Decision free monad for command handler authors, with smart constructors for random/deterministic UUID generation and command acceptance/rejection.
    public_api:
      - signature: |
          generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
          {-# INLINE generateDeterministicUuid #-}
        purpose: Produce a deterministic UUID v5 inside a Decision chain by wrapping Uuid.generateV5 in Return — no new ADT constructor, no interpreter change required.
        reuses: [Uuid.generateV5]
      - signature: |
          generateUuid :: Decision Uuid
        purpose: Generate a random v4 UUID via the GenUuid free monad constructor; resolved at interpreter time.
        reuses: []
      - signature: |
          runDecision :: HasCallStack => DecisionContext -> Decision a -> Task Text (CommandResult a)
        purpose: Execute a Decision in a Task context, threading the DecisionContext for effectful UUID generation.
        reuses: []
      - signature: |
          acceptAny :: EventVariantOf event variant => Array variant -> Decision event
        purpose: Accept a command regardless of stream state, emitting the given events.
        reuses: [Array.map]
      - signature: |
          acceptNew :: EventVariantOf event variant => Array variant -> Decision event
        purpose: Accept a command only if the stream does not yet exist (entity creation guard).
        reuses: [Array.map]
      - signature: |
          acceptExisting :: EventVariantOf event variant => Array variant -> Decision event
        purpose: Accept a command only if the stream already exists (entity update guard).
        reuses: [Array.map]
      - signature: |
          acceptAfter :: EventVariantOf event variant => StreamPosition -> Array variant -> Decision event
        purpose: Accept a command only at an exact stream position for optimistic concurrency control; StreamPosition is subject-penultimate, Array variants is the last payload argument.
        reuses: [Array.map]
      - signature: |
          reject :: Text -> Decision a
        purpose: Reject a command with a plain-text reason; subject last for pipe chains.
        reuses: []
    types:
      - name: Decision
        kind: data
        definition: |
          data Decision a where
            Return  :: a -> Decision a
            Bind    :: Decision a -> (a -> Decision b) -> Decision b
            GenUuid :: Decision Uuid
            Accept  :: InsertionType -> Array a -> Decision a
            Reject  :: Text -> Decision a
      - name: DecisionContext
        kind: data
        definition: |
          data DecisionContext = DecisionContext
            { genUuid :: Task Text Uuid
            }
      - name: CommandResult
        kind: data
        definition: |
          data CommandResult event
            = AcceptCommand InsertionType (Array event)
            | RejectCommand Text
            deriving (Eq, Show, Ord, Generic)
    instances: []
    imports:
      nhcore: [Array, Text, Task, Uuid]
      project: [EventVariantOf, Service.Event]
      base: []

  - name: DeciderSpec
    path: core/test/DeciderSpec.hs
    exposed: false
    purpose: Unit tests for Decider and Uuid — determinism of generateDeterministicUuid, RFC 4122 v5 DNS reference vector, and monadic binding confirming GenUuid is never invoked for the deterministic path.
    public_api:
      - signature: |
          spec :: Spec Unit
        purpose: Test suite entry point consumed by the nhcore-test runner via Main.hs.
        reuses: []
    types: []
    instances: []
    imports:
      nhcore: [Core, Test, Decider, Uuid, Array, Maybe, Task]
      project: []
      base: []

dependencies:
  edges:
    - from: Decider
      to: Uuid
      reason: generateDeterministicUuid delegates the pure SHA-1 computation to Uuid.generateV5; Decider must not import Data.UUID.V5 directly.
    - from: DeciderSpec
      to: Decider
      reason: Spec exercises Decision chains including generateDeterministicUuid and runDecision to verify no GenUuid effect is triggered.
    - from: DeciderSpec
      to: Uuid
      reason: Spec uses Uuid.generateV5 and Uuid.fromText to construct namespace and expected-value fixtures.

integration_points:
  - existing_module: core/service/Decider.hs
    change: add-export
    detail: >
      generateDeterministicUuid is implemented at line 132, exported at line 17, and marked {-# INLINE #-} at line 135.
  - existing_module: core/core/Uuid.hs
    change: add-export
    detail: >
      Uuid.generateV5 is exported at line 9. No further change required.
  - existing_module: core/test/DeciderSpec.hs
    change: add-export
    detail: >
      spec already covers three cases: (1) determinism — same inputs produce the same UUID;
      (2) RFC 4122 v5 DNS reference vector (DNS namespace + "www.example.com" → 2ed6657d-e927-568b-95e1-2665a8aea6a2);
      (3) monadic binding confirming GenUuid is never invoked for generateDeterministicUuid.
      No additional tests are required.

cabal_changes:
  - file: core/nhcore.cabal
    stanza: library
    add_exposed_modules: []
    add_other_modules: []
    add_build_depends: []
    note: >
      Uuid (line 355) and Decider (line 174) are already in exposed-modules.
      The uuid package (supplying Data.UUID.V5) is already listed under build-depends (line 86).
      No cabal changes needed.
  - file: core/nhcore.cabal
    stanza: test-suite nhcore-test
    add_exposed_modules: []
    add_other_modules: [DeciderSpec]
    add_build_depends: []
    note: >
      DeciderSpec was added to other-modules for nhcore-test in this PR.

flags_for_human_review: []

reuse_summary:
  - utility: Uuid.generateV5
    used_in: [Decider.generateDeterministicUuid]
    reason: >
      All SHA-1 / RFC 4122 v5 logic lives in Uuid.generateV5.
      Decider.generateDeterministicUuid is a one-line Return wrapper; duplicating
      the computation in Decider would violate the single-responsibility split between
      the primitive type layer and the free monad API layer.
  - utility: Data.UUID.V5.generateNamed
    used_in: [Uuid.generateV5]
    reason: >
      Uuid.hs already wraps this low-level GHC primitive correctly with UTF-8 encoding.
      Decider must not import Data.UUID.V5 directly — always go through Uuid.generateV5.

do_not_create:
  - considered: DeterministicUuid standalone helper module
    nhcore_alternative: Uuid.generateV5 — the full implementation is already there and exported.
  - considered: Uuid.Namespace sum type with DNS/URL/OID/X500 pre-baked RFC 4122 constants
    nhcore_alternative: >
      Uuid.nil (core/core/Uuid.hs:63) and Uuid.fromText (core/core/Uuid.hs:77) cover all
      caller-defined namespace construction. Pre-baked constants cannot model OAuth sub claims
      or domain-specific namespaces — the ADR Decision Point 3 explicitly rejects this option.
  - considered: New GenDeterministicUuid constructor in the Decision free monad ADT
    nhcore_alternative: >
      Decider.generateDeterministicUuid wraps Uuid.generateV5 in Return at core/service/Decider.hs:132.
      UUID v5 is a total pure function; an ADT constructor would demand an interpreter branch
      for a computation that requires no effect — a category error the ADR Decision Point 1 explicitly rejects.
  - considered: generateDeterministicUuid :: Uuid -> Text -> Task Text Uuid (effectful variant)
    nhcore_alternative: >
      The function is total with no error path; wrapping in Task forces dead error-handling
      code at every call site. The Decision Uuid return type via Return is correct.
```
