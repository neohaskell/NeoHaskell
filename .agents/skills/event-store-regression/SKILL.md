---
name: event-store-regression
description: Select and run the maintained event-store regression fixtures against known-good and controlled-bad behavior when changing append preconditions, payloads, conflict refetch or concurrent stream creation.
---

# Prove event-store regression sensitivity

Use [the recipe records](references/recipes.json) to find an existing independently specified assertion for the changed contract. The maintained `Regression` test module supplies service-record wrappers and barriers. Keep that implementation and its Hspec tests in the framework test library.

Run `python3 .agents/skills/event-store-regression/scripts/smoke.py check` to verify recipe locations. This checks metadata only. Run `python3 .agents/skills/event-store-regression/scripts/smoke.py run --recipe <name>` to execute the same selected examples against normal behavior and their controlled fault. Omit `--recipe` for all recipes. Provide the services required by the selected test suite. Missing prerequisites are failures, not successful proofs.

The fault changes the exercised behavior. It must not merely throw a sentinel before reaching the implementation. Require the named assertion failure in the expected example, with the expected nonempty selection. The runner rejects setup failures, compiler failures, timeouts and unrelated assertions.

Keep before and after logs and source hashes under `.agents-state/event-store-regression/`. A passing sensitivity run proves these selected tests can detect their controlled violations. Preserve the broader event-store tests for event identity, ordering, payloads, replay and historical integrity. These recipes do not replace that coverage.

For a new regression, follow [TDD](../tdd/SKILL.md) and [Regression evidence](../regression-evidence/SKILL.md). Use barriers and the owning NeoHaskell concurrency primitives for coordination, with meaningful examples or properties and independent expected outcomes.
