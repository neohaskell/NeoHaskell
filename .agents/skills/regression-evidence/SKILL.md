---
name: regression-evidence
description: Capture repeatable failing and passing evidence for NeoHaskell runtime regressions or compiler acceptance and expected rejection, with distinct checks for assertion failures and diagnostics.
---

# Capture the actual regression

Choose the boundary with [Verify This](../verify-this/SKILL.md). Use the [TDD workflow](../tdd/SKILL.md) for a focused runtime regression. Use real positive and negative consumers for a compiler or generated-API contract.

For Hspec, run `python3 .agents/skills/regression-evidence/scripts/evidence.py runtime --repo <root> --input <affected-source> --test <exact-example> --expect fail --failure <independent-assertion-text> -- <test-command>`. Repeat with `--expect pass` after the fix. The command must run the affected implementation. Expected failure needs one matching assertion failure, a nonempty selection and no setup or compiler failure.

For compiler contracts, run `python3 .agents/skills/regression-evidence/scripts/evidence.py compiler --repo <root> --positive <accept.hs> --negative <reject.hs> --diagnostic <expected-regex> -- <ghc-flags>`. The helper first requires a successful positive compilation, then a failure in the negative fixture with the expected diagnostic. Add relevant framework sources, generated declarations and build configuration through repeated `--input` arguments. Compiler rejection proves that compiler contract, not runtime behavior.

Runtime claims require demanded values, executed Tasks or decisions, and the promised observable result. Select independent expectations and meaningful properties. A copied implementation, assertion on a fixture alone, setup failure or arbitrary compiler exit is insufficient evidence.

The helper retains argument arrays, selected source snapshots, hashes, logs and observed outcomes under `.agents-state/regression-evidence/`. It rejects changes to the selected inputs during execution. This binds the captured run to those inputs. It does not establish complete dependency coverage or authenticate a hand-edited receipt. State the tested revision, dirty state, inputs and limitations in the result.
