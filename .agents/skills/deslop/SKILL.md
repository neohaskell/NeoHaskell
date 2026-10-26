---
name: deslop
description: Remove AI-generated code slop and clean up code style
---

# Remove AI code slop

Check the diff against main and remove AI-generated slop introduced in the branch.

Before accepting AI-generated Haskell, check it against the [NeoHaskell code guidelines](../neohaskell-code-guidelines/SKILL.md) and the applicable AGENTS.md instructions. Revise the initial output to follow NeoHaskell imports, control flow, error handling, and API conventions.

## Focus Areas

- Extra comments that are unnecessary or inconsistent with local style
- Defensive checks or exception recovery that are unnecessary given established typed contracts. Preserve boundary validation, intentional error handling, cleanup, and cancellation behavior.
- Unsafe coercions, partial extraction, or panic placeholders introduced only to bypass type errors or avoid handling a valid case
- Deeply nested cases or do-blocks that become clearer with exhaustive pattern matching, named helpers, or existing Result, Maybe, and Task combinators
- Other patterns inconsistent with the file and surrounding codebase

## Guardrails

- Keep behavior unchanged unless fixing a clear bug.
- Prefer minimal, focused edits over broad rewrites.
- Keep the final summary concise (1-3 sentences).
