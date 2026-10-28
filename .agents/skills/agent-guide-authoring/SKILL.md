---
name: agent-guide-authoring
description: Write or review the published NeoHaskell agent guide for generated applications. Use for docs/agent changes and consumer-facing framework changes that may make the guide stale.
---

# Agent guide authoring

Write `docs/agent/` for an agent working inside a generated application. The agent may have the generated project and the pinned guide, but no NeoHaskell framework checkout.

1. Read the affected guide page as that agent. State the API, required declarations and order, input and output behavior, failure conditions, and project commands needed for the task. Replace a framework source or test pointer with the useful fact it was meant to prove. Link to another guide page when that page owns the explanation.
2. Keep published pages and the shipped manifest free of framework repository paths. Application paths such as `src/App.hs`, `tests/Spec.hs`, `neo.json`, and `.neo/agent-guidance/active.md` are useful when the generated project contains them. Link between guide pages using relative links.
3. Verify each changed claim against maintained implementation and consumer behavior in the local checkout. Keep source paths, Hurl scenarios, and spec names in the private policy owned by [NeoHaskell consumer verification](../neohaskell-consumer-verification/SKILL.md#source-owned-agent-guidance). Keep review receipts in that skill's ignored worktree-local state. Neither belongs in the published guide.
4. Run the local corpus check from that skill. Inspect every changed page and the shipped manifest for repository paths. Report the changed page and local result in the PR verification section.

The review is complete when an agent can follow the changed topic using only its generated project and pinned guide, and the local corpus check passes. No agent executable or live provider login is part of this review.
