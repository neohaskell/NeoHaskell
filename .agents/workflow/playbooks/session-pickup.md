### Session pickup

**You own the resume point. Read the prior trail, don't redo it.**

1. Locate the prior trail through the current environment's available conversation-history interface, explicit session links or resume notes, the task's decision trail, or a pushed branch. Stay within the authorized task/workspace; do not search unrelated conversations. Read the metadata overview and last messages first, then scan back for the decision points. Parse a long transcript in a subagent through `AGENTS.md` delegation and keep the reduced timeline in the main thread (the **principle-guard-the-context-window** skill). State any history-access gaps and keep derived resume notes under repository-root `.agents-state/`.
2. Reconstruct operational state. The branch and worktree, what already landed (`git log`, `git diff` against the base), the open todos, the decisions made. Preserve accepted intent and decisions rather than re-deriving them; distinguish those decisions from reported outcomes that need supporting evidence.
3. Diff done vs pending. Compare what shipped against what was planned and name the resume point. Reuse completed work and valid evidence rather than restarting the investigation or rerunning the prior repro by default.
4. Route the remaining work to the matching playbook and pick the verdict: continue the execution, ship a finished recommendation, ratify or override a prior conclusion, or postmortem a failed run.
5. Verify the inherited claims against the original goal on the real artifact (the **principle-prove-it-works** skill). A passing prior self-report is not the proof. Check existing receipts against the current artifact, head/base and relevant inputs; rerun only checks whose evidence is missing, stale or contradicted. A changed generator may need its consumer fixture again, not the whole investigation. Complete this validation before handing off. The pickup playbook ends here. The routed playbook owns the rest.

**Reply:** where the prior agent stopped, what you inherited vs redid (ideally nothing redone), the resume point, and the outcome.
