### Pause safely

**You own a clean stop. Leave a checkpoint a cold-start agent can resume from.** This is explicit only. On "keep going", "going to bed, keep going", or "don't stop", do not pause.

1. Stop at a safe boundary. Finish the current atomic step or back out of it. Start nothing new, and stop or cancel nested subagents through the available facilities in `AGENTS.md`. Confirm that nested writers have stopped writing before checkpointing.
2. Take no irreversible action to pause. No PR and no push unless you already had one out.
3. Make this task's work durable. Commit only task-owned uncommitted edits as one clear `wip:` commit on the current branch, preserving pre-existing user edits and staging. Record any task changes left outside the checkpoint and any remaining dirty state. If the tree is broken, say so in the commit body in one line.
4. Write the durable resume note at repository-root `.agents-state/<task>/resume.md`. Capture intent, what you were doing, the last completed unit, progress and what's verified, current branch/head/base and nested writer state, next steps, key files, and gotchas. Name pending, failed or unrun checks; distinguish written code from completed compiler or runtime proof. If a **show-me-your-work** trail exists, point at it and the existing receipts instead of duplicating them.

**Reply:** where you are in the loop, what's on disk versus still in your head (paths, no diff dumps), the commits you made and whether the tree is clean, and the first action on resume. This is a pause, not a final report.
