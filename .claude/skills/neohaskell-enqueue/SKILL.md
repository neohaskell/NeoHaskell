---
name: neohaskell-enqueue
description: Use when Nick asks to enqueue work into the change process — "encola GH issue 123", "enqueue issue #123", "mete esta tarea en la cola", "queue this feature/bug". Creates the REQUEST bead that the dispatcher daemon claims and pours as a change-formula-v2 molecule (ADR-0075). This skill only enqueues — it never starts executing the work, never pours the formula by hand, and never claims the bead; the dispatcher owns everything after bd create.
---

# Enqueue a request into the change process

The contract: Nick names a request → ONE request bead exists → the
dispatcher daemon does the rest (claims it, pours the `change` molecule,
runs spec → gate → build → verify → pr per ADR-0075). Your entire job is
the bead.

## Steps

1. **Fetch the source.** For a GitHub issue:

   ```bash
   gh issue view <N> --json title,body,url,labels
   ```

   For an ad-hoc request (no issue), use Nick's words as the title and
   note `adhoc` in the description.

2. **Create the request bead** — title prefixed with the source, type
   mapped from the issue (bug → `bug`, feature request → `feature`,
   otherwise `task`), description = one-paragraph restatement + the link:

   ```bash
   bd create --title="GH #<N>: <issue title>" \
     --description="<one-paragraph restatement of what is being asked>. Source: <issue url>" \
     --external-ref="gh-<N>" --type=<bug|feature|task> -p 2
   ```

3. **Confirm the handoff.** Tell Nick the bead id and that the dispatcher
   will pour and run it. Check the daemon is alive
   (`bun src/dispatcherctl.ts status` in `~/repos/dispatcher` shows a
   recent `lastTick`); if it is not running, say so explicitly — an
   enqueued bead with no daemon sits forever, silently.

## Hard rules

- Never begin the work yourself: no spec writing, no code, no
  `bd mol pour`, no `--claim`. Enqueue-only.
- One bead per request. Never pre-split into steps — the formula does
  that (over-atomization is the failure mode ADR-0075 exists to prevent).
- If the request is ambiguous enough that the TITLE would be wrong, ask
  Nick ONE question; otherwise enqueue and let the spec step's intake
  handle contract-level clarification.
