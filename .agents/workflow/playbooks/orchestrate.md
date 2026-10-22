### Orchestrate

**You own the program, never the code. Author briefs, drain the queue, keep the frontier green, decide.** For a whole project handed to one standing coordinator chat: multi-day, many stacked PRs, dozens to hundreds of subagents, the human checking in twice a day instead of every five minutes. One task driven to a predicate is Autonomous run. One ambitious run needing a bespoke workflow is figure-it-out. Route here when the work outlives any single agent. Work one agent could finish inside the session's budget is not a program.

Ceremony must scale with the program. On cheap near-identical units, collapse it as each section directs. Record the existing grant and who owns landing; continuous landing runs only within it. A program handed over for review stops at the agreed review boundary. Use `.agents/workflow/playbooks/shipping.md` and `.agents/workflow/playbooks/babysit.md` for independent PR proof, current checks and actual merge confirmation; bookkeeping never grants merge authority.

Three rules carry the rest.

- Completions are queue events, not interrupts.
- Every spawn and every resume carries the standing orders verbatim.
- The brief is the product. A vague brief fails quietly, because a worker cannot ask you a question.

#### Roles and placement

- **Coordinator (this chat).** Runs where its durable state and required tools are available. Frames, authors briefs, drains the inbox, owns the human report, makes judgment calls. It never authors or edits code. Conflicted merges, chain rebases, and code changes are always tasks. Mechanically landing a verified unit (fast-forward or clean cherry-pick of a worker's commit, then push) is bookkeeping the coordinator may do itself on repos where local git is cheap. Queueing finished work behind an idle stacker is how a deadline harvests nothing. The loop is agentic end to end. Use the available roles, delegation and supervision facilities from `AGENTS.md`, within actual host capacity. Agent messages and state bookkeeping are separate interfaces. State reads and writes go through `.agents/skills/orchestrate/scripts/orch.ts` at drain points, one command in and one line out. The CLI never spawns, waits, or wakes anything.
- **Sub-coordinator.** Durable, with access to its track state, one per track, and only when the program exceeds what one coordinator's drains can manage. A track the coordinator can drain itself needs no middle layer. Each nested layer re-pays a full orientation preamble, and a blocking sub-coordinator hides its children while the parent idles. Owns its track's units and boards, authors its workers' briefs, spawns its own workers and verifiers when the host supports nesting. Otherwise the coordinator keeps the track assignments and drains their workers directly, without dropping coverage. Rolls up aggregates at wave boundaries. Never forwards raw child reports. Cap in-flight children at what one drain can process, roughly ten within actual capacity, as a rolling window. Never as blocking batches, which cost the slowest child of every batch.
- **Worker / verifier.** Place each where the source, credentials, drivers and evidence it needs are actually accessible. UI/CLI runtime verification may need this machine's control surface; conversation evidence may need the active environment's history interface. Inline required context or use repository paths for workers without store access. Prefer fewer, broader workers. One writer per worktree or branch (principle-separate-before-serializing-shared-state). Required independent verification comes from a verifier who did not write the code, using the roles in `AGENTS.md`.

Depth stays at coordinator, track, worker. Author the track decomposition per project (build, landing, and verification are common cuts, not a required shape). Hard-coded swarm trees were tried and parked as too rigid.

#### Store layout

Create repository-root `.agents-state/orchestrate/<project-slug>/`. Every file has exactly one writer. Owners publish facts, readers aggregate at read time. Use `node .agents/skills/orchestrate/scripts/orch.ts --store <absolute-store-path>` for bookkeeping, written below as `orch`, while its canonical plain TSV and JSON stay readable without the CLI. Resolve the workflow path from `AGENTS.md`. The helper needs Node 24.2+ and its declared Commander dependency; arrange explicit dependency setup when needed. Startup and help do not install anything. Keep bookkeeping writes on one coordinator host; the PID lock is not a distributed ownership mechanism. See [orchestration state](../../skills/orchestrate/references/orchestration-state.md) for commands, evidence and batch receipts.

- `preferences.md` is the standing-orders register: numbered lines, one constraint each (roles and required capabilities, stack shape and count, verification bar, forbidden paths, escalation policy). Paste it verbatim into every spawn and every resume. Directives decay across resumes, and each dropped one costs a human turn. When you catch yourself restating an instruction, append the line before you act (principle-encode-lessons-in-structure).
- `overview.md` is the durable PR and issue DB. Append. Never rewrite wholesale per event.
- `units.tsv` has one row per unit: id, track, state, branch, PR, head SHA, brief path. Update rows in place.
- `frontier.json` is the computed merge frontier, per Stack safety.
- `ledger.tsv` is the verification ledger, per Verification.
- `inbox/` holds new completion pointers; `batches/` retains unacknowledged drains and `accounted/` retains saved accounting receipts. `gates.md` parks human gates (question, options, default within the existing grant). An unanswered permission gate remains pending.
- `decisions.tsv` is the trail via the show-me-your-work skill.
- `status.md` is derived from `units.tsv` and `ledger.tsv` at each drain, never hand-maintained. Regenerate it from the tables instead of narrating events into it.

#### The brief

Your prompts to agents are your only product, and a sloppy brief compounds into slop across the whole tree. Every spawn carries all of it. A field you cannot fill is a unit you have not scoped yet.

```
GOAL         one sentence, the outcome, executable by a stranger with no chat access
SCOPE        paths this unit may write; paths it may not; its exclusive worktree or branch
CONTEXT      pointers to files and PRs; upstream reports pasted in full when this unit
             depends on them, because workers cannot see siblings
ACCEPTANCE   checkable criteria, one per line
VERIFY       exact commands or the control-skill path, plus known gotchas
TIMEBOX      rough cap on runtime; on expiry, return partial findings and stop rather than run on
FORBIDDEN    no PR retargets or merges, no rebase, no force-push, no fixes outside scope,
             plus unit-specific bans
REPORT       status, branch, head SHA, PRs, verdict, what you actually ran, deviations,
             suggested follow-ups
STANDING     <preferences.md pasted verbatim>
```

Size the brief to the unit. A one-command unit gets the template collapsed to a paragraph that still names goal, scope, the verify command, and the report shape. A 4KB scaffold around a two-line edit costs more to write and obey than the edit. Spawns with store access may reference the standing-orders file by path. Paste it verbatim for workers without that access and on every resume.

A sub-coordinator brief adds its track boundary and unit list, its spawn budget with actual capacity and required access for each role, the drain protocol, and the rollup format (per child: name, status, PR, head SHA, verdict, one line, plus track status and frontier delta).

A dependency is a context relay, not just ordering. Undeclared upstream context makes the worker guess. Missing fields are a refuse-to-spawn condition. Audit one sampled worker brief per sub-coordinator per wave, concurrently with the wave it samples, never as a gate in front of it. A failing brief stops that track and fixes the sub-coordinator's instructions, not just the worker, because brief quality decays late in a run. Never resume-chain a brief. Respawn fresh with consolidated scope.

#### Steps

1. **Frame.** State the done predicate as something countable ("all 126 units merged, each with a ledger verdict proving its declared contract"). Quantify scope: units, rough effort, expected stacks, and the wall-clock budget. If one agent could finish inside that budget, stop here and run Autonomous run instead. Collapsing must not depend on another document being present. It means do the work directly in this session, plain workers where they help, verification inline, landing as you go, and none of the store, register, or pilot machinery below. Schedule landing against the budget. By roughly 70% of it, stop spawning and land what is verified. Name the tracks per project. A contested decomposition or one-way door goes through the arena skill before the pilot. Present the framing once. Reversible prep proceeds without waiting.
2. **Initialize the state.** Confirm the documented runtime and dependencies, then run `orch init`. Open the trail via the show-me-your-work skill, write the standing orders before any spawn, and seed `frontier.json` from existing PRs with `orch frontier set --repo <repo-dir>`.
3. **Pilot.** Push one unit through the whole path: brief, worker, verification, stack entry, ledger row, merge. The pilot exists to falsify the brief template, the verify recipe, and the unit size while that costs one agent instead of fifty. Fix the contract from pilot evidence before any fan-out. Scale the pilot to the unit. On programs of near-identical cheap units, the first unit is the pilot, run as a normal unit with its verify command inline, and fan-out starts the moment it lands. The dedicated per-unit pilot pipeline (separate verifier agent, audit gate) is for expensive or novel unit shapes, not for clone-units where a serialized pilot has nothing to falsify. Independent PR proof under Shipping still applies before landing.
4. **Scale.** Spawn a rolling window of workers up to the in-flight cap, refilling as children finish. Blocking batches pay the slowest child of every batch. Spawn track sub-coordinators only past the one-drain threshold in Roles. Recompute ready work after each drain. Relay upstream reports into downstream briefs. Keep sibling communication upward only. The sampled brief audit runs alongside the wave it samples and stops the next refill on failure, not the current one.
5. **Drain.** Run the queue discipline below at every drain point.
6. **Land.** Landing is continuous, never a terminal phase. Integration starts with the first verified unit and runs alongside the remaining waves. On heavy repos the stacker is a standing role from wave one, integrating as units verify. On repos where local git is cheap, the coordinator lands verified units itself per Roles. Keep the frontier green before upper-stack work. Stack safety governs. Advance `frontier.json` only on merge or reported new head SHAs.
7. **Close.** Drain the final inbox, reconcile every spawned agent to a terminal row (done, abandoned, zombie-reconciled), confirm the predicate on the real artifact, confirm every landed PR has independent proof for its landed candidate and relevant inputs plus actual merge readback, audit the trail per show-me-your-work including its independent review, encode recurring corrections into `preferences.md` or the brief template. Leave the store intact. It is the postmortem.

#### Queue and drain

- On a completion notification, run `orch inbox push <agent> <unit> <status> [--report PATH]` and return to what you were doing. Never deep-review inline. A completion that needs review becomes a verifier unit. Never review a diff inside a drain.
- Drain in batches at four points: the end of a critical section, a track rollup, a frontier watcher wake (use available host scheduling/waiting from `AGENTS.md`, within actual session limits and with monitoring gaps reported), and before a human report. Begin each batch with `orch inbox drain`. Keep its batch id and pointer ids; an interrupted drain returns that same batch. Arrivals during a drain wait for the next one. After saving all resulting unit/ledger rows, write the complete accounting receipt and run `orch inbox ack <batch> --receipt <path>`. Never acknowledge unsaved results; a retry reuses unit ids and evidence keys rather than counting the same completion twice.
- Critical sections you finish first: authoring a brief, a stack operation, a conflict decision, writing a gate, updating ledger or frontier.
- Each drain classifies every pointer (landed, needs-verify, failed, zombie, noise), writes the resulting rows through `orch unit add`, `orch unit set`, and `orch ledger record`, runs `orch status`, then spawns the next wave in one message.
- Account for every spawned child at its track's rollup: arrived, respawned, or its scope explicitly absorbed. Silently redoing a missing child's work hides both the wasted spend and the coverage gap its result existed to close.
- A drain turn ends with the three lines from `orch status`: counts against the states, what changed, gates open. Detail lives in `status.md`. The full reply contract applies at checkpoints and close.

#### Stack safety

- The frontier is a computed object, never narrative. Recompute `frontier.json` after every merge and stack mutation: ordered PR list, branch names, head SHAs, a generation number, the lowest unmerged PR. The command walks the PR base chain through the checkout's current branch, using GitHub CLI for the repository/default branch and actual published head/base SHAs and states. A closed, unmerged prerequisite remains a blocker, including when reached from below through its live descendants. Missing, ambiguous, inconsistent or unknown observations fail without advancing the saved generation. A branch with no PR errors rather than guessing.
- Exactly one stacker per stack may mutate topology (chain rebases, force-pushes, base retargets, merges), serialized within its stack. Record the holder in the standing orders. Place chain rebases where the required clone and resources are available. Transfer exclusive write ownership before a stacker rewrite and return the resulting head/base/lease before the owner resumes, per `.agents/workflow/playbooks/autopilot-stack.md`.
- Workers never rebase and never retarget or merge PRs. Babysitters follow `.agents/workflow/playbooks/babysit.md`, one per stack, scoped to one immutable frontier generation. They report conflicts to the stacker rather than rebasing.
- PR closes and retargets go through the stacker only. Closing a base PR orphans every chain above it. Merges and stack surgery are units with briefs like any other.
- One retro watcher follows merged PRs for reverts, post-merge CI breaks, and orphaned follow-ups.

#### Verification

Scale verification to the unit. When VERIFY is a single cheap command, the worker runs it and reports the output, and the coordinator spot-checks receipts. A dedicated per-unit verifier is for expensive, judgment-laden or high-impact verification. Independent PR proof before landing is still required by Shipping; a worker's self-check is not that proof.

Match proof to the claim: representative consumer compilation for public APIs and Template Haskell generation; demanded pure results and executed Tasks/decisions for runtime claims; real command, storage, query/projection or other service interactions when affected. Use meaningful Hspec examples and QuickCheck properties with independent expectations. Compiler evidence can prove a compiler contract. Performance claims use **benchmark-checklist**, including relevant consumer compilation costs.

Write ledger rows with `orch ledger record`; check independent proof against the current GitHub candidate with `orch ledger check`. Supply repository identity, tested head/base, a digest of the relevant-input manifest, claim kind, method, receipt, author and verifier. The manifest records relevant compiler, dependencies, build flags, generators, fixtures and runtime inputs per Shipping; the caller must establish its freshness. The helper compares GitHub identity/head/base and the supplied digest; it does not discover all semantic inputs or authenticate agent identities. Rows preserve provenance; a worker self-report cannot replace independent proof. Labels are `artifact-verified | behavior-verified | compiler-verified | verifier-blocked | verifier-failed`, matched to their declared claim. Blocked/failed is not a pass; neither CI green nor a recorded label alone proves the claim. Respawn when the environment heals; failures get a fix unit before re-verification.

After a changed head/base or relevant input, apply Shipping's affected-check and justified carry-forward rules before recording a new candidate verdict. `ledger.tsv` is reviewable evidence, not merge authority. Landing also rechecks Babysit's reviews, checks, threads and branch rules, then Shipping's head pinning, queue rules, actual merge readback and post-squash child transition.
A unit is not done until its output is externalized the moment it lands, never batched to the end of the run. A worker pushes its branch, a verifier writes its ledger row, receipts land in the store. Work that exists only on one VM when that VM dies was never done.

#### Liveness and failure

- Never resume an agent to check on it. A resume restarts an idle agent. Probe read-only: the ledger, `units.tsv`, `gh`, pushed branches, the active host's agent-status interface. Transcript mtime is not liveness.
- A silent death gets a synthetic postmortem row in the inbox (unit, failure mode, last evidence, options). Replan on evidence as it arrives. Do not wait for unrelated work to quiesce; stop or fence the old writer before transferring its scope to a replacement.
- Retry by mode: cap-hit or oom, respawn with smaller scope. Network-drop, retry as-is. Tool-error, diagnose the failed tool or execution facility and retry using an available compatible role. Unknown, retry once. Two retries, then abandon the unit and replan around it.
- A zombie that returns hours late reconciles against the current frontier and ledger before anything is accepted. Salvage unique findings through a fresh unit, never a blind merge.
- When continued spawning would produce garbage tree-wide (bad upstream output, broken acceptance, dead infra), write a stop line at the top of the standing orders, let in-flight work finish, fix the cause, clear it.
- Bound your own infra retries the same way you bound a child's. After a few consecutive tool aborts, stop retrying. Write a terminal handoff to durable state (what is done, where it lives, the exact command to resume) and end the run.
- After a host/session restart, determine which agents and work actually survived; do not infer liveness from their former placement. Re-read the standing orders and `units.tsv`, recompute the frontier, reattach surviving work by recorded PR, branch, head and ownership, checking the current host status, respawn one sub-coordinator per track from its stored brief plus current state, drain, resume. The dead session's store lock clears itself on the next write. `orch` replaces a lock whose holder pid is gone.

#### Escalation

Reaches the human, batched into the status page rather than per item: irreversible actions (force-push to shared branches, deploys, deletions, closing someone else's PR), genuine product or preference calls no experiment settles, a standing order that contradicts observed reality, a program-level dead end that survived a replan. Park each as a `gates.md` entry before asking, and route work around it.

Never reaches the human: frontier nudges, rebase mechanics, retries, CI flake triage, review-thread triage, format fixes, scope the brief already forbids (refuse and continue), and "should I keep going". When in doubt, act and log.

Mid-run discoveries fix only what blocks the frontier. Everything else parks in follow-ups. At this fan-out a small scope leak multiplies into PRs nobody asked for.

**Reply:** at checkpoints and close: the predicate and the count against it from `units.tsv` and `ledger.tsv`, tracks and what each landed, the frontier (PR list plus SHAs), verdicts summary, what was abandoned and why, gates awaiting the human (the only asks), the store path, and the trail path. Numbers from the tables, not narrative. Include PR links.
