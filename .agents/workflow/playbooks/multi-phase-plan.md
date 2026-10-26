### Multi-phase or multi-PR plan

**You own the plan, not the code. The plan is a checklist an owner runs box by box and the operator audits from the evidence.** The plan is the deliverable. Do not implement.

1. When the change is low-risk and has an obvious approach with no meaningful sequencing or design decision, skip the plan. File count alone does not establish that for a public API or generator change. Say so and stop.
2. Settle open questions by prototype before you write. Run `.agents/workflow/playbooks/prototype.md` for each. Keep the branch, the SHA, formal test results and the relevant screenshots, compiler output or runtime evidence for Appendix A. Ask the operator only about a product or preference call that no run can settle. Give options (the **never-block-on-the-human** principle skill).
3. Explore in subagents through the available roles and delegation facilities in `AGENTS.md` (the **guard-the-context-window** principle skill). Batch within capacity without dropping required coverage. Each returns file pointers, conventions, test commands, and entry points. No inlined dumps.
4. Copy the skeleton below into the plan file and fill every placeholder. Unless the operator names a path, write the file under repository-root `.agents-state/plans/`. Keep every heading and every sub-block in the order shown. One section per PR. One PR is one coherent change with its own evidence, including the Haskell types, instances, generators and consumers required for that contract (the **sequence-verifiable-units** principle skill). Name the execution playbook in **How to read this**. Pick between `.agents/workflow/playbooks/autopilot-full.md` and `.agents/workflow/playbooks/autopilot-stack.md` per the rule at the end of `.agents/workflow/playbooks/autopilot-stack.md`. A standing program takes `.agents/workflow/playbooks/orchestrate.md`.
5. Write under `/technical-writing` in full, then the **prose-cleanup** skill. The body is one Diátaxis mode, how-to. Appendices hold explanation and reference. Each heading states the task or the finding. No long dashes. No mid-sentence colons.
6. Run `node .agents/skills/multi-phase-plan/scripts/check-plan.mjs <plan.md>` using the actual repository workflow path from `AGENTS.md` and fix every line it prints (the **encode-lessons-in-structure** principle skill).
7. Hand back. Post the plan path and the script's output, then stop. Execution starts on the operator's explicit go, under the execution playbook the plan names.

**Verification.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked (the **prove-it-works** principle skill). That sentence is the verification rule. Every verification block opens with it. The live block is mandatory. Ten lanes at the PR head drive the actual affected boundary, per **swarm** and `AGENTS.md`, concurrently or in batches within available capacity. Every required lane needs a result with its tested SHAs and method; a blocked or missing lane is not a pass. Each lane is one box with a concrete scenario, the evidence artifact it saves, and its pass predicate. One lane is the **Regression lane against trunk.** It runs the same load-bearing scenario on trunk and head. If trunk does not have the feature, the lane records that fact and gates the behavior the diff adds plus the end state the user waits for instead of inventing a trunk result. The perf gate is dual-sided. Trunk and head must both produce the named metric. If trunk lacks the feature, also isolate the work the diff adds and set an absolute budget for that work plus the end-to-end state the user waits for. Do not claim a ratio between unlike scenarios. The perf block names the metric, the interleaved probe, the trunk baseline measured first, and the rule with the number that fails. A PR that changes an interaction is review-gated. The operator reviews it in chat before merge, with screenshots and a video for visual interactions, or concrete API examples, compiler diagnostics and execution evidence for nonvisual interactions. A PR that changes no interaction writes `**Review gate.** None. <PR id> is not review-gated.` and no boxes under it.

**Control skill.** Pick it by surface. Browser and web UIs use `control-ui`; CLIs and TUIs use `control-cli`. For framework/library/compiler work, lanes use representative compiler consumers, demanded pure results and executed Task/decision or command/storage/query/service flows. Use meaningful Hspec examples and QuickCheck properties with independently specified expectations; avoid tautological tests. A PR touching multiple boundaries gets lanes on each. Name the actual runner and evidence even where no control skill applies. Performance probes follow `benchmark-checklist`, covering affected framework flows and consumer compilation costs when relevant. A missing usable driver is a risk in Appendix C, not a reason to claim a pass.

````markdown
# <Program> plan

<Under ten lines. What changes, for whom, the rule the program enforces, and the PR ids in order.>

## How to read this

One box is one unit of work. Every box names the evidence that checks it. A nested box is a sub-step of the box above it. Check a box only when its evidence exists, a file, a log line, a screenshot, a test run, or a SHA. The body is a how-to. The appendices explain and record.

The program runs `.agents/workflow/playbooks/<execution playbook>.md`. <Who merges, and which PR ids are the operator's items that stop at merge-ready.>

Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

## Program checklist

### Arm the program

- [ ] State the protocol and this plan to the operator, then stop. Start execution only on the operator's explicit go.
- [ ] Trunk. Discover and record the actual trunk ref and workflow/skill paths from `AGENTS.md`.
- [ ] Read these from trunk at program start. Re-read them at every tick.
  - [ ] `git show <trunk-ref>:.agents/workflow/playbooks/<execution playbook>.md`
  - [ ] `git show <trunk-ref>:.agents/skills/swarm/SKILL.md`
  - [ ] `git show <trunk-ref>:<relevant verification skill path>`
  - [ ] `git show <trunk-ref>:.agents/workflow/playbooks/opening-a-pr.md`
  - [ ] `git show <trunk-ref>:<each other leaf skill path>`
- [ ] Audit cadence. On the operator's go, use a supported supervision mechanism from `AGENTS.md` with a one-hour audit cadence and the tick prompt below. Record the actual mechanism and its lifetime; if persistence is unavailable, report that and supervise while the session is active. Never promise monitoring the host cannot supply.
- [ ] Use this tick prompt, verbatim. "Re-read the execution playbook from trunk. Audit the operation against it and fix drift in this tick. Probe every active lane and judge progress by side effects only. Stand down a stuck lane and dispatch its replacement now. Then post a short status message to the operator in chat only when the audit found a tracked change that no earlier status message reported, such as a PR opened, a code-ready head, a round launched or closed, a verdict, a merge, a stuck agent and the action taken, a blocker added or cleared, or a decision only the operator can make. Name every such change and nothing else. Do not repeat a table, the merged list, or an unchanged blocker. If the audit found none, end the turn with no reply text. Either way, log this tick's row in your decision trail. The row names the items reported, or none."
- [ ] On the operator's hold or stand-down, send every owner a zero-writes order at once.

### Spawn owners

- [ ] Spawn one owner per PR with the full lifecycle the execution playbook names.
- [ ] Follow this dependency graph. Start dependent work only after its parent merges, or base it on the parent branch when the execution playbook stacks.
  - [ ] <PR id> and <PR id> are independent and first. Both branch from the recorded trunk ref.
  - [ ] <PR id> after <PR id>.
- [ ] Hold the file boundaries. <PR id or class> touches only `<glob>`.
- [ ] Hold the review gate. <PR ids> change an interaction. They wait for the operator's review in chat with the boundary-appropriate evidence before merge.

### PR mechanics, for every PR

- [ ] Confirm the repository and use `gh` consistently for PR operations.
- [ ] Open the PR ready, never draft, per **Opening a PR**, using `gh pr create --base <base-branch>`. A stack child targets its parent branch.
- [ ] Run the repo's lint and typecheck once before the PR-facing push. Push with hooks on.
- [ ] Run `/deslop` before each commit and `/no-comments` before review.
- [ ] Triage every review comment per `../../skills/babysit/references/review-triage.md`.
- [ ] Preserve the declared trunk or stack-parent base through fix rounds. Route necessary topology changes to the branch owner under `.agents/workflow/playbooks/shipping.md`; revalidate changed head/base and affected inputs before declaring readiness.

### Verdict and merge, for every PR

- [ ] A full swarm runs once, at the code-ready head. Another full swarm runs only for new behavior, a redesign, or a large new diff. A changed head/base or relevant build/runtime input gets independent reassessment under `.agents/workflow/playbooks/shipping.md`; rerun affected lanes and carry forward only demonstrably unaffected evidence. Run each full swarm per `.agents/skills/swarm/SKILL.md`. One gates lane. The ten live lanes from the PR's **Verify, live** block. The perf lane from its **Verify, perf** block. Two or more audit lanes, each with its own focus, that read the diff and the receipts and distrust the PR body. The root audits the receipts in the merge-ready report before the verdict.
- [ ] Clean only when every lane is `PASS`. Findings go back to the owner, including a defect that a lane filed as a note. Bind the verdict to the tested head/base and relevant inputs; follow the evidence-reuse rule in `.agents/workflow/playbooks/shipping.md`.
- [ ] <The authorized merge or append rule from the execution playbook, following current-input verification and owner-controlled landing in `.agents/workflow/playbooks/shipping.md`.>

### Boot recipe, for every live lane

Each live lane uses an isolated checkout and fixtures/state at the declared PR head and baseline, through the available execution facilities. Drive the actual compiler, framework, CLI or UI boundary; share no mutable fixture or process state between lanes. Batch all ten lanes within capacity.

- [ ] <Provide an isolated checkout at the exact head/base SHAs and verify both revisions.>
- [ ] <Prepare the actual compiler/consumer or start required services and the surface. Wait for the named readiness predicate.>
- [ ] <Deliver input through the actual boundary/runner or matching control skill. Name the read-only diagnostics and cleanup for owned resources.>
- [ ] Save lane evidence under `.agents-state/<program>/swarm-<pr-id>/worker-<n>/` and return its paths with tested SHAs, method and results. Use screenshots for visual evidence and compiler output, assertions or traces for nonvisual evidence.

## <Task as a verb phrase> (<PR id>)

**Depends on.** <PR id, or None.>

**Files.**

- [ ] Edit `<path>`.
- [ ] Create `<path>`.
- [ ] Delete `<path>`.

**Build.**

- [ ] <One change. Name the symbol and the file.>

**You see.**

- [ ] <One observable result, with the exact log line or screen state.>

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] <Hspec/property/compiler-consumer test file and the case it gains, with independent expectations.> Run `<command>`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes at the PR head, per the boot recipe and `AGENTS.md`.

- [ ] Lane 1. Regression lane against trunk. Run <the same load-bearing scenario> at trunk and head. If trunk lacks the feature, record that and gate <the behavior the diff adds plus the end state the user waits for>. Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 2. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 3. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 4. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 5. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 6. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 7. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 8. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 9. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.
- [ ] Lane 10. <Scenario.> Save `<lane-evidence-path>`. Pass when <predicate>.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. <What is measured at both trunk and head. If trunk lacks the feature, also name the diff-added work and the end-to-end state the user waits for.>
- [ ] Probe. <The command or procedure, run at trunk and at the head, interleaved. Both sides must produce the metric.>
- [ ] Baseline. Record the trunk <value> first.
- [ ] Rule. <Head against trunk, with the number that fails. If the scenarios differ, add absolute budgets for the diff-added work and the user-visible end state instead of an invalid ratio.>

**Review gate.** <Visual. or Nonvisual.> The operator reviews before merge. Choose Visual when any visual interaction changes; use None only under the rule above.

- [ ] Present the operator with evidence from lane <n> at `<review-evidence-path>`. For nonvisual interactions, include API examples or diagnostics plus the relevant executed checks.
- [ ] For a Visual gate, copy screenshots and record a 30 to 60 second video of the change, saving both under `.agents-state/<program>/review/`. Omit this box for a Nonvisual gate.
- [ ] Post the review evidence in chat. Stop at merge-ready. Wait for the operator's approval.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Review triage done.
- [ ] Current head/base, required checks/reviews and verified inputs still satisfy `.agents/workflow/playbooks/shipping.md`; any owner-controlled rebase has been independently revalidated.
- [ ] <The owner squash-merges its own PR, or the root appends it to the base-branch stack and the operator lands it bottom-up.>

## Close the program

- [ ] Every box above is checked with its evidence.
- [ ] Reply to the operator with the report the execution playbook names.

## Appendix A. Prototype evidence

<Each open question a prototype answered, with the branch, the SHA, formal test commands/results and the artifact links. Each question that stays unproven.>

## Appendix B. Alternatives rejected

<Each approach weighed and why it lost.>

## Appendix C. Risks

<Each risk with the PR it lands in and what the owner watches.>

## Appendix D. Links and reading list

<Docs to read before editing. Which PRs get `.agents/skills/how/SKILL.md` and `.agents/skills/interrogate/SKILL.md`. The trail per `.agents/skills/show-me-your-work/SKILL.md`.>
````

**Reply:** the plan path, the PR ids with their dependencies and the review-gated set, what the prototypes proved and what stays unproven, and the check script's output. The checker validates structure, not whether the receipts prove their claims.
