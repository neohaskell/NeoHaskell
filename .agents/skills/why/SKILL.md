---
name: why
description: "Use for 'why does X work this way', 'why we picked Y', design rationale, regressions, postmortems, or data-backed thresholds. Discovers available evidence sources and queries each evidence category (source control, issue tracker, long-form docs, real-time chat, infrastructure observability, error tracking, product analytics warehouse) in parallel, then returns a cited read on decisions and tradeoffs. Use how for runtime behavior."
---

# Why

Investigate the motivation and intent behind code. The companion `how` skill answers what the code does and how it works. `why` answers what forces led to its shape.

Follow the delegation instructions in `AGENTS.md`. Use the current environment's configured investigation and synthesis roles and its available delegation interface. Inherit supported runtime defaults when no role override is configured; report unsupported overrides rather than inventing fallback identifiers. Select only reasoning and speed settings supported by the environment. Centralized role configuration is managed separately.

Investigators and the synthesizer must not edit files or modify external state. Use read-only enforcement compatible with the required evidence access when available; otherwise state the no-write constraint in their tasks. Do not assume a particular runtime mode controls access to connected evidence sources.

## Operating Posture

Operate as a **careful, cautious, and precise investigator**. Be honest about what you know vs what you're inferring. Read `references/epistemics.md` for the full confidence framework and phrasing guide. The synthesizer must follow it.

## Step 1. Understand the Target and the Question

The **target** is usually a chunk of code, a pattern, a feature, or a named design decision. The **question** is usually a design rationale, a tradeoff, a motivating edge case, an external constraint, dead code, or a broad history sweep.

If the target is vague ("why do we do it this way?" with no clear referent), make your best guess from conversation context (open files, recent edits, cursor location, what was just discussed). State your interpretation briefly so the user can redirect if you're off, then proceed.

## Step 2. Establish the Code Anchor

For this repository, investigate the internal NeoHaskell framework, language, and toolchain. Before spawning investigators, build the seed context inline: file paths and line ranges in the maintained implementation, key Haskell symbols (types, functions, service records and their operations, typeclasses or instances where relevant), the last few commits touching the target, PR numbers from merge commits (pattern `(#1234)` in the subject line), and any linked ticket IDs. Run `git blame -L <start>,<end> <file>` for last-touch commits, `git log --follow -p -- <file>` for full history with patches through renames, `git log --oneline -20 -- <file>` for recent commits with PR numbers visible, and `git log -1 --format=%B <commit>` to extract PR numbers. When the hosting interface is available and authenticated, pull the PR bodies and discussion for any substantive commit: `gh pr view <number> --json title,body,author,createdAt,mergedAt,labels,closingIssuesReferences,comments,reviews`. Confirm the repository and hosting provider from the available checkout and configuration; record unavailable history or PR access as a gap. Follow generated declarations back to their generator or source types only when relevant to the question. This anchor goes to every investigator and to the synthesizer.

## Step 3. Spawn Parallel Investigators

**Default to the full parallel investigation.** The goal is a complete coverage map, not a minimal one. Document the null, don't skip the search.

### Discovery

Before spawning investigators, identify the evidence sources accessible through the current environment: local files and version control, configured connectors, and available read interfaces such as CLIs or APIs. Use the environment's tool discovery facilities and repository instructions. A category can be searchable without an MCP.

Map each available source to one evidence category:

1. Source control history
2. Issue / ticket tracker
3. Long-form documents
4. Real-time team chat
5. Infrastructure observability
6. Error / exception tracking
7. Product analytics warehouse

Verify local git history and hosting access separately; `gh` availability and authentication are environment-dependent. Classify other sources using their purpose, repository instructions, tool descriptions, and resource descriptors. If a source could fit more than one category, assign its primary evidence category and record ambiguous cases in the coverage map. Discover the project's task tracker and design documents from its instructions, linked sources, and available evidence interfaces. Search the relevant sources in their respective categories.

Spawn one investigator per category with an accessible evidence source, using the configured investigation role. Launch them concurrently within the available delegation capacity. Each investigator owns one evidence category and may use the relevant read interfaces within that category; list the actual sources searched and any gaps individually. Keep cross-category leads separate for the corresponding investigator.

Each investigator's prompt is `references/investigator-prompt.md` filled in, plus:
- the applicable category playbooks `references/sources/<source>.md` for the available sources, selected through `references/source-playbook.md`
- `references/sources/incident-postmortem.md` **if the target code looks defensive** (null checks, retry logic, timeout handling, rate limiting, feature flags, egress guards, OOM handlers)
- the code anchor and the user's original question

### Investigator roster, one per available evidence category

Each category below is uniquely positioned to surface one shape of "why", shown after "Best at". Use it to know what to expect back, to name a gap when a category returns empty, and, only in the rare provably-irrelevant case, to justify a skip.

1. **Source control** (git history, an available hosting interface for PRs, code comments, tests). Spawn when accessible; report missing history or hosting access explicitly. Best at *implementation-time rationale captured during review*.
2. **Issue / ticket tracker** (the project's task tracker and its available read interface). Best at *the product or business forcing function*. Strongest when the why is external to engineering.
3. **Long-form documents** (the project's design documents and other relevant long-form sources, accessed locally or through available document interfaces). Best at *long-form design rationale*, written out before it becomes code.
4. **Real-time team chat** (e.g. Slack, Discord, Microsoft Teams, Mattermost MCP). Best at *real-time deliberation that never reached a doc*. Especially important when the source control, ticket, and doc paper trail is thin.
5. **Infrastructure observability** (e.g. Datadog, New Relic, Honeycomb, Grafana, Splunk MCP). Best at *infrastructure and runtime reality that motivated the code*. Strongest when the target reacts to an infra signal (timeouts, retries, rate limits, circuit breakers).
6. **Error / exception tracking** (e.g. Sentry, Rollbar, Bugsnag, Airbrake MCP). Best at *the specific exceptions and error trajectories that motivated defensive or corrective code*. Strongest for catch blocks, null guards, type checks, retries, and other defenses.
7. **Product analytics warehouse** (e.g. Databricks, Snowflake, BigQuery, ClickHouse, dbt, Redshift MCP). Best at *product and data reality that shaped the code*. Strongest for flag-gated code, experiment-driven ships, data migrations, and "where did this number come from" questions.

### When to skip an investigator

Only with an **explicit, written justification** that goes in the final Sources Consulted. Two valid reasons:

- **No usable read interface or evidence source is available for that category** in this environment. Flag it as a gap, not a choice, and name the actual limitation: "Real-time team chat skipped. No searchable chat interface is available." An unavailable connector does not make local documents or an available CLI inaccessible.
- **The source is provably irrelevant**, not just "probably irrelevant." A high bar: "Error / exception tracking skipped. Target is a build-time script with no runtime code path."

If your scope assessment suggests a single-commit trivial target where the PR description already contains the complete answer, you may answer inline **only after** confirming all seven available category searches would be redundant. Say so explicitly. This should be rare.

## Step 4. Synthesize

Spawn one synthesizer using the configured synthesis role. It gets:
1. The investigator findings, including any null results and any categories skipped with justification
2. The code anchor
3. The user's original question
4. The epistemics framework from `references/epistemics.md`
5. The synthesizer prompt template from `references/synthesizer-prompt.md`

## Step 5. Present

Present the synthesizer's output to the user. You may lightly edit for clarity or add context from the conversation, but **do not rewrite the confidence language**.

## Output Format

The structure is the one in `references/synthesizer-prompt.md`: The Question, The Code in Question, What We Found, What We Can Reasonably Infer, Competing Hypotheses, What We Don't Know, Sources Consulted, Confidence Summary. Adapt as needed, but keep the confidence separation intact, and keep Sources Consulted as one line per investigator, including the ones that returned nothing or were skipped, with the reason.

After Sources Consulted, if the user's `why` question is a precursor to changing this code, convert the lineage findings into a Preserve / Change / Avoid / Risk constraint set for planning the change.

## Common Failure Modes to Avoid

- **Recency bias**. Assuming the most recent commit is authoritative. The current shape is often the accretion of many earlier decisions. Trace back.
