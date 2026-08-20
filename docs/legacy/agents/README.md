# Retired agent roles (pre-ADR-0075)

These role files defined per-stage personas of the retired 20-step `change`
formula (micro-step stages, `./dev telemetry` rituals, references to
`docs/processes/*.md` files that no longer exist). They were moved out of
`.claude/agents/` so orchestrators cannot spawn them into the retired
process — change process v2 (ADR-0075) executes its 5 steps through the
`neohaskell-change` skill, not through this roster.

Kept live in `.claude/agents/`: `ci-medic` (used by the v2 pr step and
claude.yml), `retrospective-miner` (wired to retrospect.yml), and the
process-independent design/audit roles (`seneschal`, `skill-auditor`,
`skill-designer`, `ux-designer`, `ui-reviewer`, `docs-auditor`).

Rollback: `git mv` a file back and update it against the current process
doc before use — the stage names and telemetry commands in these files no
longer exist.
