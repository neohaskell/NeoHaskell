# Development agent archive

This directory holds the repository's former development-agent and developer
tooling setup while it is being reworked. Paths inside this directory mirror
their former paths at the repository root. The archived files are not active
Codex instructions, discoverable project skills, live hook configuration, or
working root-level `./dev` commands.

Contents:

- `AGENTS.md` and the six scoped `AGENTS.md` files: former agent instructions.
- `.agents/skills/`: all 15 former project skills and their supporting files.
- `.codex/` and `.coderabbit.yaml`: former agent and AI review configuration.
- `scripts/guards/codex-hooks.py` and its test: former Codex hook adapter.
- `scripts/agent-config-check`, `scripts/neo-skills-check`, and
  `scripts/process-check.sh`: checks for the former agent setup.
- `scripts/cloud-setup.sh`, `scripts/regression-scaffold-smoke`, and the
  `scripts/pr-comments-*` files: agent environment, skill, and review helpers.
- `dev`, the rest of `scripts/`, and all of `codemap/`: the former command
  entrypoint, its implementations, and the complete codemap source and output.

No `CLAUDE.md`, `GEMINI.md`, or equivalent alternate agent guide was present in
this checkout when the archive was created. Product AI features remain in their
original locations.

The archived documents and scripts still contain their former root-relative
paths. Review those references when rebuilding the agent setup. GitHub workflows
and repository documentation still call the old `./dev`, `scripts/`, and
`codemap/` paths; those calls will fail until the rework reconnects them.
