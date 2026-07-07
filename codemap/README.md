# codemap/ — localization assets (Phase 3, coming)

This directory will hold the pipeline's localization assets, per the
[Continuous Generation pipeline plan](../docs/plans/2026-07-07-continuous-generation-pipeline-plan.md)
(tracker: [#715](https://github.com/neohaskell/NeoHaskell/issues/715)):

- `capabilities.yaml` — curated capability ontology (owns-globs, responsibilities, aliases, tests, risk tags)
- `extension-points.yaml` — where new things of each kind go (create/register/tests/skill)
- `signatures/` — generated hoogle-format export lists per namespace (committed, CI sync-checked)
- `MAP.md` — generated human/agent render
- `check.py` — CI validity checker (orphan modules, ghost paths, alias uniqueness, doc-ratchet)

**Governing rule:** no agent-visible document without a CI check or a generation source.

Until Phase 3 lands, this directory is a placeholder. Salvaged knowledge from the
pre-pipeline docs lives in `docs/archive/2026-07-ai-artifacts/SALVAGE.md`.
