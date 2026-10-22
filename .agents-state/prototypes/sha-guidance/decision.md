# SHA-matched agent guidance prototype

This is a throwaway planning artifact. The production feature is not implemented.

## Decision

Use a stable `AGENTS.md` pointer to a small, Git-ignored active index. The index states the app's resolved full framework SHA and links to the task index under `cache/<sha>/`. On a SHA change, the active index must say that exact-SHA docs are unavailable before any fetch begins. It may link a corpus only after that SHA's own corpus is present and validated. Keep `CLAUDE.md` as `@AGENTS.md`. Nick accepted the small active index in the live Wayfinder exchange on 2026-10-22.

The active index was compared with a `current` symlink to the SHA cache. Both can select the exact SHA. The index can state the missing SHA and an actionable unavailable status. The symlink alone becomes a broken path when its target is absent and conveys no status. Putting a SHA literal in committed `AGENTS.md` would require editing developer instructions on every upgrade, so it does not fit the accepted preservation contract.

## Observed checks

1. `sh .agents-state/prototypes/sha-guidance/probe.sh` passed initial sync, SHA change, offline reading, missing exact-SHA docs, preserved developer files, and the symlink comparison. The script uses local Git commits containing synthetic docs. It does not claim that NeoHaskell already publishes a corpus.
2. `codex exec --ephemeral --json -s read-only -C <scratch-consumer> <question>` on Codex CLI 0.162.1 read the active index, the selected SHA's task index, and its entities page. It returned `beta` from `cache/b50ee43f05f6b31a193fbe4d9ce359a22f06d370/entities.md`. The raw local trace is `codex-discovery-clean.jsonl`.
3. After the active index named missing SHA `2a30d76156628dc9bd805bd3785867564c6d26ba`, the same Codex question returned an unavailable answer without reading the still-cached `beta` page. The raw local trace is `codex-missing.jsonl`.
4. `neo new sha-guidance-consumer --ci` created a real scratch consumer. Its `neo.json`, generated `cabal.project`, and `flake.nix` all pin `4459b861649d12e883a38544e09f9b09374038c8`. `git cat-file -e 4459b861649d12e883a38544e09f9b09374038c8:docs/agent/index.md` fails because that pre-doc commit has no corpus. That SHA must report unavailable.

Claude Code was not installed locally, so no live Claude behavior was observed. Its [memory documentation](https://code.claude.com/docs/en/memory) documents `@AGENTS.md` imports and says the import remains valid when direct `AGENTS.md` loading is available. A live Claude check remains a release acceptance requirement.

## Markdown tool experiment

The [markdown-unlit README](https://github.com/sol/markdown-unlit) documents `-pgmL markdown-unlit`, `haskell top`, and `haskell ignore`. Version 0.6.0 built with `nix develop <repo> --command cabal install markdown-unlit --ignore-project --installdir=<scratch-bin>`. A GHC run with `-pgmL` parsed `example.md` and reached its `Core` import. This proves extraction, but the host repository shell alone cannot resolve the generated app's `nhcore` package.

The generated consumer has a scratch Cabal test suite pointing at the Markdown file through `Example.lhs`. It uses the installed `markdown-unlit` binary and depends on `nhcore`. The consumer's Cabal source package resolved commit `4459b861649d12e883a38544e09f9b09374038c8`. `nix develop --command cabal test agent-doc-probe` compiled and ran the Markdown example successfully. The `haskell ignore` fragment contains invalid standalone code and did not break compilation. Adding `broken = missingName` in an ordinary `haskell` fence made the same command fail at `docs/Example.lhs:27` with `missingName` out of scope. This is a meaningful failure check for the proposed blocking gate.

## Execution acceptance checks

1. A fresh `neo new` and a normal build, run, or test reconciliation select docs from the resolved full SHA. A changed SHA replaces the active index before retrieval. An explicit retry can recover a failed sync.
2. A successful sync leaves an immutable `cache/<sha>/` that agents can read offline. A fresh clone performs initial sync. A missing corpus, failed fetch, or invalid corpus leaves an unavailable index with the requested SHA and emits a warning while ordinary app commands continue.
3. Live Codex and Claude checks start in generated consumers. They open only relevant pages through their startup instructions after initial sync and after a SHA change. With a missing exact-SHA corpus, neither reads another cached SHA. Developer-edited instructions and skills remain byte-identical through repeated syncs.
4. The CI and release gate compile runnable Haskell fences against the same framework SHA as the docs. Explanatory fragments carry an explicit ignore label and still receive prose review. The gate fails on a bad runnable fence, missing page, invalid link, or incomplete testbed coverage.
5. Internal Haskell tool commands run through `nix develop --command`. Production packaging must expose the chosen Markdown tool inside that shell. The scratch test uses an absolute binary path only to isolate this prototype.

## Tool result

`markdown-unlit` is feasible for a page whose runnable fences form one Haskell module. This prototype proves compilation at one real generated-app SHA and a deliberate rejection at that SHA. It does not prove that every future corpus page can use one-module layout, that production Nix packaging exposes the executable, or that all testbed flows run. Execution must settle those integration details and keep the gate at the matching SHA.
