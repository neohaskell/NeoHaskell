# Contributing

[AGENTS.md](AGENTS.md) is the shared starting point for contributors and coding
agents. It explains the project's scope, development principles, and workflow,
with links to more detailed guidance when a task needs it.

When working with a coding agent, have it read AGENTS.md before it starts so
its work follows the repository's conventions. Human contributors can use the
same guidance to understand how changes are developed, reviewed, and verified.

## Check workflows locally

The Nix development shell includes [act](https://nektosact.com/), which runs
GitHub Actions jobs locally. List the framework test jobs with:

```sh
nix develop --command act -l -W .github/workflows/test.yml
```

With Docker running, execute the ADR index check with:

```sh
act_git_dir=$(git rev-parse --path-format=absolute --git-common-dir)
nix develop --command act push -W .github/workflows/checks.yml -j adr-index \
  --container-architecture linux/amd64 \
  --container-options "--volume ${act_git_dir}:${act_git_dir}:ro" \
  -P ubuntu-latest=catthehacker/ubuntu:act-latest
```

The read-only Git mount supports linked worktrees. This command downloads a
runner image on first use. Run Haskell suites directly
through the host Nix shell using the [test instructions](.agents/skills/neohaskell-test/SKILL.md).
Local jobs do not prove native macOS runner behavior, GitHub permissions, or cache
downloads on a fresh hosted runner. Keep those checks on GitHub Actions.

## Verify installer changes locally

The installer verification entrypoint is `python3 installer/verification/verify.py`.
The maintained [verification guide](installer/verification/README.md) describes
input classification, local evidence, and repository hook setup. The full
installer matrix runs on suitable local environments. It does not run in hosted CI.

Fixture checks prove the verifier and Git gate. They do not establish installer
compatibility. Missing native Windows or macOS coverage remains incomplete.
The existing generated-project and component checks still apply.
