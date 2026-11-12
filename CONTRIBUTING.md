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
