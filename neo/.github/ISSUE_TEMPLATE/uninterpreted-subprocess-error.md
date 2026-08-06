---
name: Uninterpreted subprocess error
about: "`neo` ran a child process that failed in a way it didn't recognise."
title: "[subprocess-raw] <command that failed>"
labels: ["error-interpretation"]
---

`neo` ran a subprocess and it failed, but the matchers in
`src/subprocess/interpret.rs` (`interpret_cabal` / `interpret_nix` /
`interpret_git` / `interpret_hurl`) didn't recognise the failure. With your
logs we can add a match so the next person sees a concrete fix recipe instead
of this issue template.

### What command did you run?

<!-- e.g. `neo build`, `neo test`, `neo new my-project`. -->

### `neo --version`

```
(paste output here)
```

### Operating system

<!-- e.g. macOS 14.5 (darwin), NixOS 24.11, Ubuntu 24.04. -->

### Full child output

Paste the entire `--- full child output ---` block from the error message
between the fences below. **Do not truncate** — the line we need to match
on is often one of the "boring" ones.

```text
(paste full output here)
```

### What were you trying to do?

<!-- One or two sentences about project state: fresh `neo new`? Edited
`neo.json`? Running on a corp network? Anything that scopes the repro. -->

### Anything else

<!-- Optional. Links to `neo.json`, `flake.lock` diffs, etc. -->
