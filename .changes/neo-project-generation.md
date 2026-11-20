---
group: platform
component: CLI
impact: compatible
category: Added
---

## Summary

Run `neo generate --ci` to regenerate your project's Cabal and Nix files from
`neo.json` without compiling. Existing build, run, and test commands still
regenerate these files automatically. Generated executables can read their
package version through Cabal's `Paths_` module.
