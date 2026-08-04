//! Interpreter for nix `error: builder for ... failed with exit code` errors.
//!
//! Generic nix-build-failed wrapper. Names the failing derivation but
//! cannot extract the underlying cause (which lives in `nix log <drv>`).

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "nix-builder-failed",
    kind: Kind::Nix,
    // The drv path may be quoted with `'` or `` ` ``, or unquoted. `(?s)`
    // lets `.*?` cross newlines because nix prints the "failed with exit
    // code N" line some distance after the "builder for" line.
    pattern: r"(?s)error: builder for ['`]?(?<drv>[^\s'`]+)['`]?.*?failed with exit code",
    cause: "nix derivation `{drv}` failed to build — the underlying compiler / cabal / shell step exited non-zero inside the sandbox",
    fix:   "Read the full nix log to find the real error: `nix log {drv}`. Look for the line beginning with `Error:`, `error:`, or the last `cabal: ` line. Most common causes: (a) a dependency in `neo.json` is misspelled (cabal will say `unknown package: <name>`), (b) version constraints don't overlap (cabal will say `Could not resolve dependencies`), (c) a transitive `git:` dep points at a non-existent ref.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches_quoted_drv() {
        let captured = "error: builder for '/nix/store/abc-foo.drv' failed with exit code 1";
        let i = match_kind(Kind::Nix, captured).expect("should match");
        assert!(i.cause.contains("`/nix/store/abc-foo.drv`"), "cause: {}", i.cause);
        assert!(i.fix.contains("nix log /nix/store/abc-foo.drv"), "fix: {}", i.fix);
    }

    #[test]
    fn matches_unquoted_drv() {
        let captured = "error: builder for /nix/store/xyz.drv\n  blah\n  failed with exit code 2";
        let i = match_kind(Kind::Nix, captured).expect("should match");
        assert!(i.cause.contains("/nix/store/xyz.drv"));
    }
}
