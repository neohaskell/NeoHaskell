//! Interpreter for nix `attribute 'X' missing` errors (single-quote variant).
//!
//! Companion to `nix_attribute_missing_backticks` — same failure mode, just
//! the other quote style nix uses depending on context.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "nix-attribute-missing-single-quotes",
    kind: Kind::Nix,
    pattern: r"attribute '(?<attr>[^']*)' missing",
    cause: "`flake.nix` references attribute `{attr}` which no longer exists in the resolved flake inputs",
    fix:   "Run `rm flake.nix cabal.project *.cabal && neo build` to regenerate the build artifacts from the current `neo.json`. Direnv will pick up the new flake automatically.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches() {
        let captured = "error: attribute 'xyz' missing\n       at /nix/store/...";
        let i = match_kind(Kind::Nix, captured).expect("should match");
        assert!(i.cause.contains("`flake.nix` references attribute `xyz`"), "cause: {}", i.cause);
        assert!(i.fix.contains("rm flake.nix cabal.project *.cabal && neo build"), "fix: {}", i.fix);
    }
}
