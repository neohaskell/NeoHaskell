//! Interpreter for cabal `unknown package: <name>` errors.
//!
//! Emitted when `cabal` cannot find a referenced package — either because
//! the name is misspelled, or because it lives outside the NeoPackages
//! registry and was not declared with an explicit `hackage:` / `git:` /
//! `github:` / `file:` source prefix in `neo.json`.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "cabal-unknown-package",
    kind: Kind::Cabal,
    // Capture stops at whitespace or any of `,;.:` — mirrors the original
    // `strip_trailing_punct` helper. Captures an empty string when the
    // name is missing entirely (`unknown package: \n`).
    pattern: r"unknown package:\s*(?<package>[^\s,;.:]*)",
    cause: "package `{package}` is referenced in `neo.json` but is neither in the NeoPackages registry nor declared as `hackage:`/`git:`/`github:`/`file:`",
    fix:   "Edit `neo.json`: replace the entry for `{package}` with `\"hackage:{package}\": \"^…\"` if it is on Hackage, OR use `\"git:<url>#<ref>\"` / `\"github:<owner>/<repo>#<ref>\"` / `\"file:<path>\"` for an explicit source, OR remove it. Then re-run `neo build`.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches_typical_cabal_output() {
        let captured = "Resolving dependencies...\nError: unknown package: definitely-wrong-pkg\ncabal: Could not build the package.";
        let i = match_kind(Kind::Cabal, captured).expect("should match");
        assert!(i.cause.contains("package `definitely-wrong-pkg`"), "cause: {}", i.cause);
        assert!(i.fix.contains("hackage:definitely-wrong-pkg"), "fix: {}", i.fix);
        assert!(i.fix.contains("neo build"), "fix: {}", i.fix);
    }

    #[test]
    fn handles_trailing_newline() {
        let captured = "unknown package: foo\nmore output";
        let i = match_kind(Kind::Cabal, captured).unwrap();
        assert!(i.cause.contains("package `foo`"));
    }

    #[test]
    fn empty_package_name() {
        let captured = "unknown package: \n";
        let i = match_kind(Kind::Cabal, captured).unwrap();
        assert!(i.cause.contains("package ``"), "cause: {}", i.cause);
    }
}
