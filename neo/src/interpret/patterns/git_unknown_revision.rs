//! Interpreter for git `unknown revision or path not in the working tree` errors.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "git-unknown-revision",
    kind: Kind::Git,
    pattern: r"unknown revision or path not in the working tree",
    cause: "git could not resolve the requested revision",
    fix:   "Check that the `#<ref>` in your `neo.json` `git:` or `github:` dependency exists on the remote (use `git ls-remote <url>`).",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches() {
        let captured = "fatal: ambiguous argument 'abc': unknown revision or path not in the working tree.";
        let i = match_kind(Kind::Git, captured).expect("should match");
        assert!(i.cause.contains("could not resolve"));
    }
}
