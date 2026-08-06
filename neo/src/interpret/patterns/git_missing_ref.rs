//! Interpreter for git `couldn't find remote ref <ref>` errors.
//!
//! Emitted when a `git:` or `github:` dependency in `neo.json` points at a
//! branch / tag / SHA that doesn't exist on the remote.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "git-missing-ref",
    kind: Kind::Git,
    pattern: r"couldn't find remote ref\s+(?<ref>\S+)",
    cause: "git dependency in `neo.json` points to ref `{ref}` which does not exist on the remote",
    fix:   "Edit `neo.json`: change `#{ref}` (in the `git:<url>#<ref>` or `github:<owner>/<repo>#<ref>` entry) to a real branch, tag, or full SHA on the remote. List remote refs with `git ls-remote <url>`.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches_typical() {
        let captured = "fatal: couldn't find remote ref refs/heads/typo\n";
        let i = match_kind(Kind::Git, captured).expect("should match");
        assert!(i.cause.contains("ref `refs/heads/typo`"), "cause: {}", i.cause);
        assert!(i.fix.contains("`#refs/heads/typo`"), "fix: {}", i.fix);
    }

    #[test]
    fn unicode_ref_preserved_verbatim() {
        let captured = "fatal: couldn't find remote ref refs/heads/feature/日本語\n";
        let i = match_kind(Kind::Git, captured).unwrap();
        assert!(i.cause.contains("refs/heads/feature/日本語"), "cause: {}", i.cause);
    }
}
