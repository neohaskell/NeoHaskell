//! Interpreter for git `Repository not found` errors.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "git-repo-not-found",
    kind: Kind::Git,
    pattern: r"Repository not found",
    cause: "the git URL in `neo.json` points to a repository that does not exist or is private",
    fix:   "Check the spelling of the `git:<url>` / `github:<owner>/<repo>` entry in `neo.json`. If the repo is private, configure a credential helper (`git config --global credential.helper store`) and authenticate once.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches() {
        let i = match_kind(Kind::Git, "ERROR: Repository not found.").expect("should match");
        assert!(i.cause.contains("does not exist or is private"));
    }
}
