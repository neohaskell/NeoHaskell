//! Interpreter for hurl connection-refused errors.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "hurl-connection-refused",
    kind: Kind::Hurl,
    pattern: r"Connection refused|Failed to connect",
    cause: "hurl could not reach the target server",
    fix:   "Start the server `neo` is testing against before running `neo test` (e.g. in another terminal: `neo run`), or change the host:port in your `.hurl` files to point at a running instance.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches() {
        let i = match_kind(Kind::Hurl, "error: HTTP connection: Connection refused").expect("should match");
        assert!(i.cause.contains("could not reach the target server"));
        assert!(i.fix.contains("neo run"));
    }
}
