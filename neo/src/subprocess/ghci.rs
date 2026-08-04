use crate::errors::NeoError;
use std::process::Stdio;
use tokio::io::AsyncWriteExt;
use tokio::process::{Child, ChildStdin, Command};

pub struct GhciSession {
    child: Child,
    stdin: ChildStdin,
}

impl GhciSession {
    pub async fn start() -> miette::Result<Self> {
        let mut child = Command::new("nix")
            .args(["develop", "--command", "bash", "-c", "cabal repl"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| NeoError::SubprocessFailed {
                operation: "starting `cabal repl` (via `nix develop --command bash -c 'cabal repl'`)".to_string(),
                cause: format!("could not spawn child process: {}", e),
                fix: "Ensure `nix` is installed and on PATH (`which nix`), and that you are in a flake-enabled project directory.".to_string(),
            })?;

        let stdin = child.stdin.take().ok_or_else(|| {
            NeoError::SubprocessFailed {
                operation: "starting `cabal repl`".to_string(),
                cause: "failed to attach to child stdin (piped stdin was not available)".to_string(),
                fix: "This is an internal `neo` bug — `Stdio::piped()` should always provide a stdin handle. Re-run with `RUST_BACKTRACE=1` and file an issue at https://github.com/NeoHaskell/neocli/issues.".to_string(),
            }
        })?;

        let mut session = Self { child, stdin };

        // Wait for initial prompt
        session.wait_for_prompt().await?;

        Ok(session)
    }

    pub async fn reload(&mut self) -> miette::Result<Vec<String>> {
        self.stdin.write_all(b":reload\n").await
            .map_err(|e| NeoError::SubprocessFailed {
                operation: "writing `:reload` to GHCi stdin".to_string(),
                cause: format!("could not send command to child: {}", e),
                fix: "The GHCi child likely died. Quit `neo build --watch` (Ctrl-C) and re-run it to start a fresh GHCi session.".to_string(),
            })?;

        self.wait_for_prompt().await
    }

    async fn wait_for_prompt(&mut self) -> miette::Result<Vec<String>> {
        let stdout = self.child.stdout.as_mut().ok_or_else(|| {
            NeoError::SubprocessFailed {
                operation: "reading GHCi stdout".to_string(),
                cause: "failed to attach to child stdout (piped stdout was not available)".to_string(),
                fix: "This is an internal `neo` bug — `Stdio::piped()` should always provide a stdout handle. Re-run with `RUST_BACKTRACE=1` and file an issue.".to_string(),
            }
        })?;
        Self::read_until_prompt(stdout).await
    }

    async fn read_until_prompt<R: tokio::io::AsyncRead + Unpin>(reader: &mut R) -> miette::Result<Vec<String>> {
        let mut buffer = Vec::new();
        let mut output_lines = Vec::new();
        let mut byte = [0u8; 1];

        loop {
            match tokio::io::AsyncReadExt::read_exact(reader, &mut byte).await {
                Ok(_) => {
                    buffer.push(byte[0]);
                    let current_str = String::from_utf8_lossy(&buffer);
                    if current_str.ends_with("> ") || current_str.ends_with("| ") {
                        output_lines.push(current_str.to_string());
                        break;
                    }
                    if byte[0] == b'\n' {
                        output_lines.push(current_str.to_string());
                        buffer.clear();
                    }
                }
                Err(e) => return Err(NeoError::SubprocessFailed {
                    operation: "reading GHCi output".to_string(),
                    cause: format!("child stream ended before a prompt was seen: {}", e),
                    fix: "The GHCi child died (often: a syntax error so severe it killed the REPL, or it ran out of memory). Quit `neo build --watch` (Ctrl-C) and re-run to start a fresh session.".to_string(),
                }.into()),
            }
        }

        Ok(output_lines)
    }

    pub async fn stop(mut self) -> miette::Result<()> {
        self.stdin.write_all(b":quit\n").await.ok();
        self.child.wait().await
            .map_err(|e| NeoError::SubprocessFailed {
                operation: "waiting on GHCi to exit after `:quit`".to_string(),
                cause: format!("could not reap child process: {}", e),
                fix: "Send the cabal repl SIGKILL if it is still running: `pkill -9 ghc` then re-run.".to_string(),
            })?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[tokio::test]
    async fn test_read_until_prompt() {
        let input = "GHCi, version 9.4.5: https://www.haskell.org/ghc/  :? for help\n[1 of 1] Compiling Main             ( src/Main.hs, interpreted )\nOk, one module loaded.\nghci> ";
        let mut cursor = Cursor::new(input);

        let output = GhciSession::read_until_prompt(&mut cursor).await.unwrap();
        assert_eq!(output.len(), 4);
        assert_eq!(output[3], "ghci> ");
    }

    #[tokio::test]
    async fn test_read_until_prompt_multiline() {
        let input = "Some output\nMore output\nPrelude> ";
        let mut cursor = Cursor::new(input);

        let output = GhciSession::read_until_prompt(&mut cursor).await.unwrap();
        assert_eq!(output.len(), 3);
        assert_eq!(output[0], "Some output\n");
        assert_eq!(output[1], "More output\n");
        assert_eq!(output[2], "Prelude> ");
    }

    #[tokio::test]
    async fn test_read_until_prompt_with_pipe() {
        let input = "module Main where\n  | ";
        let mut cursor = Cursor::new(input);

        let output = GhciSession::read_until_prompt(&mut cursor).await.unwrap();
        assert_eq!(output.len(), 2);
        assert_eq!(output[1], "  | ");
    }

    #[tokio::test]
    async fn test_read_until_prompt_empty() {
        let input = "> ";
        let mut cursor = Cursor::new(input);

        let output = GhciSession::read_until_prompt(&mut cursor).await.unwrap();
        assert_eq!(output.len(), 1);
        assert_eq!(output[0], "> ");
    }

    #[tokio::test]
    async fn test_read_until_prompt_eof_error() {
        let input = "Some output without prompt";
        let mut cursor = Cursor::new(input);

        let result = GhciSession::read_until_prompt(&mut cursor).await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        if let Some(NeoError::SubprocessFailed { operation, cause, fix }) = err.downcast_ref::<NeoError>() {
            assert_eq!(operation, "reading GHCi output");
            assert!(cause.contains("ended before a prompt"), "cause: {}", cause);
            assert!(fix.contains("Ctrl-C"), "fix: {}", fix);
        } else {
            panic!("Expected NeoError::SubprocessFailed, got {:?}", err);
        }
    }
}
