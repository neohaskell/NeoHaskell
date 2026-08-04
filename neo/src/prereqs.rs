use crate::errors::NeoError;
use crate::output::OutputMode;
use crossterm::style::Stylize;

pub async fn require_nix() -> miette::Result<()> {
    if tokio::process::Command::new("nix")
        .arg("--version")
        .output()
        .await
        .is_err()
    {
        return Err(NeoError::NixNotFound.into());
    }
    Ok(())
}

pub async fn require_git() -> miette::Result<()> {
    if tokio::process::Command::new("git")
        .arg("--version")
        .output()
        .await
        .is_err()
    {
        return Err(NeoError::GitNotFound.into());
    }
    Ok(())
}

pub async fn warn_direnv(output_mode: &OutputMode) {
    if tokio::process::Command::new("direnv")
        .arg("--version")
        .output()
        .await
        .is_err()
    {
        let msg = "direnv is not installed. Install it for automatic HLS integration in your editor.";
        if output_mode.is_ci() {
            println!("[warn] {}", msg);
        } else {
            println!("{} {}", "⚠".yellow(), msg.yellow());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_require_nix() {
        require_nix().await.expect("nix must be present in the test environment");
    }

    #[tokio::test]
    async fn test_require_git() {
        require_git().await.expect("git must be present in the test environment");
    }

    #[tokio::test]
    async fn test_warn_direnv() {
        // This should just not panic
        warn_direnv(&OutputMode::Ci).await;
        // Testing Interactive mode here is hard due to DefaultTerminal type constraints,
        // but since warn_direnv only uses is_ci(), we've covered the logic.
    }
}
