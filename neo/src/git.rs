use std::path::Path;
use std::process::Command;
use crate::errors::NeoError;

pub fn init(path: &Path) -> miette::Result<()> {
    let output = Command::new("git")
        .arg("init")
        .current_dir(path)
        .output()
        .map_err(|e| NeoError::GitError {
            subcommand: "init".to_string(),
            reason: format!("could not spawn git: {}", e),
            fix: "Ensure `git` is installed and on PATH (run `which git`). If git is installed, ensure your current shell can execute it (you may need to open a new shell).".to_string(),
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(NeoError::GitError {
            subcommand: format!("init {}", path.display()),
            reason: stderr.trim().to_string(),
            fix: format!("Inspect the directory `{}` — `git init` failed (often: directory is unwritable, or already contains a corrupt `.git`). Try `rm -rf {}/.git && git init {}` manually to diagnose.", path.display(), path.display(), path.display()),
        }.into());
    }

    let _ = Command::new("git").args(["config", "user.email", "neo@example.com"]).current_dir(path).output();
    let _ = Command::new("git").args(["config", "user.name", "NeoCLI"]).current_dir(path).output();

    Ok(())
}

pub fn add_all(path: &Path) -> miette::Result<()> {
    let output = Command::new("git")
        .arg("add")
        .arg(".")
        .current_dir(path)
        .output()
        .map_err(|e| NeoError::GitError {
            subcommand: "add .".to_string(),
            reason: format!("could not spawn git: {}", e),
            fix: "Ensure `git` is installed and on PATH (`which git`).".to_string(),
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(NeoError::GitError {
            subcommand: format!("add . (in {})", path.display()),
            reason: stderr.trim().to_string(),
            fix: format!("Run `git status` in `{}` to inspect — usually means a file referenced in `.gitignore` rules is being negated incorrectly, or a submodule is in an unexpected state.", path.display()),
        }.into());
    }

    Ok(())
}

pub fn commit(path: &Path, message: &str) -> miette::Result<()> {
    let output = Command::new("git")
        .arg("commit")
        .arg("--no-verify")
        .arg("-m")
        .arg(message)
        .current_dir(path)
        .output()
        .map_err(|e| NeoError::GitError {
            subcommand: "commit".to_string(),
            reason: format!("could not spawn git: {}", e),
            fix: "Ensure `git` is installed and on PATH (`which git`).".to_string(),
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(NeoError::GitError {
            subcommand: format!("commit -m {:?}", message),
            reason: stderr.trim().to_string(),
            fix: format!("Configure a git identity for `{}` if missing: `git -C {} config user.email you@example.com && git -C {} config user.name 'Your Name'`. Then re-run.", path.display(), path.display(), path.display()),
        }.into());
    }

    Ok(())
}

pub fn install_lock_hook(path: &Path) -> miette::Result<()> {
    let hooks_dir = path.join(".git").join("hooks");
    std::fs::create_dir_all(&hooks_dir)
        .map_err(|e| NeoError::io_at("creating the git hooks directory at", &hooks_dir, e))?;

    let hook_path = hooks_dir.join("pre-commit");
    let hook_content = r#"#!/bin/sh
# NeoHaskell Lock Hook
# This hook prevents committing changes to locked files.
neo lock check
"#;

    std::fs::write(&hook_path, hook_content)
        .map_err(|e| NeoError::io_at("writing the pre-commit hook at", &hook_path, e))?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&hook_path)
            .map_err(|e| NeoError::io_at("reading metadata of the pre-commit hook at", &hook_path, e))?
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&hook_path, perms)
            .map_err(|e| NeoError::io_at("setting executable permissions on the pre-commit hook at", &hook_path, e))?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_git_init() {
        let dir = tempdir().unwrap();
        init(dir.path()).expect("Failed to init git");
        assert!(dir.path().join(".git").exists());
    }

    #[test]
    fn test_install_lock_hook() {
        let dir = tempdir().unwrap();
        // Create .git dir first as install_lock_hook expects it (though it creates hooks dir)
        std::fs::create_dir_all(dir.path().join(".git")).unwrap();

        install_lock_hook(dir.path()).expect("Failed to install lock hook");
        let hook_path = dir.path().join(".git/hooks/pre-commit");
        assert!(hook_path.exists());

        let content = std::fs::read_to_string(hook_path).unwrap();
        assert!(content.contains("neo lock check"));

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let metadata = std::fs::metadata(dir.path().join(".git/hooks/pre-commit")).unwrap();
            assert!(metadata.permissions().mode() & 0o111 != 0);
        }
    }
}
