#![allow(dead_code)]

use assert_cmd::Command;
use std::path::{Path, PathBuf};
use std::process::Command as StdCommand;

pub fn neo_bin() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let bin = PathBuf::from(manifest_dir)
        .join("result")
        .join("bin")
        .join("neo");
    if !bin.exists() {
        panic!(
            "Nix-built binary not found at {}.\nRun `nix build` at the repo root before invoking the e2e suite.",
            bin.display()
        );
    }
    bin
}

pub struct Sandbox {
    pub root: PathBuf,
    pub home: PathBuf,
    keep: bool,
}

impl Sandbox {
    pub fn new(name: &str) -> Self {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let root = PathBuf::from(manifest_dir)
            .join("target")
            .join("e2e-sandbox")
            .join(name);
        if root.exists() {
            std::fs::remove_dir_all(&root).expect("could not clean previous sandbox");
        }
        std::fs::create_dir_all(&root).expect("could not create sandbox");
        let home = root.join("home");
        std::fs::create_dir_all(&home).expect("could not create sandbox home");
        let keep = std::env::var("NEO_E2E_KEEP").is_ok();
        Self { root, home, keep }
    }

    pub fn path<P: AsRef<Path>>(&self, sub: P) -> PathBuf {
        self.root.join(sub)
    }

    fn path_with_neo() -> String {
        let bin_dir = neo_bin()
            .parent()
            .expect("neo_bin has no parent")
            .to_path_buf();
        let inherited = std::env::var("PATH").unwrap_or_default();
        format!("{}:{}", bin_dir.display(), inherited)
    }

    pub fn neo<P: AsRef<Path>>(&self, dir: P) -> Command {
        let target = self.root.join(dir);
        std::fs::create_dir_all(&target).ok();
        let bin = neo_bin();
        let mut cmd = Command::new(bin.as_os_str());
        cmd.current_dir(&target)
            .env_remove("CI")
            .env_remove("NEO_SKIP_NETWORK")
            .env("HOME", &self.home)
            .env("PATH", Self::path_with_neo())
            .env("GIT_AUTHOR_NAME", "neo-e2e")
            .env("GIT_AUTHOR_EMAIL", "e2e@neo.test")
            .env("GIT_COMMITTER_NAME", "neo-e2e")
            .env("GIT_COMMITTER_EMAIL", "e2e@neo.test");
        cmd
    }

    pub fn git<P: AsRef<Path>>(&self, dir: P, args: &[&str]) -> std::process::Output {
        StdCommand::new("git")
            .args(args)
            .current_dir(self.root.join(dir))
            .env("HOME", &self.home)
            .env("PATH", Self::path_with_neo())
            .env("GIT_AUTHOR_NAME", "neo-e2e")
            .env("GIT_AUTHOR_EMAIL", "e2e@neo.test")
            .env("GIT_COMMITTER_NAME", "neo-e2e")
            .env("GIT_COMMITTER_EMAIL", "e2e@neo.test")
            .output()
            .expect("git failed to spawn")
    }
}

impl Drop for Sandbox {
    fn drop(&mut self) {
        if self.keep || std::thread::panicking() {
            eprintln!("e2e: leaving sandbox at {}", self.root.display());
            return;
        }
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

pub fn read_neo_json(project_dir: &Path) -> serde_json::Value {
    let content = std::fs::read_to_string(project_dir.join("neo.json"))
        .expect("neo.json missing");
    serde_json::from_str(&content).expect("neo.json is not valid JSON")
}

pub fn file_contains(path: &Path, needle: &str) -> bool {
    std::fs::read_to_string(path)
        .map(|s| s.contains(needle))
        .unwrap_or(false)
}

pub fn fetch_neohaskell_main_sha() -> String {
    let out = StdCommand::new("git")
        .args(["ls-remote", "https://github.com/NeoHaskell/neohaskell", "main"])
        .output()
        .expect("git ls-remote spawn failed");
    if !out.status.success() {
        panic!(
            "git ls-remote failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    stdout
        .split_whitespace()
        .next()
        .expect("empty git ls-remote response")
        .to_string()
}

pub fn is_executable(path: &Path) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        match std::fs::metadata(path) {
            Ok(m) => m.permissions().mode() & 0o111 != 0,
            Err(_) => false,
        }
    }
    #[cfg(not(unix))]
    {
        path.exists()
    }
}

pub fn count_processes_in(sandbox_root: &Path) -> usize {
    let pattern = sandbox_root.to_string_lossy().to_string();
    let out = StdCommand::new("pgrep")
        .args(["-f", &pattern])
        .output();
    match out {
        Ok(o) if o.status.success() => {
            String::from_utf8_lossy(&o.stdout).lines().count()
        }
        _ => 0,
    }
}
