use super::{InstallationOwner, TransactionError, nix::NixBlock, process::WorkerRequest};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::io;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::process::Command;

const SYSTEM: &str = "/etc/nix/nix.conf";
const CUSTOM: &str = "/etc/nix/nix.custom.conf";
const PENDING: &str = "/etc/nix/.neohaskell-cache-setup";
fn manual() -> TransactionError {
    TransactionError::NixBlocked(NixBlock::CacheConfiguration)
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub(super) struct CachePlan {
    determinate: bool,
    main_digest: String,
    target_digest: String,
    additions: String,
}

fn digest(data: &[u8]) -> String {
    format!("{:x}", Sha256::digest(data))
}

fn protected_source(path: &Path) -> Result<Vec<u8>, TransactionError> {
    let metadata = fs::symlink_metadata(path).map_err(|_| manual())?;
    if metadata.uid() != 0
        || metadata.mode() & 0o022 != 0
        || !metadata.is_file()
        || metadata.len() > 1024 * 1024
    {
        return Err(manual());
    }
    fs::read(path).map_err(|_| manual())
}

pub(super) fn check_pending() -> Result<(), TransactionError> {
    match fs::symlink_metadata(PENDING) {
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
        _ => Err(manual()),
    }
}

impl CachePlan {
    fn inspect(additions: String) -> Result<Option<Self>, TransactionError> {
        check_pending()?;
        if additions.is_empty() {
            return Ok(None);
        }
        let directory = fs::symlink_metadata("/etc/nix").map_err(|_| manual())?;
        if !directory.is_dir() || directory.uid() != 0 || directory.mode() & 0o022 != 0 {
            return Err(manual());
        }
        let main = protected_source(Path::new(SYSTEM))?;
        let source = std::str::from_utf8(&main).map_err(|_| manual())?;
        let determinate = source
            .lines()
            .any(|line| line == "# DETERMINATE NIX CONFIG");
        let includes: Vec<_> = source
            .lines()
            .map(|line| line.split('#').next().unwrap_or("").trim())
            .filter(|line| matches!(line.split_whitespace().next(), Some("!include" | "include")))
            .collect();
        if determinate {
            if includes != ["!include nix.custom.conf"] {
                return Err(manual());
            }
        } else if !includes.is_empty()
            || Path::new("/run/current-system").exists()
            || source.to_lowercase().contains("generated")
        {
            return Err(manual());
        }
        let target = if determinate {
            protected_source(Path::new(CUSTOM))?
        } else {
            main.clone()
        };
        Ok(Some(Self {
            determinate,
            main_digest: digest(&main),
            target_digest: digest(&target),
            additions,
        }))
    }

    fn target(&self) -> &'static str {
        if self.determinate { CUSTOM } else { SYSTEM }
    }

    pub(super) fn review(&self) -> String {
        format!(
            "Add the following settings to {} and restart the Nix daemon with administrator access.\n{}",
            self.target(),
            self.additions
        )
    }

    pub(super) fn command(&self) -> io::Result<Command> {
        let mut command = Command::new("/usr/bin/sudo");
        command.args([
            "-n",
            "--",
            "/bin/sh",
            "-eu",
            "-c",
            include_str!("cache_setup.sh"),
            "neo-cache-setup",
            self.target(),
            &self.main_digest,
            &self.target_digest,
            &self.additions,
        ]);
        Ok(command)
    }
}

fn missing_settings(
    bytes: &[u8],
    needs_command: bool,
    has_flakes: bool,
) -> Result<String, TransactionError> {
    let settings: serde_json::Value = serde_json::from_slice(bytes).map_err(|_| manual())?;
    let (urls, keys) = crate::candidate::nix_cache_settings().map_err(|_| manual())?;
    let mut additions = String::new();
    for (name, required) in [
        ("substituters", urls.as_str()),
        ("trusted-public-keys", keys.as_str()),
        ("experimental-features", "nix-command flakes"),
    ] {
        let actual = settings[name]["value"].as_array().ok_or_else(manual)?;
        let missing: Vec<_> = required
            .split_ascii_whitespace()
            .filter(|&value| {
                if name == "experimental-features" {
                    return if value == "nix-command" {
                        needs_command
                    } else {
                        !has_flakes
                    };
                }
                !actual.iter().any(|entry| {
                    entry.as_str().is_some_and(|entry| {
                        entry == value || (name == "substituters" && entry.trim_end_matches('/') == value.trim_end_matches('/'))
                    })
                })
            })
            .collect();
        if !missing.is_empty() {
            additions.push_str(&format!("extra-{name} = {}\n", missing.join(" ")));
        }
    }
    Ok(additions)
}

impl InstallationOwner {
    pub(super) async fn cache_plan(&self) -> Result<Option<CachePlan>, TransactionError> {
        let mut config = self
            .observe(&self.effects, WorkerRequest::CacheSettings(false))
            .await?;
        let needs_command = config.exit_code != Some(0);
        if needs_command {
            config = self
                .observe(&self.effects, WorkerRequest::CacheSettings(true))
                .await?;
        }
        if config.exit_code != Some(0) || config.failure.is_some() {
            return Err(manual());
        }
        let flakes = self
            .observe(&self.effects, WorkerRequest::FlakeFeature)
            .await?;
        let has_flakes = flakes.exit_code == Some(0) && flakes.stdout == b"true\n";
        CachePlan::inspect(missing_settings(&config.stdout, needs_command, has_flakes)?)
    }
}
