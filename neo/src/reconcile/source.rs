use std::fmt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use crate::errors::NeoError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FullSha(String);

impl FullSha {
    pub fn parse(value: &str) -> miette::Result<Self> {
        if value.len() != 40 || !value.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(miette::miette!(
                "expected a full 40-character Git commit SHA, got `{value}`"
            ));
        }
        Ok(Self(value.to_ascii_lowercase()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for FullSha {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl FromStr for FullSha {
    type Err = miette::Report;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::parse(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrameworkOrigin {
    Upstream,
    LocalGit { root: PathBuf },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrameworkSource {
    Commit {
        sha: FullSha,
        origin: FrameworkOrigin,
    },
    OfflinePlaceholder,
}

impl FrameworkSource {
    pub async fn resolve(version: &str) -> miette::Result<Self> {
        Self::resolve_with_local_source(version, local_source_path()?).await
    }

    async fn resolve_with_local_source(
        version: &str,
        local_source: Option<PathBuf>,
    ) -> miette::Result<Self> {
        if let Some(root) = local_source {
            let output = Command::new("git")
                .args([
                    "-C",
                    root.to_str().ok_or_else(|| {
                        miette::miette!(
                            "NEO_NEOHASKELL_SOURCE is not a valid UTF-8 path: `{}`",
                            root.display()
                        )
                    })?,
                    "rev-parse",
                    "HEAD",
                ])
                .output()
                .map_err(|error| {
                    NeoError::io_at("running git in NEO_NEOHASKELL_SOURCE at", &root, error)
                })?;
            if !output.status.success() {
                return Err(miette::miette!(
                    "NEO_NEOHASKELL_SOURCE is not a readable Git checkout: `{}`",
                    root.display()
                ));
            }
            let revision = String::from_utf8_lossy(&output.stdout).trim().to_string();
            let sha = FullSha::parse(&revision).map_err(|_| {
                miette::miette!(
                    "Git returned an invalid HEAD revision for `{}`",
                    root.display()
                )
            })?;
            return Ok(Self::Commit {
                sha,
                origin: FrameworkOrigin::LocalGit { root },
            });
        }

        if let Ok(sha) = FullSha::parse(version) {
            return Ok(Self::Commit {
                sha,
                origin: FrameworkOrigin::Upstream,
            });
        }

        if std::env::var_os("NEO_SKIP_NETWORK").is_some() {
            return Ok(Self::OfflinePlaceholder);
        }

        let sha = FullSha::parse(&crate::network::fetch_neo_sha(version).await?)?;
        Ok(Self::Commit {
            sha,
            origin: FrameworkOrigin::Upstream,
        })
    }

    pub fn pin(&self) -> &str {
        match self {
            Self::Commit { sha, .. } => sha.as_str(),
            Self::OfflinePlaceholder => "deadbeef",
        }
    }

    pub fn origin(&self) -> Option<&FrameworkOrigin> {
        match self {
            Self::Commit { origin, .. } => Some(origin),
            Self::OfflinePlaceholder => None,
        }
    }

    pub fn git_url(&self) -> String {
        match self.origin() {
            Some(FrameworkOrigin::LocalGit { root }) => {
                format!("file://{}", encode_file_path(root))
            }
            Some(FrameworkOrigin::Upstream) | None => {
                "https://github.com/neohaskell/neohaskell.git".to_string()
            }
        }
    }

    pub fn nix_source_url(&self) -> Option<String> {
        match self.origin() {
            Some(FrameworkOrigin::LocalGit { .. }) => {
                Some(format!("git+{}?rev={}", self.git_url(), self.pin()))
            }
            Some(FrameworkOrigin::Upstream) | None => None,
        }
    }
}

fn local_source_path() -> miette::Result<Option<PathBuf>> {
    let Some(raw) = std::env::var_os("NEO_NEOHASKELL_SOURCE").filter(|value| !value.is_empty())
    else {
        return Ok(None);
    };
    local_source_path_from(PathBuf::from(raw)).map(Some)
}

fn local_source_path_from(path: PathBuf) -> miette::Result<PathBuf> {
    if !path.is_absolute() {
        return Err(miette::miette!(
            "NEO_NEOHASKELL_SOURCE must be an absolute path, got `{}`",
            path.display()
        ));
    }
    let root = path
        .canonicalize()
        .map_err(|error| NeoError::io_at("resolving NEO_NEOHASKELL_SOURCE at", &path, error))?;
    Ok(root)
}

fn encode_file_path(path: &Path) -> String {
    let text = path.to_string_lossy();
    let mut encoded = String::with_capacity(text.len());
    for byte in text.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' | b'/' => {
                encoded.push(byte as char);
            }
            _ => encoded.push_str(&format!("%{byte:02X}")),
        }
    }
    encoded
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn git(path: &Path, args: &[&str]) -> String {
        let output = Command::new("git")
            .arg("-C")
            .arg(path)
            .args(args)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }

    #[test]
    fn full_sha_requires_forty_hex_digits_and_normalizes_case() {
        let sha = FullSha::parse("0123456789ABCDEF0123456789ABCDEF01234567").unwrap();
        assert_eq!(sha.as_str(), "0123456789abcdef0123456789abcdef01234567");
        assert!(FullSha::parse("deadbeef").is_err());
        assert!(FullSha::parse("0123456789abcdef0123456789abcdef0123456z").is_err());
    }

    #[test]
    fn local_source_build_urls_share_the_resolved_commit() {
        let root = PathBuf::from("/work/Neo Haskell");
        let sha = FullSha::parse("0123456789012345678901234567890123456789").unwrap();
        let source = FrameworkSource::Commit {
            sha,
            origin: FrameworkOrigin::LocalGit { root },
        };
        assert_eq!(source.pin(), "0123456789012345678901234567890123456789");
        assert_eq!(source.git_url(), "file:///work/Neo%20Haskell");
        assert_eq!(
            source.nix_source_url().as_deref(),
            Some("git+file:///work/Neo%20Haskell?rev=0123456789012345678901234567890123456789")
        );
    }

    #[tokio::test]
    async fn local_source_override_resolves_the_checkout_head() {
        let root = tempdir().unwrap();
        git(root.path(), &["init", "-q"]);
        git(
            root.path(),
            &["config", "user.email", "test@example.invalid"],
        );
        git(root.path(), &["config", "user.name", "Source Test"]);
        std::fs::write(root.path().join("README.md"), "committed\n").unwrap();
        git(root.path(), &["add", "README.md"]);
        git(root.path(), &["commit", "-qm", "source fixture"]);
        std::fs::write(root.path().join("README.md"), "dirty working tree\n").unwrap();
        let expected = FullSha::parse(&git(root.path(), &["rev-parse", "HEAD"])).unwrap();
        let resolved = FrameworkSource::resolve_with_local_source(
            "not-a-commit",
            Some(local_source_path_from(root.path().to_path_buf()).unwrap()),
        )
        .await
        .unwrap();
        assert_eq!(
            resolved,
            FrameworkSource::Commit {
                sha: expected,
                origin: FrameworkOrigin::LocalGit {
                    root: root.path().canonicalize().unwrap(),
                },
            }
        );
    }
}
