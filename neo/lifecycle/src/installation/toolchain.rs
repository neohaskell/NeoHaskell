use super::{Effects, InstallationOwner, Layout, RootLease, TransactionError};
use crate::candidate::{OutputPath, SelectedOutputs};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

pub(super) const PAIR_MEMBER: &str = "share/neohaskell/toolchain";
const NAME: &str = "neo-toolchain";
const ROOT_PREFIX: &str = "toolchain-";
const NIX_HASH_LENGTH: usize = 32;

pub(super) fn system_for_target(target: &'static str) -> Option<&'static str> {
    match target {
        "aarch64-apple-darwin" => Some("aarch64-darwin"),
        "aarch64-unknown-linux-gnu" => Some("aarch64-linux"),
        "x86_64-unknown-linux-gnu" => Some("x86_64-linux"),
        _ => None,
    }
}

pub(super) fn release_acquisition(
    system: &'static str,
    pair: &OutputPath,
    toolchain: &OutputPath,
) -> TransactionError {
    TransactionError::ReleaseAcquisition {
        system,
        pair: pair.as_str().into(),
        toolchain: toolchain.as_str().into(),
    }
}

pub(super) struct RootNames<'a> {
    hash: &'a str,
}

impl<'a> RootNames<'a> {
    pub(super) fn new(output: &'a OutputPath) -> Option<Self> {
        let (hash, name) = output
            .as_str()
            .strip_prefix("/nix/store/")?
            .split_once('-')?;
        (name == NAME && nix_hash(hash)).then_some(Self { hash })
    }

    pub(super) fn registered(&self) -> String {
        format!("{ROOT_PREFIX}{}", self.hash)
    }

    pub(super) fn staged(&self) -> String {
        format!("{}-preparing", self.registered())
    }
}

fn nix_hash(value: &str) -> bool {
    value.len() == NIX_HASH_LENGTH
        && value
            .bytes()
            .all(|byte| b"0123456789abcdfghijklmnpqrsvwxyz".contains(&byte))
}

pub(super) fn is_registered_root(name: &str, target: &str) -> bool {
    let Some(hash) = name.strip_prefix(ROOT_PREFIX).filter(|hash| nix_hash(hash)) else {
        return false;
    };
    target == format!("/nix/store/{hash}-{NAME}")
}

fn pair_toolchain(pair: &Path) -> Option<OutputPath> {
    let target = fs::read_link(pair.join(PAIR_MEMBER)).ok()?;
    let output = OutputPath::parse(target.to_str()?).ok()?;
    RootNames::new(&output)?;
    Some(output)
}

fn active_toolchain(layout: &Layout) -> Option<OutputPath> {
    let pair = super::read_owned_link(&layout.active_link()).ok()??;
    OutputPath::parse(pair.to_str()?).ok()?;
    let executable = std::env::current_exe().ok()?.canonicalize().ok()?;
    if layout.active_link().join("bin/neo").canonicalize().ok()? != executable {
        return None;
    }
    pair_toolchain(&pair)
}

pub(crate) fn installed_project_toolchain() -> Option<PathBuf> {
    let layout = Layout::user_paths().ok()?;
    layout.validate().ok()?;
    Some(PathBuf::from(active_toolchain(&layout)?.as_str()))
}

pub(crate) fn ensure_project_toolchain(
    expected: &str,
    framework_pin: Option<&str>,
) -> Result<Option<PathBuf>, String> {
    let identity = crate::identity::BuildIdentity::current();
    let Some(system) = system_for_target(identity.target) else {
        return Ok(None);
    };
    let Some(layout) = Layout::user_paths()
        .ok()
        .filter(|layout| layout.validate().is_ok())
    else {
        return Ok(None);
    };
    if active_toolchain(&layout).is_none() {
        return Ok(None);
    }
    let output = OutputPath::parse(expected)
        .ok()
        .filter(|output| RootNames::new(output).is_some())
        .ok_or_else(|| format!("Invalid NeoHaskell toolchain output {expected:?} for {system}."))?;
    let pin = framework_pin.and_then(FrameworkPin::parse);
    let result = RootLease::wait(&layout.lock()).and_then(|lease| {
        Effects::with_project_lease(layout, lease, async |owner| {
            owner
                .ensure_toolchain(&output, pin, identity.target, system)
                .await
        })
    });
    result.map_err(|error| {
        format!(
            "Could not retain the prebuilt NeoHaskell toolchain for {system} at {}. {error:?}",
            output.as_str()
        )
    })
}

enum FrameworkPin<'a> {
    Version(&'a str),
    Revision(&'a str),
}

impl<'a> FrameworkPin<'a> {
    fn parse(value: &'a str) -> Option<Self> {
        if value.len() == 40
            && value
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
        {
            return Some(Self::Revision(value));
        }
        let version = value.strip_prefix('v').unwrap_or(value);
        let parsed = semver::Version::parse(version).ok()?;
        (parsed.pre.is_empty() && parsed.build.is_empty() && parsed.to_string() == version)
            .then_some(Self::Version(version))
    }
}

impl InstallationOwner {
    async fn ensure_toolchain(
        &self,
        output: &OutputPath,
        pin: Option<FrameworkPin<'_>>,
        target: &'static str,
        system: &str,
    ) -> Result<Option<PathBuf>, TransactionError> {
        let mut record = self.read_record().await?;
        if record.uninstall.is_some() || record.pending_nix_policy.is_some() {
            return Err(TransactionError::InvalidState(
                "installation recovery is pending",
            ));
        }
        let Some(receipt) = &record.committed else {
            return Ok(None);
        };
        if self.read_active_target().await?.as_ref() != Some(&receipt.active.output) {
            return Err(TransactionError::ForeignActiveTarget);
        }
        let path = PathBuf::from(output.as_str());
        let names = RootNames::new(output)
            .ok_or(TransactionError::Verification("toolchain output name"))?;
        if path.is_dir()
            && self
                .observed_resource(&record, &names.registered(), output)
                .await?
        {
            return Ok(Some(path));
        }
        let can_register = record.activation.is_none()
            && !record.shell.as_ref().is_some_and(super::shell::Ledger::pending);
        let mut selection = None;
        for pair in std::iter::once(&receipt.active).chain(receipt.previous.iter()) {
            if pair_toolchain(&pair.output).as_ref() != Some(output) {
                continue;
            }
            if let Some(pinned) = &pair.selection {
                let pair_path = pair
                    .output
                    .to_str()
                    .and_then(|path| OutputPath::parse(path).ok())
                    .ok_or(TransactionError::InvalidPair)?;
                let root = format!("candidate-{}", pinned.candidate_id);
                if path.is_dir() && self.observed_resource(&record, &root, &pair_path).await? {
                    if can_register {
                        self.effects.admit_project_retention()?;
                        self.retain_release_toolchain(output).await?;
                    }
                    return Ok(Some(path));
                }
                selection = Some(pinned.clone());
                break;
            }
        }
        if !can_register {
            return Err(TransactionError::InvalidState(
                "installation recovery is pending. Run `neo update` to resume it.",
            ));
        }
        let source = super::release::ReleaseSource::production(target, &self.effects)
            .map_err(|error| error.transaction("project toolchain release transport"))?;
        let selection = match (selection, pin) {
            (Some(selection), _) => Ok(selection),
            (None, Some(FrameworkPin::Version(version))) => source.select_exact(version).await,
            (None, Some(FrameworkPin::Revision(revision))) => {
                source.select_framework(revision).await
            }
            (None, None) => return Ok(None),
        };
        let selection = match selection {
            Ok(selection) => selection,
            Err(
                super::release::ReleaseError::Missing
                | super::release::ReleaseError::NoCompleteRelease,
            ) => return Ok(None),
            Err(error) => return Err(error.transaction("project toolchain release")),
        };
        let prepared = self
            .prepare_snapshot(&selection, &source, target, false)
            .await?;
        let selected = prepared
            .candidate
            .selected(system)
            .map_err(|_| TransactionError::Verification("project toolchain system"))?;
        let Some(published) = selected.output("toolchain") else {
            return Ok(None);
        };
        if published != output {
            return Err(TransactionError::Verification(
                "project pin differs from the published toolchain",
            ));
        }

        self.verify_remote(&self.effects, &prepared.candidate, system, false)
            .await?;
        self.effects.admit_project_retention()?;
        let directory = self
            .acquire_project_toolchain(&mut record, &names, output)
            .await?;
        let closure = toolchain_closure(selected, output)?;
        self.metadata_observations(&self.effects, None, &closure)
            .await?;
        self.successful_observation(
            &self.effects,
            super::process::WorkerRequest::VerifyContents {
                pair: output.as_str().into(),
            },
        )
        .await?;
        self.register_project_toolchain(&mut record, &directory, &names, output)
            .await?;
        Ok(Some(path))
    }
}

fn toolchain_closure<'a>(
    selected: &'a SelectedOutputs,
    output: &'a OutputPath,
) -> Result<Vec<(&'a OutputPath, &'a crate::candidate::NarMetadata)>, TransactionError> {
    let all = selected.closure();
    let mut seen = BTreeSet::new();
    let mut pending = vec![output];
    let mut closure = Vec::new();
    while let Some(path) = pending.pop() {
        if seen.insert(path) {
            let metadata = all
                .get(path)
                .ok_or(TransactionError::Verification("project toolchain closure"))?;
            closure.push((path, metadata));
            pending.extend(metadata.references());
        }
    }
    Ok(closure)
}
