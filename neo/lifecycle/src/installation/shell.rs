mod removal;
pub(super) use removal::RemovalPlan;

use super::{
    FilesystemJob, FilesystemResult, InstallationOwner, Layout, SettlementKind, TransactionError,
};
use fs2::FileExt;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::os::fd::{AsRawFd, FromRawFd};
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::path::{Path, PathBuf};
use std::sync::Arc;

const MAX_PROFILE: u64 = 1024 * 1024;
const CLAIM: &str = ".neo-shell-owner";

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Identity {
    device: u64,
    inode: u64,
    uid: u32,
}

impl Identity {
    fn of(file: &File) -> Result<Self, TransactionError> {
        let m = file.metadata().map_err(|_| TransactionError::Io)?;
        Ok(Self {
            device: m.dev(),
            inode: m.ino(),
            uid: m.uid(),
        })
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Claim {
    home: PathBuf,
    home_identity: Identity,
    state: PathBuf,
    state_identity: Identity,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Receipt {
    name: String,
    identity: Identity,
    prefix_length: u64,
    prefix_digest: String,
    block: String,
    staged: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Ledger {
    claim: Claim,
    receipts: Vec<Receipt>,
    pending: Option<Receipt>,
}

impl Ledger {
    pub(super) fn pending(&self) -> bool {
        self.pending.is_some()
    }
    pub(super) fn validate(&self, layout: &Layout) -> Result<(), TransactionError> {
        if !self.claim.home.is_absolute()
            || self.claim.home.as_os_str().len() > 4096
            || !self.claim.state.is_absolute()
            || self.claim.state.as_os_str().len() > 4096
            || self.receipts.len() + usize::from(self.pending.is_some()) > 4
        {
            return Err(TransactionError::InvalidState("shell ownership shape"));
        }
        let mut names = std::collections::BTreeSet::new();
        for receipt in self.receipts.iter().chain(self.pending.iter()) {
            if !matches!(
                receipt.name.as_str(),
                ".bash_profile" | ".bash_login" | ".profile" | ".bashrc" | ".zprofile" | ".zshrc"
            ) || !names.insert(&receipt.name)
                || receipt.prefix_length > MAX_PROFILE
                || receipt
                    .prefix_length
                    .checked_add(receipt.block.len() as u64)
                    .is_none_or(|length| length > MAX_PROFILE)
                || receipt.prefix_digest.len() != 64
                || !receipt.prefix_digest.bytes().all(|b| b.is_ascii_hexdigit())
                || receipt.identity.uid != unsafe { libc::geteuid() }
                || (receipt.staged.is_some()
                    && (receipt.prefix_length != 0 || receipt.prefix_digest != digest(&[])))
                || receipt.block != path_block(&layout.command_directory)?
                || receipt.block.len() > 8192
                || receipt.block.is_empty()
                || receipt.staged.as_ref().is_some_and(|s| {
                    !s.starts_with("stage-")
                        || s.len() > 100
                        || !s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-')
                })
            {
                return Err(TransactionError::InvalidState("shell receipt shape"));
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct ProfilePreimage {
    identity: Identity,
    length: u64,
    digest: String,
}

pub(super) fn inspect_profile(
    home: &Path,
    name: &str,
) -> Result<Option<ProfilePreimage>, TransactionError> {
    let parent = directory(home)?;
    let file = match at(&parent, name, libc::O_RDONLY, 0) {
        Ok(file) => file,
        Err(error) => {
            return match fs::symlink_metadata(home.join(name)) {
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
                _ => Err(error),
            };
        }
    };
    if file.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1 {
        return Err(TransactionError::UnsafePath);
    }
    let value = bytes(&file)?;
    Ok(Some(ProfilePreimage {
        identity: Identity::of(&file)?,
        length: value.len() as u64,
        digest: digest(&value),
    }))
}

pub(super) struct ShellSetup {
    home: PathBuf,
    shell: super::process::shell_startup::ShellKind,
    environment: std::collections::BTreeMap<String, String>,
    profiles: Vec<(String, Option<ProfilePreimage>)>,
    selected: Vec<String>,
}

impl ShellSetup {
    pub(super) fn context() -> Result<Self, TransactionError> {
        use super::process::shell_startup::ShellKind;
        let home = std::env::var_os("HOME")
            .map(PathBuf::from)
            .ok_or(TransactionError::UnsafePath)?;
        directory(&home)?;
        let shell = match std::env::var("SHELL").as_deref() {
            Ok("/bin/bash") => ShellKind::Bash,
            Ok("/bin/zsh") => ShellKind::Zsh,
            _ => return Err(TransactionError::ShellUnsupported),
        };
        if matches!(shell, ShellKind::Zsh) && std::env::var_os("ZDOTDIR").is_some() {
            return Err(TransactionError::ShellZdotdirUnsupported);
        }
        let mut environment = std::collections::BTreeMap::new();
        for name in [
            "PATH", "TERM", "LANG", "LC_ALL", "LC_CTYPE", "USER", "LOGNAME", "SHELL",
        ] {
            if let Some(value) = std::env::var_os(name) {
                let value = value
                    .into_string()
                    .map_err(|_| TransactionError::Verification("shell environment encoding"))?;
                if value.len() > 16384 || value.contains('\0') {
                    return Err(TransactionError::Verification("shell environment limit"));
                }
                environment.insert(name.into(), value);
            }
        }
        Ok(Self {
            home,
            shell,
            environment,
            profiles: Vec::new(),
            selected: Vec::new(),
        })
    }
}

pub(super) struct HomeLease {
    home: File,
    directory: File,
    _lock: File,
    claim: Claim,
}

fn directory(path: &Path) -> Result<File, TransactionError> {
    super::validate_directory(path, false)?;
    OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(path)
        .map_err(|_| TransactionError::UnsafePath)
}

fn at(parent: &File, name: &str, flags: i32, mode: u32) -> Result<File, TransactionError> {
    let name = std::ffi::CString::new(name).map_err(|_| TransactionError::UnsafePath)?;
    let fd = unsafe {
        libc::openat(
            parent.as_raw_fd(),
            name.as_ptr(),
            flags | libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_NONBLOCK,
            mode as libc::c_uint,
        )
    };
    if fd < 0 {
        return Err(TransactionError::Io);
    }
    let file = unsafe { File::from_raw_fd(fd) };
    let m = file.metadata().map_err(|_| TransactionError::Io)?;
    if (!m.is_file() && !m.is_dir())
        || m.uid() != unsafe { libc::geteuid() }
        || m.mode() & 0o022 != 0
    {
        return Err(TransactionError::UnsafePath);
    }
    Ok(file)
}

fn bytes(file: &File) -> Result<Vec<u8>, TransactionError> {
    let m = file.metadata().map_err(|_| TransactionError::Io)?;
    if !m.is_file() || m.len() > MAX_PROFILE || m.nlink() > 2 {
        return Err(TransactionError::UnsafePath);
    }
    let mut value = Vec::new();
    let reader = file;
    reader
        .take(MAX_PROFILE + 1)
        .read_to_end(&mut value)
        .map_err(|_| TransactionError::Io)?;
    if value.len() as u64 > MAX_PROFILE {
        return Err(TransactionError::UnsafePath);
    }
    Ok(value)
}

fn digest(value: &[u8]) -> String {
    format!("{:x}", Sha256::digest(value))
}

fn path_block(command_directory: &Path) -> Result<String, TransactionError> {
    let path = command_directory
        .to_str()
        .ok_or(TransactionError::UnsafePath)?;
    if !command_directory.is_absolute() || path.len() > 4096 || path.contains([':', '\n', '\r']) {
        return Err(TransactionError::UnsafePath);
    }
    let quoted = format!("'{}'", path.replace('\'', "'\\''"));
    Ok(format!(
        "\n# BEGIN NeoHaskell managed PATH\ncase \":${{PATH-}}:\" in\n  *:{quoted}:*) ;;\n  *) export PATH=\"${{PATH:+$PATH:}}\"{quoted} ;;\nesac\n# END NeoHaskell managed PATH\n"
    ))
}

impl HomeLease {
    pub(super) fn acquire(home: PathBuf, layout: &Layout) -> Result<Arc<Self>, TransactionError> {
        let home_file = directory(&home)?;
        let state = directory(&layout.state_directory)?;
        let claim = Claim {
            home,
            home_identity: Identity::of(&home_file)?,
            state: layout.state_directory.clone(),
            state_identity: Identity::of(&state)?,
        };
        let name = std::ffi::CString::new(CLAIM).unwrap();
        let created = unsafe { libc::mkdirat(home_file.as_raw_fd(), name.as_ptr(), 0o700) };
        if created != 0 && std::io::Error::last_os_error().raw_os_error() != Some(libc::EEXIST) {
            return Err(TransactionError::Io);
        }
        home_file.sync_all().map_err(|_| TransactionError::Io)?;
        let dir = at(&home_file, CLAIM, libc::O_RDONLY | libc::O_DIRECTORY, 0)?;
        if dir.metadata().map_err(|_| TransactionError::Io)?.mode() & 0o077 != 0 {
            return Err(TransactionError::UnsafePath);
        }
        let lock = at(&dir, "lock", libc::O_RDWR | libc::O_CREAT, 0o600)?;
        if lock.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1 {
            return Err(TransactionError::UnsafePath);
        }
        lock.try_lock_exclusive()
            .map_err(|_| TransactionError::Busy)?;
        let encoded = serde_json::to_vec(&claim).map_err(|_| TransactionError::Io)?;
        let existing = at(&dir, "owner", libc::O_RDONLY, 0);
        if existing.is_err() {
            let stage = format!("claim-{}", super::new_installation_id());
            let mut staged = at(
                &dir,
                &stage,
                libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL,
                0o600,
            )?;
            staged
                .write_all(&encoded)
                .map_err(|_| TransactionError::Io)?;
            staged.sync_all().map_err(|_| TransactionError::Io)?;
            dir.sync_all().map_err(|_| TransactionError::Io)?;
            let from = std::ffi::CString::new(stage).unwrap();
            let to = std::ffi::CString::new("owner").unwrap();
            #[cfg(target_os = "macos")]
            let published = unsafe {
                libc::renameatx_np(
                    dir.as_raw_fd(),
                    from.as_ptr(),
                    dir.as_raw_fd(),
                    to.as_ptr(),
                    libc::RENAME_EXCL,
                )
            };
            #[cfg(target_os = "linux")]
            let published = unsafe {
                libc::renameat2(
                    dir.as_raw_fd(),
                    from.as_ptr(),
                    dir.as_raw_fd(),
                    to.as_ptr(),
                    libc::RENAME_NOREPLACE,
                )
            };
            if published != 0
                && std::io::Error::last_os_error().raw_os_error() != Some(libc::EEXIST)
            {
                return Err(TransactionError::Io);
            }
            dir.sync_all().map_err(|_| TransactionError::Io)?;
        }
        let owner = at(&dir, "owner", libc::O_RDONLY, 0)?;
        if owner.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1
            || bytes(&owner)? != encoded
        {
            return Err(TransactionError::InvalidState(
                "HOME belongs to another state root or incomplete claim",
            ));
        }
        Ok(Arc::new(Self {
            home: home_file,
            directory: dir,
            _lock: lock,
            claim,
        }))
    }

    fn check(&self) -> Result<(), TransactionError> {
        if Identity::of(&directory(&self.claim.home)?)? != self.claim.home_identity {
            return Err(TransactionError::UnsafePath);
        }
        if Identity::of(&directory(&self.claim.state)?)? != self.claim.state_identity {
            return Err(TransactionError::UnsafePath);
        }
        let owner = at(&self.directory, "owner", libc::O_RDONLY, 0)?;
        if owner.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1
            || bytes(&owner)?
                != serde_json::to_vec(&self.claim).map_err(|_| TransactionError::Io)?
        {
            return Err(TransactionError::UnsafePath);
        }
        let current = at(&self.home, CLAIM, libc::O_RDONLY | libc::O_DIRECTORY, 0)?;
        if Identity::of(&current)? != Identity::of(&self.directory)? {
            return Err(TransactionError::UnsafePath);
        }
        Ok(())
    }

    pub(super) fn plan(&self, name: String, block: String) -> Result<Receipt, TransactionError> {
        self.check()?;
        let existing = match at(&self.home, &name, libc::O_RDONLY, 0) {
            Ok(file) => Some(file),
            Err(error) => {
                let path = self.claim.home.join(&name);
                match fs::symlink_metadata(path) {
                    Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
                    _ => return Err(error),
                }
            }
        };
        let (file, staged, prefix) = match existing {
            Some(file) => {
                if file.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1 {
                    return Err(TransactionError::UnsafePath);
                }
                let prefix = bytes(&file)?;
                if prefix.len() as u64 + block.len() as u64 > MAX_PROFILE {
                    return Err(TransactionError::InvalidState("shell profile size limit"));
                }
                if [
                    b"# BEGIN NeoHaskell managed PATH".as_slice(),
                    b"# END NeoHaskell managed PATH".as_slice(),
                ]
                .iter()
                .any(|marker| prefix.windows(marker.len()).any(|window| window == *marker))
                {
                    return Err(TransactionError::InvalidState(
                        "shell block has no ownership receipt",
                    ));
                }
                (file, None, prefix)
            }
            None => {
                let entries =
                    fs::read_dir(self.claim.home.join(CLAIM)).map_err(|_| TransactionError::Io)?;
                if entries.take(65).count() >= 64 {
                    return Err(TransactionError::InvalidState("shell staging limit"));
                }
                self.check()?;
                let stage = format!("stage-{}", super::new_installation_id());
                let mut file = at(
                    &self.directory,
                    &stage,
                    libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
                    0o600,
                )?;
                file.write_all(block.as_bytes())
                    .map_err(|_| TransactionError::Io)?;
                file.sync_all().map_err(|_| TransactionError::Io)?;
                self.directory
                    .sync_all()
                    .map_err(|_| TransactionError::Io)?;
                (file, Some(stage), Vec::new())
            }
        };
        Ok(Receipt {
            name,
            identity: Identity::of(&file)?,
            prefix_length: prefix.len() as u64,
            prefix_digest: digest(&prefix),
            block,
            staged,
        })
    }

    pub(super) fn verify(&self, receipt: &Receipt) -> Result<(), TransactionError> {
        self.check()?;
        let file = at(&self.home, &receipt.name, libc::O_RDONLY, 0)?;
        if file.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1
            || Identity::of(&file)? != receipt.identity
        {
            return Err(TransactionError::UnsafePath);
        }
        let value = bytes(&file)?;
        if value
            .windows(receipt.block.len())
            .filter(|window| *window == receipt.block.as_bytes())
            .count()
            != 1
        {
            return Err(TransactionError::InvalidState(
                "owned shell block is absent or duplicated",
            ));
        }
        self.check()
    }

    pub(super) fn apply(&self, receipt: &Receipt) -> Result<(), TransactionError> {
        self.check()?;
        if let Some(stage) = &receipt.staged {
            match at(&self.directory, stage, libc::O_RDONLY, 0) {
                Ok(staged) => {
                    if Identity::of(&staged)? != receipt.identity
                        || bytes(&staged)? != receipt.block.as_bytes()
                    {
                        return Err(TransactionError::UnsafePath);
                    }
                    let from = std::ffi::CString::new(stage.as_str()).unwrap();
                    let to = std::ffi::CString::new(receipt.name.as_str()).unwrap();
                    #[cfg(target_os = "macos")]
                    let published = unsafe {
                        libc::renameatx_np(
                            self.directory.as_raw_fd(),
                            from.as_ptr(),
                            self.home.as_raw_fd(),
                            to.as_ptr(),
                            libc::RENAME_EXCL,
                        )
                    };
                    #[cfg(target_os = "linux")]
                    let published = unsafe {
                        libc::renameat2(
                            self.directory.as_raw_fd(),
                            from.as_ptr(),
                            self.home.as_raw_fd(),
                            to.as_ptr(),
                            libc::RENAME_NOREPLACE,
                        )
                    };
                    if published != 0 {
                        return Err(TransactionError::InvalidState(
                            "shell publication conflicted",
                        ));
                    }
                    self.directory
                        .sync_all()
                        .map_err(|_| TransactionError::Io)?;
                }
                Err(_) => {
                    let active = at(&self.home, &receipt.name, libc::O_RDONLY, 0)?;
                    if Identity::of(&active)? != receipt.identity
                        || !bytes(&active)?.starts_with(receipt.block.as_bytes())
                    {
                        return Err(TransactionError::InvalidState(
                            "shell publication unrecognized",
                        ));
                    }
                }
            }
            self.home.sync_all().map_err(|_| TransactionError::Io)?;
        }
        let mut file = at(&self.home, &receipt.name, libc::O_RDWR | libc::O_APPEND, 0)?;
        if file.metadata().map_err(|_| TransactionError::Io)?.nlink() != 1
            || Identity::of(&file)? != receipt.identity
        {
            return Err(TransactionError::UnsafePath);
        }
        let value = bytes(&file)?;
        let start = receipt.prefix_length as usize;
        if value.len() < start || digest(&value[..start]) != receipt.prefix_digest {
            return Err(TransactionError::InvalidState("shell preimage changed"));
        }
        if value[start..].starts_with(receipt.block.as_bytes()) {
            file.sync_all().map_err(|_| TransactionError::Io)?;
        } else if value.len() == start && receipt.staged.is_none() {
            file.write_all(receipt.block.as_bytes())
                .map_err(|_| TransactionError::Io)?;
            file.sync_all().map_err(|_| TransactionError::Io)?;
        } else {
            return Err(TransactionError::InvalidState(
                "shell append incomplete or conflicted",
            ));
        }
        let active = at(&self.home, &receipt.name, libc::O_RDONLY, 0)?;
        if Identity::of(&active)? != receipt.identity
            || !bytes(&active)?
                .get(start..)
                .is_some_and(|s| s.starts_with(receipt.block.as_bytes()))
        {
            return Err(TransactionError::InvalidState(
                "shell active pathname changed",
            ));
        }
        self.check()
    }
}

impl InstallationOwner {
    pub(super) async fn recover_shell_rerun(&self) -> Result<(), TransactionError> {
        let raw = self.record_bytes().await?;
        let record = self.decode_record(raw.as_deref())?;
        let Some(ledger) = record.shell.filter(Ledger::pending) else {
            return Ok(());
        };
        if record.activation.is_some() || record.resources.pending.is_some() {
            return Err(TransactionError::InvalidState(
                "shell recovery overlaps another intent",
            ));
        }
        let home = std::env::var_os("HOME")
            .map(PathBuf::from)
            .ok_or(TransactionError::UnsafePath)?;
        self.recover_reviewed_shell_rerun(raw, ledger, home).await
    }

    async fn recover_reviewed_shell_rerun(
        &self,
        raw: Option<Vec<u8>>,
        ledger: Ledger,
        home: PathBuf,
    ) -> Result<(), TransactionError> {
        use super::coordinator::{RecoveryPending, ReviewOperation, ReviewSummary, ReviewedChange};
        if ledger.claim.home != home {
            return Err(TransactionError::UnsafePath);
        }
        let receipt = ledger
            .pending
            .as_ref()
            .ok_or(TransactionError::InvalidState(
                "shell recovery intent missing",
            ))?;
        let preimage = self.shell_preimage(&home, &receipt.name).await?;
        self.effects.recovery_pending(RecoveryPending::Shell);
        let review = self
            .effects
            .review(ReviewSummary {
                operation: ReviewOperation::Recover,
                version: None,
                recovery: RecoveryPending::Shell,
                changes: vec![ReviewedChange::Recovery, ReviewedChange::ShellPath],
            })
            .await?;
        if self.record_bytes().await? != raw
            || self.shell_preimage(&home, &receipt.name).await? != preimage
        {
            return Err(self.effects.review_changed());
        }
        self.effects.admit_reviewed(review)?;
        let lease = self.acquire_shell(home.clone()).await?;
        if self.record_bytes().await? != raw
            || self.shell_preimage(&home, &receipt.name).await? != preimage
        {
            return Err(self.effects.review_changed());
        }
        self.recover_shell(lease).await?;
        self.effects
            .completed(crate::diagnostics::CompletedStep::Recovery);
        Err(TransactionError::ShellRecoveryCompleted)
    }

    pub(super) async fn prepare_shell(&self) -> Result<ShellSetup, TransactionError> {
        self.prepare_shell_context(ShellSetup::context()?).await
    }

    async fn prepare_shell_context(
        &self,
        mut setup: ShellSetup,
    ) -> Result<ShellSetup, TransactionError> {
        use super::process::shell_startup::ShellKind;
        let names: &[&str] = match setup.shell {
            ShellKind::Bash => &[".bash_profile", ".bash_login", ".profile", ".bashrc"],
            ShellKind::Zsh => &[".zprofile", ".zshrc"],
        };
        for name in names {
            let observed = self.shell_preimage(&setup.home, name).await?;
            setup.profiles.push(((*name).into(), observed));
        }
        setup.selected = match setup.shell {
            ShellKind::Bash => vec![
                setup.profiles[..3]
                    .iter()
                    .find(|(_, p)| p.is_some())
                    .map(|(n, _)| n.clone())
                    .unwrap_or_else(|| ".bash_profile".into()),
                ".bashrc".into(),
            ],
            ShellKind::Zsh => vec![".zprofile".into(), ".zshrc".into()],
        };
        Ok(setup)
    }

    async fn shell_preimage(
        &self,
        home: &Path,
        name: &str,
    ) -> Result<Option<ProfilePreimage>, TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::ShellInspect {
                home: home.into(),
                name: name.into(),
            })
            .await?
        {
            FilesystemResult::ShellPreimage(value) => Ok(value),
            _ => Err(TransactionError::InvalidState(
                "shell preimage completion type",
            )),
        }
    }

    pub(super) async fn recheck_shell(&self, setup: &ShellSetup) -> Result<(), TransactionError> {
        for (name, expected) in &setup.profiles {
            if self.shell_preimage(&setup.home, name).await? != *expected {
                return Err(self.effects.review_changed());
            }
        }
        Ok(())
    }

    pub(super) async fn install_shell(
        &self,
        setup: &ShellSetup,
    ) -> Result<Arc<HomeLease>, TransactionError> {
        let lease = self.acquire_shell(setup.home.clone()).await?;
        self.recheck_shell(setup).await?;
        for name in &setup.selected {
            let expected = &setup
                .profiles
                .iter()
                .find(|(n, _)| n == name)
                .ok_or(TransactionError::InvalidState("shell selected preimage"))?
                .1;
            self.append_reviewed_shell(lease.clone(), name.clone(), Some(expected))
                .await?;
        }
        Ok(lease)
    }

    pub(super) async fn verify_shell_pair(
        &self,
        setup: &ShellSetup,
        pair: &super::PairIdentity,
    ) -> Result<(), TransactionError> {
        use super::process::shell_startup::{ShellStartupRequest, StartupMode};
        let mut environment = setup.environment.clone();
        if let Some(path) = environment.remove("PATH") {
            let filtered = std::env::join_paths(
                std::env::split_paths(&path)
                    .filter(|entry| entry != &self.layout.command_directory),
            )
            .map_err(|_| TransactionError::UnsafePath)?;
            if !filtered.is_empty() {
                environment.insert(
                    "PATH".to_owned(),
                    filtered
                        .into_string()
                        .map_err(|_| TransactionError::UnsafePath)?,
                );
            }
        }
        for mode in [
            StartupMode::Login,
            StartupMode::Interactive,
            StartupMode::InteractiveLogin,
        ] {
            let result = self
                .effects
                .process(super::process::WorkerRequest::ShellStartup(
                    ShellStartupRequest {
                        shell: setup.shell,
                        mode,
                        home: setup.home.clone(),
                        directory: setup.home.clone(),
                        environment: environment.clone(),
                        neo: self.layout.command_directory.join("neo"),
                        neoctl: self.layout.command_directory.join("neoctl"),
                        neo_version: format!("neo {}", pair.version),
                        neoctl_version: format!("neoctl {}", pair.version),
                    },
                ))
                .await?;
            if result.exit_code != Some(0) || result.failure.is_some() {
                return Err(TransactionError::ShellVerificationFailed);
            }
        }
        Ok(())
    }

    pub(super) async fn acquire_shell(
        &self,
        home: PathBuf,
    ) -> Result<Arc<HomeLease>, TransactionError> {
        match self
            .effects
            .filesystem(FilesystemJob::ShellAcquire {
                home,
                layout: self.layout.clone(),
            })
            .await?
        {
            FilesystemResult::ShellLease(lease) => Ok(lease),
            _ => Err(TransactionError::InvalidState(
                "shell lease completion type",
            )),
        }
    }

    #[cfg(test)]
    pub(super) async fn append_shell(
        &self,
        lease: Arc<HomeLease>,
        name: String,
    ) -> Result<(), TransactionError> {
        self.append_reviewed_shell(lease, name, None).await
    }

    async fn append_reviewed_shell(
        &self,
        lease: Arc<HomeLease>,
        name: String,
        expected: Option<&Option<ProfilePreimage>>,
    ) -> Result<(), TransactionError> {
        let block = path_block(&self.layout.command_directory)?;
        let mut record = self.read_record().await?;
        if record.activation.is_some()
            || record.resources.pending.is_some()
            || record.shell.as_ref().is_some_and(Ledger::pending)
        {
            return Err(TransactionError::InvalidState(
                "another transaction requires recovery",
            ));
        }
        let shell = record.shell.get_or_insert_with(|| Ledger {
            claim: lease.claim.clone(),
            receipts: Vec::new(),
            pending: None,
        });
        if shell.claim != lease.claim {
            return Err(TransactionError::UnsafePath);
        }
        if let Some(receipt) = shell.receipts.iter().find(|r| r.name == name) {
            if receipt.block != block {
                return Err(TransactionError::InvalidState("owned shell block differs"));
            }
            self.effects
                .filesystem(FilesystemJob::ShellVerify {
                    lease,
                    receipt: receipt.clone(),
                })
                .await?;
            return Ok(());
        }
        let validation = Receipt {
            name: name.clone(),
            identity: lease.claim.home_identity.clone(),
            prefix_length: 0,
            prefix_digest: digest(&[]),
            block: block.clone(),
            staged: None,
        };
        shell.pending = Some(validation);
        shell.validate(&self.layout)?;
        let FilesystemResult::ShellPlan(receipt) = self
            .effects
            .filesystem(FilesystemJob::ShellPlan {
                lease: lease.clone(),
                name,
                block,
            })
            .await?
        else {
            return Err(TransactionError::InvalidState("shell plan completion type"));
        };
        if let Some(expected) = expected {
            let matches = match expected {
                Some(preimage) => {
                    receipt.staged.is_none()
                        && receipt.identity == preimage.identity
                        && receipt.prefix_length == preimage.length
                        && receipt.prefix_digest == preimage.digest
                }
                None => receipt.staged.is_some(),
            };
            if !matches {
                return Err(self.effects.review_changed());
            }
        }
        shell.pending = Some(receipt);
        let publication = self.write_record(&record, false).await;
        #[cfg(test)]
        self.effects.hit("after_shell_intent");
        let settlement = match &publication {
            Ok(()) => self.effects.settlement(SettlementKind::Shell)?,
            Err(_) => {
                self.effects
                    .recover_failed_publication(SettlementKind::Shell, &self.layout)
                    .await?
            }
        };
        let result = self
            .with_effects(settlement.clone())
            .settle_shell(lease)
            .await;
        settlement.finish_settlement()?;
        publication?;
        result?;
        self.effects.checkpoint()
    }

    pub(super) async fn recover_shell(
        &self,
        lease: Arc<HomeLease>,
    ) -> Result<(), TransactionError> {
        let record = self.read_record().await?;
        let Some(shell) = record.shell else {
            return Ok(());
        };
        if shell.claim != lease.claim {
            return Err(TransactionError::UnsafePath);
        }
        let Some(receipt) = shell.pending else {
            return Ok(());
        };
        let settlement = self
            .effects
            .begin_reviewed_recovery(FilesystemJob::ShellApply {
                lease: lease.clone(),
                receipt,
            })
            .await?;
        let result = self
            .with_effects(settlement.clone())
            .settle_shell(lease)
            .await;
        settlement.finish_settlement()?;
        result
    }

    pub(super) async fn settle_shell(&self, lease: Arc<HomeLease>) -> Result<(), TransactionError> {
        let raw = self.record_bytes().await?;
        self.effects.check_recovery_preimage(raw.as_deref())?;
        let mut record = self.decode_record(raw.as_deref())?;
        let Some(shell) = &mut record.shell else {
            return Ok(());
        };
        if shell.claim != lease.claim {
            return Err(TransactionError::UnsafePath);
        }
        let Some(receipt) = shell.pending.clone() else {
            return Ok(());
        };
        self.effects
            .filesystem(FilesystemJob::ShellApply {
                lease,
                receipt: receipt.clone(),
            })
            .await?;
        shell.receipts.push(receipt);
        shell.pending = None;
        self.write_record(&record, false).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    fn fixture() -> (tempfile::TempDir, PathBuf, InstallationOwner) {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let home = root.join("home");
        let state = root.join("state");
        fs::create_dir(&home).unwrap();
        fs::create_dir(&state).unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        let owner = InstallationOwner::new(Layout::new(state, home.join("bin")));
        owner.effects.accept_fixture_review();
        (temp, home, owner)
    }

    #[test]
    fn shell_owner_schema_two_without_ledger_is_preserved() {
        let (_temp, _home, owner) = fixture();
        let original = serde_json::to_vec(&super::super::InstallationRecord::default()).unwrap();
        assert!(!String::from_utf8_lossy(&original).contains("shell"));
        let decoded = owner.decode_record(Some(&original)).unwrap();
        assert!(decoded.shell.is_none());
        assert_eq!(serde_json::to_vec(&decoded).unwrap(), original);
    }

    #[test]
    fn shell_owner_appends_and_preserves_inode() {
        let (_temp, home, owner) = fixture();
        let profile = home.join(".bashrc");
        fs::write(&profile, "export USER_VALUE=kept\n").unwrap();
        let before = fs::metadata(&profile).unwrap().ino();
        owner
            .with_fixture_lease(|| async {
                let lease = owner.acquire_shell(home.clone()).await?;
                owner.append_shell(lease, ".bashrc".into()).await
            })
            .unwrap();
        assert_eq!(fs::metadata(&profile).unwrap().ino(), before);
        assert!(
            fs::read_to_string(&profile)
                .unwrap()
                .starts_with("export USER_VALUE=kept\n")
        );
        let saved: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert_eq!(saved.shell.as_ref().unwrap().receipts.len(), 1);
        assert!(!saved.shell.unwrap().pending());
    }

    #[test]
    fn shell_owner_missing_file_retains_staged_identity() {
        let (_temp, home, owner) = fixture();
        owner
            .with_fixture_lease(|| async {
                let lease = owner.acquire_shell(home.clone()).await?;
                owner.append_shell(lease, ".zshrc".into()).await
            })
            .unwrap();
        let saved: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        let receipt = &saved.shell.as_ref().unwrap().receipts[0];
        assert_eq!(
            fs::metadata(home.join(".zshrc")).unwrap().ino(),
            receipt.identity.inode
        );
        assert_eq!(fs::metadata(home.join(".zshrc")).unwrap().nlink(), 1);
        assert!(
            !home
                .join(CLAIM)
                .join(receipt.staged.as_ref().unwrap())
                .exists()
        );
    }

    #[test]
    fn shell_owner_cross_xdg_refuses() {
        let (_temp, home, owner) = fixture();
        owner
            .with_fixture_lease(|| async { owner.acquire_shell(home.clone()).await.map(|_| ()) })
            .unwrap();
        let second = owner.layout.state_directory.with_file_name("state-two");
        fs::create_dir(&second).unwrap();
        fs::set_permissions(&second, fs::Permissions::from_mode(0o700)).unwrap();
        let other = InstallationOwner::new(Layout::new(second, home.join("bin")));
        other.effects.accept_fixture_review();
        assert!(
            other
                .with_fixture_lease(|| async {
                    other.acquire_shell(home.clone()).await.map(|_| ())
                })
                .is_err()
        );
        assert!(!home.join(".bashrc").exists());
    }

    #[test]
    fn shell_owner_recovers_published_intent_after_restart() {
        let (_temp, home, owner) = fixture();
        owner
            .with_fixture_lease(|| async {
                let lease = owner.acquire_shell(home.clone()).await?;
                let FilesystemResult::ShellPlan(receipt) = owner
                    .effects
                    .filesystem(FilesystemJob::ShellPlan {
                        lease: lease.clone(),
                        name: ".bashrc".into(),
                        block: path_block(&owner.layout.command_directory)?,
                    })
                    .await?
                else {
                    panic!("plan");
                };
                let mut record = owner.read_record().await?;
                record.shell = Some(Ledger {
                    claim: lease.claim.clone(),
                    receipts: Vec::new(),
                    pending: Some(receipt),
                });
                owner.write_record(&record, false).await
            })
            .unwrap();
        assert!(!home.join(".bashrc").exists());
        let restarted = InstallationOwner::new(owner.layout.clone());
        restarted.effects.accept_fixture_review();
        restarted
            .with_fixture_lease(|| async {
                let lease = restarted.acquire_shell(home.clone()).await?;
                restarted.recover_shell(lease).await
            })
            .unwrap();
        assert_eq!(
            fs::read_to_string(home.join(".bashrc")).unwrap(),
            path_block(&owner.layout.command_directory).unwrap()
        );
    }

    #[test]
    fn shell_owner_refuses_competing_file_after_intent() {
        let (_temp, home, owner) = fixture();
        owner
            .with_fixture_lease(|| async {
                let lease = owner.acquire_shell(home.clone()).await?;
                let FilesystemResult::ShellPlan(receipt) = owner
                    .effects
                    .filesystem(FilesystemJob::ShellPlan {
                        lease: lease.clone(),
                        name: ".bashrc".into(),
                        block: path_block(&owner.layout.command_directory)?,
                    })
                    .await?
                else {
                    panic!("plan");
                };
                let mut record = owner.read_record().await?;
                record.shell = Some(Ledger {
                    claim: lease.claim.clone(),
                    receipts: Vec::new(),
                    pending: Some(receipt),
                });
                owner.write_record(&record, false).await?;
                fs::write(home.join(".bashrc"), "editor bytes\n").unwrap();
                let settlement = owner.effects.settlement(SettlementKind::Shell)?;
                let result = owner
                    .with_effects(settlement.clone())
                    .settle_shell(lease)
                    .await;
                settlement.finish_settlement()?;
                assert!(result.is_err());
                Ok(())
            })
            .unwrap();
        assert_eq!(
            fs::read_to_string(home.join(".bashrc")).unwrap(),
            "editor bytes\n"
        );
        let saved: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(saved.shell.unwrap().pending());
    }
    #[test]
    fn shell_owner_rerun_verifies_without_duplicate() {
        let (_temp, home, owner) = fixture();
        let block = path_block(&owner.layout.command_directory).unwrap();
        for _ in 0..2 {
            owner
                .with_fixture_lease(|| async {
                    let lease = owner.acquire_shell(home.clone()).await?;
                    owner.append_shell(lease, ".bashrc".into()).await
                })
                .unwrap();
        }
        assert_eq!(fs::read_to_string(home.join(".bashrc")).unwrap(), block);
    }

    #[test]
    fn shell_owner_cancellation_after_intent_settles() {
        let (_temp, home, owner) = fixture();
        owner.effects.cancel_at("after_shell_intent");
        let result = owner.with_fixture_lease(|| async {
            let lease = owner.acquire_shell(home.clone()).await?;
            owner.append_shell(lease, ".bashrc".into()).await
        });
        assert_eq!(result, Err(TransactionError::StopRequested));
        let saved: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(!saved.shell.unwrap().pending());
        assert_eq!(
            fs::read_to_string(home.join(".bashrc")).unwrap(),
            path_block(&owner.layout.command_directory).unwrap()
        );
    }

    #[test]
    fn shell_owner_failed_publication_settles_and_preserves_error() {
        let (_temp, home, owner) = fixture();
        owner.effects.cancel_at("after_journal_publication_error");
        let result = owner.with_fixture_lease(|| async {
            let lease = owner.acquire_shell(home.clone()).await?;
            owner.append_shell(lease, ".bashrc".into()).await
        });
        assert_eq!(result, Err(TransactionError::Io));
        let saved: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(!saved.shell.unwrap().pending());
        assert_eq!(
            fs::read_to_string(home.join(".bashrc")).unwrap(),
            path_block(&owner.layout.command_directory).unwrap()
        );
    }

    fn context(
        home: PathBuf,
        shell: super::super::process::shell_startup::ShellKind,
    ) -> ShellSetup {
        ShellSetup {
            home,
            shell,
            environment: [
                ("PATH".into(), "/usr/bin:/bin".into()),
                ("TERM".into(), "dumb".into()),
            ]
            .into(),
            profiles: Vec::new(),
            selected: Vec::new(),
        }
    }

    #[test]
    fn shell_review_selects_effective_bash_login_and_rejects_drift() {
        use super::super::process::shell_startup::ShellKind;
        let (_temp, home, owner) = fixture();
        fs::write(home.join(".profile"), "export KEEP=present\n").unwrap();
        owner
            .with_fixture_lease(|| async {
                let setup = owner
                    .prepare_shell_context(context(home.clone(), ShellKind::Bash))
                    .await?;
                assert_eq!(setup.selected, [".profile", ".bashrc"]);
                fs::write(home.join(".bash_profile"), "export EDITOR=kept\n").unwrap();
                assert!(owner.recheck_shell(&setup).await.is_err());
                Ok(())
            })
            .unwrap();
        assert!(!home.join(CLAIM).exists());
        assert!(!owner.layout.record().exists());
    }

    #[test]
    fn shell_review_binds_profile_preimage_to_append() {
        use super::super::process::shell_startup::ShellKind;
        let (_temp, home, owner) = fixture();
        fs::write(home.join(".bashrc"), "export KEEP=present\n").unwrap();
        owner
            .with_fixture_lease(|| async {
                let setup = owner
                    .prepare_shell_context(context(home.clone(), ShellKind::Bash))
                    .await?;
                let lease = owner.acquire_shell(home.clone()).await?;
                fs::write(home.join(".bashrc"), "export EDITOR=kept\n").unwrap();
                let expected = &setup
                    .profiles
                    .iter()
                    .find(|(name, _)| name == ".bashrc")
                    .unwrap()
                    .1;
                assert!(
                    owner
                        .append_reviewed_shell(lease, ".bashrc".into(), Some(expected))
                        .await
                        .is_err()
                );
                Ok(())
            })
            .unwrap();
        assert_eq!(
            fs::read_to_string(home.join(".bashrc")).unwrap(),
            "export EDITOR=kept\n"
        );
        assert!(!owner.layout.record().exists());
    }

    #[test]
    fn shell_owner_install_rerun_through_normal_worker() {
        use super::super::process::shell_startup::ShellKind;
        for shell in [ShellKind::Bash, ShellKind::Zsh] {
            let executable = match shell {
                ShellKind::Bash => "/bin/bash",
                ShellKind::Zsh => "/bin/zsh",
            };
            if !Path::new(executable).exists() {
                continue;
            }
            let (_temp, home, owner) = fixture();
            fs::create_dir(&owner.layout.command_directory).unwrap();
            for tool in ["neo", "neoctl"] {
                let path = owner.layout.command_directory.join(tool);
                fs::write(&path, format!("#!/bin/sh\nprintf '{tool} 1.2.3\\n'\n")).unwrap();
                fs::set_permissions(path, fs::Permissions::from_mode(0o700)).unwrap();
            }
            let pair = super::super::PairIdentity {
                version: "1.2.3".into(),
                output: home.join("pair"),
                selection: None,
            };
            for _ in 0..2 {
                owner
                    .with_fixture_lease(|| async {
                        let setup = owner
                            .prepare_shell_context(context(home.clone(), shell))
                            .await?;
                        let _lease = owner.install_shell(&setup).await?;
                        owner.verify_shell_pair(&setup, &pair).await
                    })
                    .unwrap();
            }
            let record: super::super::InstallationRecord =
                serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
            assert_eq!(record.shell.unwrap().receipts.len(), 2);
        }
    }

    #[test]
    fn shell_owner_rejects_oversize_append_before_intent() {
        let (_temp, home, owner) = fixture();
        let original = vec![b'x'; MAX_PROFILE as usize];
        fs::write(home.join(".bashrc"), &original).unwrap();
        owner
            .with_fixture_lease(|| async {
                let lease = owner.acquire_shell(home.clone()).await?;
                assert!(owner.append_shell(lease, ".bashrc".into()).await.is_err());
                Ok(())
            })
            .unwrap();
        assert_eq!(fs::read(home.join(".bashrc")).unwrap(), original);
        assert!(!owner.layout.record().exists());
    }

    #[test]
    fn shell_rerun_recovers_with_one_review_and_requests_retry() {
        let (_temp, home, owner) = fixture();
        owner
            .with_fixture_lease(|| async {
                let lease = owner.acquire_shell(home.clone()).await?;
                let FilesystemResult::ShellPlan(receipt) = owner
                    .effects
                    .filesystem(FilesystemJob::ShellPlan {
                        lease: lease.clone(),
                        name: ".bashrc".into(),
                        block: path_block(&owner.layout.command_directory)?,
                    })
                    .await?
                else {
                    panic!("plan");
                };
                let mut record = owner.read_record().await?;
                record.shell = Some(Ledger {
                    claim: lease.claim.clone(),
                    receipts: Vec::new(),
                    pending: Some(receipt),
                });
                owner.write_record(&record, false).await
            })
            .unwrap();
        let restarted = InstallationOwner::new(owner.layout.clone());
        let lock = super::super::RootLease::acquire(&restarted.layout.lock()).unwrap();
        let result =
            super::super::coordinator::test_with_review(&restarted.effects, &lock, async {
                let raw = restarted.record_bytes().await?;
                let ledger = restarted.decode_record(raw.as_deref())?.shell.unwrap();
                restarted
                    .recover_reviewed_shell_rerun(raw, ledger, home.clone())
                    .await
            });
        assert_eq!(result, Err(TransactionError::ShellRecoveryCompleted));
        let record: super::super::InstallationRecord =
            serde_json::from_slice(&fs::read(owner.layout.record()).unwrap()).unwrap();
        assert!(!record.shell.unwrap().pending());
        assert!(record.committed.is_none());
        assert_eq!(
            fs::read_to_string(home.join(".bashrc")).unwrap(),
            path_block(&owner.layout.command_directory).unwrap()
        );
    }
}
