use super::super::{InputIdentity, MAX_CONFIGURATION_BYTES, POLICY_LEAF, POLICY_STAGE};
use super::VendorPublicationRequest;
use super::configuration::{
    VendorApplication, observe_remaining_inputs, protect_configuration_path,
};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use sha2::{Digest, Sha256};
use std::ffi::{CStr, CString};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Write};
use std::mem::MaybeUninit;
use std::os::fd::{AsRawFd, FromRawFd};
use std::os::unix::fs::{FileExt, MetadataExt, OpenOptionsExt};
use std::path::Path;
use std::ptr::null_mut;

const DIRECTORY: &str = "/private/etc/nix";
const CUSTOM: &CStr = c"nix.custom.conf";
const STAGED_CUSTOM: &CStr = c"main-slot";
const STAGED_LEAF: &CStr = c"leaf-slot";
const RECEIPT: &CStr = c"receipt.json";
const RESTART: &CStr = c"restart.json";
const RECEIPT_SCHEMA: u8 = 2;
const RESTART_SCHEMA: u8 = 1;
const MAX_RECORD_BYTES: usize = 4096;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::installation) enum PublicationOutcome {
    Refused,
    Pending,
    Published,
}

pub(super) struct LockedVendorDirectory {
    directory: File,
}

pub(super) enum RecoveredPublication {
    Prepared,
    LeafPublished,
    Published(PublishedPolicy),
}

#[derive(PartialEq, Eq)]
pub(super) struct PublishedPolicy {
    pub(super) fingerprint: [u8; 32],
    pub(super) restart_from: Option<[u8; 32]>,
}

impl LockedVendorDirectory {
    pub(super) fn lock() -> io::Result<Self> {
        if unsafe { libc::getuid() != 0 || libc::geteuid() != 0 } {
            return Err(changed());
        }
        let path = Path::new(DIRECTORY);
        if protect_configuration_path(path)? != path {
            return Err(changed());
        }
        let directory = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_DIRECTORY)
            .open(path)?;
        check_syscall(unsafe {
            libc::flock(directory.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB)
        })?;
        let locked = Self { directory };
        locked.check()?;
        Ok(locked)
    }

    fn check(&self) -> io::Result<()> {
        let path = Path::new(DIRECTORY);
        let named = fs::symlink_metadata(path)?;
        let opened = self.directory.metadata()?;
        if !opened.is_dir()
            || InputIdentity::of(&named) != InputIdentity::of(&opened)
            || protect_configuration_path(path)? != path
        {
            return Err(changed());
        }
        Ok(())
    }

    pub(super) fn publish(
        &self,
        application: &VendorApplication,
        request: &VendorPublicationRequest,
        daemon: &[u8; 32],
        authorize: impl FnOnce() -> bool,
    ) -> PublicationOutcome {
        let stage_name = CString::new(POLICY_STAGE).expect("fixed publication stage");
        let leaf_name = CString::new(POLICY_LEAF).expect("fixed policy leaf");
        let preflight = || {
            self.check()?;
            require_absent(&self.directory, &stage_name)?;
            require_absent(&self.directory, &leaf_name)?;
            require(application.source_inputs_current())
        };
        let baseline = (|| {
            preflight()?;
            let original = open_at(&self.directory, CUSTOM, libc::O_RDONLY, 0)?;
            let snapshot = FileSnapshot::capture(&original)?;
            require(
                &snapshot.identity == application.custom_identity()
                    && snapshot.digest == application.desired().observed_digest,
            )?;
            require(authorize())?;
            preflight()?;
            snapshot.check_named(&self.directory, CUSTOM, false)?;
            Ok::<_, io::Error>((original, snapshot))
        })();
        let Ok((original, snapshot)) = baseline else {
            return PublicationOutcome::Refused;
        };

        if unsafe { libc::mkdirat(self.directory.as_raw_fd(), stage_name.as_ptr(), 0o700) } != 0 {
            return if io::Error::last_os_error().kind() == io::ErrorKind::AlreadyExists
                || require_absent(&self.directory, &stage_name).is_ok()
            {
                PublicationOutcome::Refused
            } else {
                PublicationOutcome::Pending
            };
        }

        let publish = || {
            let stage = open_at(
                &self.directory,
                &stage_name,
                libc::O_RDONLY | libc::O_DIRECTORY,
                0,
            )?;
            let check_directories = || {
                self.check()?;
                check_stage(&self.directory, &stage_name, &stage)
            };
            check_directories()?;
            let desired = application.desired();
            let source = create_file(
                &stage,
                STAGED_CUSTOM,
                &desired.source,
                snapshot.identity.gid,
                snapshot.identity.mode & 0o777,
                Some(&original),
            )?;
            require(source.attributes == snapshot.attributes)?;
            let leaf = create_file(&stage, STAGED_LEAF, &desired.leaf, 0, 0o644, None)?;
            let receipt = PublicationReceipt {
                schema: RECEIPT_SCHEMA,
                attempt: request.attempt,
                reviewed_plan: request.reviewed.0,
                daemon: *daemon,
                unchanged_inputs: application.remaining_inputs_commitment(),
                original: snapshot,
                source,
                leaf,
            };
            let marker = write_record(&stage, RECEIPT, &receipt)?;
            sync_directories(&stage, &self.directory)?;

            let check_source = || {
                check_directories()?;
                marker.check_named(&stage, RECEIPT, false)?;
                require(application.source_inputs_current())?;
                receipt
                    .original
                    .check_named(&self.directory, CUSTOM, false)?;
                receipt.source.check_named(&stage, STAGED_CUSTOM, false)
            };
            check_source()?;
            receipt.leaf.check_named(&stage, STAGED_LEAF, false)?;
            rename_at(
                &stage,
                STAGED_LEAF,
                &self.directory,
                &leaf_name,
                libc::RENAME_EXCL,
            )?;
            sync_directories(&stage, &self.directory)?;

            check_source()?;
            require_absent(&stage, STAGED_LEAF)?;
            receipt
                .leaf
                .check_named(&self.directory, &leaf_name, true)?;
            rename_at(
                &stage,
                STAGED_CUSTOM,
                &self.directory,
                CUSTOM,
                libc::RENAME_SWAP,
            )?;
            sync_directories(&stage, &self.directory)?;
            check_directories()?;
            marker.check_named(&stage, RECEIPT, false)?;
            require_absent(&stage, STAGED_LEAF)?;
            receipt.source.check_named(&self.directory, CUSTOM, true)?;
            receipt
                .leaf
                .check_named(&self.directory, &leaf_name, true)?;
            require(application.remaining_inputs_current())?;
            receipt.original.check_named(&stage, STAGED_CUSTOM, true)
        };
        if publish().is_ok() {
            PublicationOutcome::Published
        } else {
            PublicationOutcome::Pending
        }
    }

    pub(super) fn recover(
        &self,
        request: &VendorPublicationRequest,
    ) -> io::Result<RecoveredPublication> {
        let stage = self.open_stage()?;
        self.recover_stage(request, &stage)
    }

    pub(super) fn mark_restart(
        &self,
        request: &VendorPublicationRequest,
        expected: &PublishedPolicy,
        live: [u8; 32],
    ) -> io::Result<()> {
        require(live != [0; 32] && expected.restart_from.is_none())?;
        let stage = self.open_stage()?;
        let RecoveredPublication::Published(current) = self.recover_stage(request, &stage)? else {
            return Err(changed());
        };
        require(current == *expected)?;
        let marker = write_record(
            &stage,
            RESTART,
            &RestartReceipt {
                schema: RESTART_SCHEMA,
                attempt: request.attempt,
                reviewed_plan: request.reviewed.0,
                publication: expected.fingerprint,
                previous: live,
            },
        )?;
        sync_directories(&stage, &self.directory)?;
        require(matches!(
            self.recover_stage(request, &stage)?,
            RecoveredPublication::Published(current)
                if current.fingerprint == expected.fingerprint && current.restart_from == Some(live)
        ))?;
        marker.check_named(&stage, RESTART, false)
    }

    fn open_stage(&self) -> io::Result<File> {
        self.check()?;
        let name = CString::new(POLICY_STAGE).expect("fixed publication stage");
        let stage = open_at(
            &self.directory,
            &name,
            libc::O_RDONLY | libc::O_DIRECTORY,
            0,
        )?;
        check_stage(&self.directory, &name, &stage)?;
        Ok(stage)
    }

    fn recover_stage(
        &self,
        request: &VendorPublicationRequest,
        stage: &File,
    ) -> io::Result<RecoveredPublication> {
        let stage_name = CString::new(POLICY_STAGE).expect("fixed publication stage");
        let stage_identity = InputIdentity::of(&stage.metadata()?);
        let (receipt, marker) = read_record::<PublicationReceipt>(stage, RECEIPT)?;
        require(
            receipt.schema == RECEIPT_SCHEMA
                && receipt.attempt == request.attempt
                && receipt.reviewed_plan == request.reviewed.0
                && receipt.daemon != [0; 32],
        )?;
        let check_unchanged_inputs = || {
            require(
                observe_remaining_inputs().is_ok_and(|actual| actual == receipt.unchanged_inputs),
            )
        };
        check_unchanged_inputs()?;
        let files = PublicationFiles::read(&self.directory, stage)?;
        let placement = files.placement(&receipt).ok_or_else(changed)?;
        let restart = match read_record::<RestartReceipt>(stage, RESTART) {
            Ok(found) => Some(found),
            Err(error) if error.kind() == io::ErrorKind::NotFound => None,
            Err(error) => return Err(error),
        };
        let recovered = match placement {
            PublicationPlacement::Prepared if restart.is_none() => RecoveredPublication::Prepared,
            PublicationPlacement::LeafPublished if restart.is_none() => {
                RecoveredPublication::LeafPublished
            }
            PublicationPlacement::Published => {
                let fingerprint = files.fingerprint(&stage_identity, &marker)?;
                let restart_from = restart
                    .as_ref()
                    .map(|(receipt, _)| receipt.previous(request, fingerprint))
                    .transpose()?;
                RecoveredPublication::Published(PublishedPolicy {
                    fingerprint,
                    restart_from,
                })
            }
            _ => return Err(changed()),
        };
        require(PublicationFiles::read(&self.directory, stage)? == files)?;
        marker.check_named(stage, RECEIPT, false)?;
        match restart {
            Some((_, marker)) => marker.check_named(stage, RESTART, false)?,
            None => require_absent(stage, RESTART)?,
        }
        check_unchanged_inputs()?;
        require(InputIdentity::of(&stage.metadata()?) == stage_identity)?;
        check_stage(&self.directory, &stage_name, stage)?;
        self.check()?;
        Ok(recovered)
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct PublicationReceipt {
    schema: u8,
    attempt: [u8; 32],
    reviewed_plan: [u8; 32],
    daemon: [u8; 32],
    unchanged_inputs: [u8; 32],
    original: FileSnapshot,
    source: FileSnapshot,
    leaf: FileSnapshot,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RestartReceipt {
    schema: u8,
    attempt: [u8; 32],
    reviewed_plan: [u8; 32],
    publication: [u8; 32],
    previous: [u8; 32],
}

impl RestartReceipt {
    fn previous(
        &self,
        request: &VendorPublicationRequest,
        fingerprint: [u8; 32],
    ) -> io::Result<[u8; 32]> {
        require(
            self.schema == RESTART_SCHEMA
                && self.attempt == request.attempt
                && self.reviewed_plan == request.reviewed.0
                && self.publication == fingerprint
                && self.previous != [0; 32],
        )?;
        Ok(self.previous)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum PublicationPlacement {
    Prepared,
    LeafPublished,
    Published,
}

#[derive(PartialEq, Eq, Serialize)]
struct PublicationFiles {
    live_custom: FileSnapshot,
    staged_custom: FileSnapshot,
    live_leaf: Option<FileSnapshot>,
    staged_leaf: Option<FileSnapshot>,
}

impl PublicationFiles {
    fn read(directory: &File, stage: &File) -> io::Result<Self> {
        fn optional(directory: &File, name: &CStr) -> io::Result<Option<FileSnapshot>> {
            match FileSnapshot::read_named(directory, name) {
                Ok(found) => Ok(Some(found)),
                Err(error) if error.kind() == io::ErrorKind::NotFound => {
                    require_absent(directory, name)?;
                    Ok(None)
                }
                Err(error) => Err(error),
            }
        }
        let leaf = CString::new(POLICY_LEAF).expect("fixed policy leaf");
        Ok(Self {
            live_custom: FileSnapshot::read_named(directory, CUSTOM)?,
            staged_custom: FileSnapshot::read_named(stage, STAGED_CUSTOM)?,
            live_leaf: optional(directory, &leaf)?,
            staged_leaf: optional(stage, STAGED_LEAF)?,
        })
    }

    fn placement(&self, receipt: &PublicationReceipt) -> Option<PublicationPlacement> {
        use PublicationPlacement::{LeafPublished, Prepared, Published};

        let leaf_landed = self
            .live_leaf
            .as_ref()
            .is_some_and(|leaf| leaf.matches_after_rename(&receipt.leaf));
        if self.live_custom == receipt.original && self.staged_custom == receipt.source {
            return match (&self.live_leaf, &self.staged_leaf) {
                (None, Some(leaf)) if *leaf == receipt.leaf => Some(Prepared),
                (Some(_), None) if leaf_landed => Some(LeafPublished),
                _ => None,
            };
        }
        (self.live_custom.matches_after_rename(&receipt.source)
            && self.staged_custom.matches_after_rename(&receipt.original)
            && leaf_landed
            && self.staged_leaf.is_none())
        .then_some(Published)
    }

    fn fingerprint(&self, stage: &InputIdentity, receipt: &FileSnapshot) -> io::Result<[u8; 32]> {
        let mut hash = Sha256::new();
        hash.update(b"neo:determinate-darwin-published-policy:v1\0");
        hash.update(serde_json::to_vec(&(stage, receipt, self)).map_err(io::Error::other)?);
        Ok(hash.finalize().into())
    }
}

fn write_record(
    directory: &File,
    name: &CStr,
    record: &impl Serialize,
) -> io::Result<FileSnapshot> {
    let bytes = serde_json::to_vec(record).map_err(io::Error::other)?;
    require(bytes.len() <= MAX_RECORD_BYTES)?;
    create_file(directory, name, &bytes, 0, 0o600, None)
}

fn read_record<T: DeserializeOwned>(
    directory: &File,
    name: &CStr,
) -> io::Result<(T, FileSnapshot)> {
    let file = open_at(directory, name, libc::O_RDONLY, 0)?;
    let (snapshot, bytes) = FileSnapshot::read_bounded(&file, MAX_RECORD_BYTES)?;
    require(
        snapshot.identity.mode & 0o7777 == 0o600
            && snapshot.identity.gid == 0
            && named_identity_matches(directory, name, &file)?
            && InputIdentity::of(&file.metadata()?) == snapshot.identity,
    )?;
    let record = serde_json::from_slice(&bytes).map_err(io::Error::other)?;
    Ok((record, snapshot))
}

#[derive(Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct FileSnapshot {
    identity: InputIdentity,
    digest: String,
    attributes: [u8; 32],
}

impl FileSnapshot {
    fn capture(file: &File) -> io::Result<Self> {
        Self::read_bounded(file, MAX_CONFIGURATION_BYTES).map(|(snapshot, _)| snapshot)
    }

    fn read_bounded(file: &File, maximum: usize) -> io::Result<(Self, Vec<u8>)> {
        let before = file.metadata()?;
        let identity = InputIdentity::of(&before);
        require(
            before.is_file()
                && identity.uid == 0
                && identity.links == 1
                && identity.mode & 0o7022 == 0
                && identity.size <= maximum as u64,
        )?;
        require_plain_metadata(file)?;
        let mut bytes = vec![0; identity.size as usize];
        file.read_exact_at(&mut bytes, 0)?;
        let attributes = attributes(file)?;
        require(InputIdentity::of(&file.metadata()?) == identity)?;
        Ok((
            Self {
                identity,
                digest: format!("{:x}", Sha256::digest(&bytes)),
                attributes,
            },
            bytes,
        ))
    }

    fn read_named(directory: &File, name: &CStr) -> io::Result<Self> {
        let file = open_at(directory, name, libc::O_RDONLY, 0)?;
        let actual = Self::capture(&file)?;
        require(
            named_identity_matches(directory, name, &file)?
                && InputIdentity::of(&file.metadata()?) == actual.identity,
        )?;
        Ok(actual)
    }

    fn check_named(&self, directory: &File, name: &CStr, renamed: bool) -> io::Result<()> {
        let actual = Self::read_named(directory, name)?;
        require(if renamed {
            actual.matches_after_rename(self)
        } else {
            actual == *self
        })
    }

    fn matches_after_rename(&self, before: &Self) -> bool {
        let mut identity = self.identity.clone();
        identity.changed = before.identity.changed;
        identity == before.identity
            && self.digest == before.digest
            && self.attributes == before.attributes
    }
}

fn require_plain_metadata(file: &File) -> io::Result<()> {
    unsafe extern "C" {
        fn acl_get_fd_np(fd: libc::c_int, kind: libc::c_int) -> *mut libc::c_void;
        fn acl_get_entry(
            acl: *mut libc::c_void,
            index: libc::c_int,
            entry: *mut *mut libc::c_void,
        ) -> libc::c_int;
        fn acl_free(acl: *mut libc::c_void) -> libc::c_int;
    }
    let mut stat = MaybeUninit::<libc::stat>::uninit();
    check_syscall(unsafe { libc::fstat(file.as_raw_fd(), stat.as_mut_ptr()) })?;
    require(unsafe { stat.assume_init() }.st_flags == 0)?;
    let acl = unsafe { acl_get_fd_np(file.as_raw_fd(), 0x100) };
    if acl.is_null() {
        return require(io::Error::last_os_error().raw_os_error() == Some(libc::ENOENT));
    }
    let mut entry = null_mut();
    let status = unsafe { acl_get_entry(acl, 0, &mut entry) };
    let error = io::Error::last_os_error().raw_os_error();
    unsafe { acl_free(acl) };
    require(status == -1 && error == Some(libc::EINVAL))
}

fn attributes(file: &File) -> io::Result<[u8; 32]> {
    let mut names = [0_u8; 4096];
    let fd = file.as_raw_fd();
    let length = unsafe { libc::flistxattr(fd, names.as_mut_ptr().cast(), names.len(), 0) };
    let length = usize::try_from(length).map_err(|_| io::Error::last_os_error())?;
    let names = names.get(..length).ok_or_else(changed)?;
    let mut names = names.split_inclusive(|byte| *byte == 0).collect::<Vec<_>>();
    names.sort_unstable();
    let mut digest = Sha256::new();
    let mut remaining = MAX_CONFIGURATION_BYTES;
    for name in names {
        let c_name = CStr::from_bytes_with_nul(name).map_err(io::Error::other)?;
        let length = unsafe { libc::fgetxattr(fd, c_name.as_ptr(), null_mut(), 0, 0, 0) };
        let length = usize::try_from(length).map_err(|_| io::Error::last_os_error())?;
        remaining = remaining.checked_sub(length).ok_or_else(changed)?;
        let mut value = vec![0; length];
        let actual = unsafe {
            libc::fgetxattr(fd, c_name.as_ptr(), value.as_mut_ptr().cast(), length, 0, 0)
        };
        require(actual == length as isize)?;
        digest.update(name);
        digest.update((length as u64).to_be_bytes());
        digest.update(value);
    }
    Ok(digest.finalize().into())
}

fn create_file(
    directory: &File,
    name: &CStr,
    bytes: &[u8],
    gid: u32,
    mode: u32,
    metadata_source: Option<&File>,
) -> io::Result<FileSnapshot> {
    let mut file = open_at(
        directory,
        name,
        libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
        0o600,
    )?;
    file.write_all(bytes)?;
    check_syscall(unsafe { libc::fchown(file.as_raw_fd(), 0, gid) })?;
    check_syscall(unsafe { libc::fchmod(file.as_raw_fd(), mode as libc::mode_t) })?;
    if let Some(original) = metadata_source {
        check_syscall(unsafe {
            libc::fcopyfile(
                original.as_raw_fd(),
                file.as_raw_fd(),
                null_mut(),
                libc::COPYFILE_XATTR,
            )
        })?;
    }
    check_syscall(unsafe { libc::fcntl(file.as_raw_fd(), libc::F_FULLFSYNC) })?;
    let snapshot = FileSnapshot::capture(&file)?;
    require(
        snapshot.identity.gid == gid
            && snapshot.identity.mode & 0o777 == mode
            && snapshot.digest == format!("{:x}", Sha256::digest(bytes))
            && named_identity_matches(directory, name, &file)?,
    )?;
    Ok(snapshot)
}

fn open_at(parent: &File, name: &CStr, flags: libc::c_int, mode: libc::mode_t) -> io::Result<File> {
    let fd = unsafe {
        libc::openat(
            parent.as_raw_fd(),
            name.as_ptr(),
            flags | libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_CLOEXEC,
            libc::c_int::from(mode),
        )
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { File::from_raw_fd(fd) })
}

fn stat_at(parent: &File, name: &CStr) -> io::Result<libc::stat> {
    let mut stat = MaybeUninit::<libc::stat>::uninit();
    check_syscall(unsafe {
        libc::fstatat(
            parent.as_raw_fd(),
            name.as_ptr(),
            stat.as_mut_ptr(),
            libc::AT_SYMLINK_NOFOLLOW,
        )
    })?;
    Ok(unsafe { stat.assume_init() })
}

fn require_absent(parent: &File, name: &CStr) -> io::Result<()> {
    match stat_at(parent, name) {
        Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(err) => Err(err),
        Ok(_) => Err(changed()),
    }
}

fn named_identity_matches(parent: &File, name: &CStr, file: &File) -> io::Result<bool> {
    let named = stat_at(parent, name)?;
    let opened = file.metadata()?;
    Ok(named.st_dev as u64 == opened.dev()
        && named.st_ino == opened.ino()
        && u32::from(named.st_mode) == opened.mode()
        && named.st_uid == opened.uid()
        && named.st_gid == opened.gid())
}

fn check_stage(parent: &File, name: &CStr, stage: &File) -> io::Result<()> {
    let metadata = stage.metadata()?;
    require(
        metadata.is_dir()
            && metadata.uid() == 0
            && metadata.mode() & 0o7777 == 0o700
            && named_identity_matches(parent, name, stage)?,
    )?;
    require_plain_metadata(stage)
}

fn rename_at(from: &File, old: &CStr, to: &File, new: &CStr, flags: u32) -> io::Result<()> {
    check_syscall(unsafe {
        libc::renameatx_np(
            from.as_raw_fd(),
            old.as_ptr(),
            to.as_raw_fd(),
            new.as_ptr(),
            flags,
        )
    })
}

fn sync_directories(stage: &File, parent: &File) -> io::Result<()> {
    stage.sync_all()?;
    parent.sync_all()
}

fn check_syscall(result: libc::c_int) -> io::Result<()> {
    if result == 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

fn require(value: bool) -> io::Result<()> {
    if value { Ok(()) } else { Err(changed()) }
}

fn changed() -> io::Error {
    io::Error::other("Darwin vendor policy publication inputs changed")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn snapshot(inode: u64, ctime: i64, digest: &str) -> FileSnapshot {
        FileSnapshot {
            identity: InputIdentity {
                device: 1,
                inode,
                mode: 0o100644,
                uid: 0,
                gid: 0,
                size: 42,
                links: 1,
                modified: (1, 0),
                changed: (ctime, 0),
            },
            digest: digest.to_owned(),
            attributes: [0; 32],
        }
    }

    #[test]
    fn recovery_identifies_the_exact_slot_placement() {
        let receipt = PublicationReceipt {
            schema: RECEIPT_SCHEMA,
            attempt: [1; 32],
            reviewed_plan: [2; 32],
            daemon: [3; 32],
            unchanged_inputs: [4; 32],
            original: snapshot(1, 1, "original"),
            source: snapshot(2, 1, "source"),
            leaf: snapshot(3, 1, "leaf"),
        };
        let mut files = PublicationFiles {
            live_custom: snapshot(1, 1, "original"),
            staged_custom: snapshot(2, 1, "source"),
            live_leaf: None,
            staged_leaf: Some(snapshot(3, 1, "leaf")),
        };
        assert_eq!(
            files.placement(&receipt),
            Some(PublicationPlacement::Prepared)
        );

        files.live_leaf = Some(snapshot(3, 2, "leaf"));
        assert_eq!(files.placement(&receipt), None);
        files.staged_leaf = None;
        assert_eq!(
            files.placement(&receipt),
            Some(PublicationPlacement::LeafPublished)
        );

        files.live_custom = snapshot(2, 2, "source");
        files.staged_custom = snapshot(1, 2, "original");
        assert_eq!(
            files.placement(&receipt),
            Some(PublicationPlacement::Published)
        );
        files.staged_custom = snapshot(4, 2, "original");
        assert_eq!(files.placement(&receipt), None);
    }

    #[test]
    fn restart_marker_matches_only_the_observed_published_policy() {
        let request = VendorPublicationRequest {
            attempt: [1; 32],
            reviewed: super::super::VendorPlanCommitment([2; 32]),
        };
        let mut files = PublicationFiles {
            live_custom: snapshot(2, 2, "source"),
            staged_custom: snapshot(1, 2, "original"),
            live_leaf: Some(snapshot(3, 2, "leaf")),
            staged_leaf: None,
        };
        let stage = snapshot(4, 1, "stage").identity;
        let receipt = snapshot(5, 1, "receipt");
        let fingerprint = files.fingerprint(&stage, &receipt).unwrap();
        let restart = RestartReceipt {
            schema: RESTART_SCHEMA,
            attempt: request.attempt,
            reviewed_plan: request.reviewed.0,
            publication: fingerprint,
            previous: [3; 32],
        };
        assert_eq!(restart.previous(&request, fingerprint).unwrap(), [3; 32]);

        let changed_receipt = snapshot(5, 1, "changed receipt");
        assert!(
            restart
                .previous(
                    &request,
                    files.fingerprint(&stage, &changed_receipt).unwrap()
                )
                .is_err()
        );
        files.live_custom = snapshot(2, 3, "source");
        assert!(
            restart
                .previous(&request, files.fingerprint(&stage, &receipt).unwrap())
                .is_err()
        );
    }
}
