use super::super::{InputIdentity, MAX_CONFIGURATION_BYTES, POLICY_LEAF, POLICY_STAGE};
use super::VendorPublicationRequest;
use super::configuration::{VendorApplication, protect_configuration_path};
use serde::Serialize;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::installation) enum PublicationOutcome {
    Refused,
    Pending,
    Published,
}

pub(super) struct LockedVendorDirectory {
    directory: File,
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
            let receipt = serde_json::to_vec(&PublicationReceipt {
                schema: 1,
                attempt: &request.attempt,
                reviewed_plan: &request.reviewed.0,
                daemon,
                original: &snapshot,
                source: &source,
                leaf: &leaf,
            })
            .map_err(io::Error::other)?;
            let marker = create_file(&stage, RECEIPT, &receipt, 0, 0o600, None)?;
            sync_directories(&stage, &self.directory)?;

            let check_source = || {
                check_directories()?;
                marker.check_named(&stage, RECEIPT, false)?;
                require(application.source_inputs_current())?;
                snapshot.check_named(&self.directory, CUSTOM, false)?;
                source.check_named(&stage, STAGED_CUSTOM, false)
            };
            check_source()?;
            leaf.check_named(&stage, STAGED_LEAF, false)?;
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
            leaf.check_named(&self.directory, &leaf_name, true)?;
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
            source.check_named(&self.directory, CUSTOM, true)?;
            leaf.check_named(&self.directory, &leaf_name, true)?;
            require(application.remaining_inputs_current())?;
            snapshot.check_named(&stage, STAGED_CUSTOM, true)
        };
        if publish().is_ok() {
            PublicationOutcome::Published
        } else {
            PublicationOutcome::Pending
        }
    }
}

#[derive(Serialize)]
struct PublicationReceipt<'a> {
    schema: u8,
    attempt: &'a [u8; 32],
    reviewed_plan: &'a [u8; 32],
    daemon: &'a [u8; 32],
    original: &'a FileSnapshot,
    source: &'a FileSnapshot,
    leaf: &'a FileSnapshot,
}

#[derive(PartialEq, Eq, Serialize)]
struct FileSnapshot {
    identity: InputIdentity,
    digest: String,
    attributes: [u8; 32],
}

impl FileSnapshot {
    fn capture(file: &File) -> io::Result<Self> {
        let before = file.metadata()?;
        let identity = InputIdentity::of(&before);
        require(
            before.is_file()
                && identity.uid == 0
                && identity.links == 1
                && identity.mode & 0o7022 == 0
                && identity.size <= MAX_CONFIGURATION_BYTES as u64,
        )?;
        require_plain_metadata(file)?;
        let mut bytes = vec![0; identity.size as usize];
        file.read_exact_at(&mut bytes, 0)?;
        let attributes = attributes(file)?;
        require(InputIdentity::of(&file.metadata()?) == identity)?;
        Ok(Self {
            identity,
            digest: format!("{:x}", Sha256::digest(bytes)),
            attributes,
        })
    }

    fn check_named(&self, directory: &File, name: &CStr, renamed: bool) -> io::Result<()> {
        let file = open_at(directory, name, libc::O_RDONLY, 0)?;
        let mut actual = Self::capture(&file)?;
        require(
            named_identity_matches(directory, name, &file)?
                && InputIdentity::of(&file.metadata()?) == actual.identity,
        )?;
        if renamed {
            actual.identity.changed = self.identity.changed;
        }
        require(actual == *self)
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
