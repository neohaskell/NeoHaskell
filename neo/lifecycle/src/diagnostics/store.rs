use super::Report;
use fs2::FileExt;
use ring::rand::{SecureRandom, SystemRandom};
use std::ffi::{CStr, CString, OsStr};
use std::fs::File;
use std::io::{Read, Write};
use std::os::fd::{AsRawFd, FromRawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::path::{Component, Path, PathBuf};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

const CAP: u64 = 10 * 1024 * 1024;
const MIN_RECORD: usize = 1024;
const MAX_RECORD: usize = 16 * 1024;
const MAX_ENTRIES: usize = CAP as usize / MIN_RECORD + 2;
const RETENTION_MS: u64 = 604_800_000;
const STAGE: &str = ".pending-v1";
const LOCK: &str = "lock";
const LOCK_WAIT: Duration = Duration::from_millis(100);

type Result<T> = std::result::Result<T, ()>;

struct Store {
    directory: File,
    path: PathBuf,
}

pub(super) struct OpenRecord {
    store: Store,
    name: String,
    id: String,
    created: u64,
    identity: Identity,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Identity {
    device: u64,
    inode: u64,
}

struct Retained {
    name: String,
    created: u64,
    size: u64,
    identity: Identity,
}

struct Locked(File);
impl Drop for Locked {
    fn drop(&mut self) {
        let _ = FileExt::unlock(&self.0);
    }
}

pub(super) fn now_ms() -> Result<u64> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| ())?
        .as_millis()
        .try_into()
        .map_err(|_| ())
}

pub(super) fn begin(report: &Report) -> Result<OpenRecord> {
    let mask = unsafe { libc::umask(0o777) };
    unsafe { libc::umask(mask) };
    if mask & 0o700 != 0 {
        return Err(());
    }
    let home = std::env::var_os("HOME").ok_or(())?;
    let home = PathBuf::from(home);
    let home_directory = walk(&home, false)?;
    owned_directory(&home_directory, false)?;
    let state = std::env::var_os("XDG_STATE_HOME")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
        .unwrap_or_else(|| home.join(".local/state"));
    Store::open(&state)?.begin(report)
}

impl Store {
    fn open(base: &Path) -> Result<Self> {
        if base
            .to_str()
            .is_none_or(|path| path.chars().any(char::is_control))
        {
            return Err(());
        }
        let parent = walk(base, true)?;
        owned_directory(&parent, false)?;
        let directory = child_directory(&parent, OsStr::new("neohaskell-diagnostics"), true)?;
        owned_directory(&directory, true)?;
        let store = Self {
            directory,
            path: base.join("neohaskell-diagnostics"),
        };
        store.verify_path()?;
        Ok(store)
    }

    fn verify_path(&self) -> Result<()> {
        let observed = walk(&self.path, false)?;
        owned_directory(&observed, true)?;
        if identity(&observed)? != identity(&self.directory)? {
            return Err(());
        }
        Ok(())
    }

    fn lock(&self) -> Result<Locked> {
        self.verify_path()?;
        let file = match open_at(
            &self.directory,
            LOCK,
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
            0o600,
        ) {
            Ok(file) => {
                self.directory.sync_all().map_err(|_| ())?;
                file
            }
            Err(error) if error.raw_os_error() == Some(libc::EEXIST) => {
                open_at(&self.directory, LOCK, libc::O_RDWR, 0).map_err(|_| ())?
            }
            Err(_) => return Err(()),
        };
        private_file(&file)?;
        if file.metadata().map_err(|_| ())?.len() != 0 {
            return Err(());
        }
        let deadline = Instant::now() + LOCK_WAIT;
        loop {
            match FileExt::try_lock_exclusive(&file) {
                Ok(()) => break,
                Err(error)
                    if error.kind() == std::io::ErrorKind::WouldBlock
                        && Instant::now() < deadline =>
                {
                    std::thread::sleep(Duration::from_millis(5));
                }
                Err(_) => return Err(()),
            }
        }
        let locked = Locked(file);
        private_file(&locked.0)?;
        self.matches(LOCK, identity(&locked.0)?)?;
        if locked.0.metadata().map_err(|_| ())?.len() != 0 {
            return Err(());
        }
        self.verify_path()?;
        Ok(locked)
    }

    fn matches(&self, name: &str, expected: Identity) -> Result<()> {
        let file = open_at(&self.directory, name, libc::O_RDONLY, 0).map_err(|_| ())?;
        private_file(&file)?;
        if identity(&file)? != expected {
            return Err(());
        }
        Ok(())
    }

    fn inventory(&self) -> Result<(Vec<Retained>, Option<Identity>, usize)> {
        let mut records = Vec::new();
        let mut stage = None;
        let names = names(&self.directory)?;
        let entries = names.len();
        for name in names {
            if name == LOCK {
                continue;
            }
            let bytes = name.as_bytes();
            if bytes == STAGE.as_bytes() {
                let file = open_at(&self.directory, STAGE, libc::O_RDONLY, 0).map_err(|_| ())?;
                private_file(&file)?;
                if file.metadata().map_err(|_| ())?.len() > MAX_RECORD as u64 {
                    return Err(());
                }
                stage = Some(identity(&file)?);
                continue;
            }
            if !bytes.starts_with(b"v1-") {
                continue;
            }
            let name = name.to_str().ok_or(())?.to_owned();
            let (created, id) = parse_name(&name)?;
            let mut file = open_at(&self.directory, &name, libc::O_RDONLY, 0).map_err(|_| ())?;
            private_file(&file)?;
            let size = file.metadata().map_err(|_| ())?.len();
            if !(MIN_RECORD as u64..=MAX_RECORD as u64).contains(&size) {
                return Err(());
            }
            let header = header(&id, created);
            let mut actual = vec![0; header.len()];
            file.read_exact(&mut actual).map_err(|_| ())?;
            if actual != header.as_bytes() {
                return Err(());
            }
            records.push(Retained {
                name,
                created,
                size,
                identity: identity(&file)?,
            });
        }
        records.sort_by(|a, b| (a.created, &a.name).cmp(&(b.created, &b.name)));
        Ok((records, stage, entries))
    }

    fn remove(&self, name: &str, expected: Identity) -> Result<()> {
        self.verify_path()?;
        self.matches(name, expected)?;
        let name = CString::new(name).map_err(|_| ())?;
        if unsafe { libc::unlinkat(self.directory.as_raw_fd(), name.as_ptr(), 0) } != 0 {
            return Err(());
        }
        Ok(())
    }

    fn prepare(&self, now: u64, replacement: Option<(&str, Identity)>) -> Result<()> {
        let (records, stage, entries) = self.inventory()?;
        if let Some((name, expected)) = replacement {
            if !records.iter().any(|record| {
                record.name == name
                    && record.identity == expected
                    && now.saturating_sub(record.created) < RETENTION_MS
            }) {
                return Err(());
            }
        }
        let mut total = records
            .iter()
            .try_fold(0u64, |sum, record| sum.checked_add(record.size))
            .ok_or(())?;
        let mut removals = Vec::new();
        for record in &records {
            let expired = now
                .checked_sub(record.created)
                .is_some_and(|age| age >= RETENTION_MS);
            if replacement.is_some_and(|(name, _)| name == record.name) {
                continue;
            }
            if expired || total + MAX_RECORD as u64 > CAP {
                total -= record.size;
                removals.push(record);
            }
        }
        if total + MAX_RECORD as u64 > CAP {
            return Err(());
        }
        let final_entries = entries - usize::from(stage.is_some()) - removals.len()
            + usize::from(replacement.is_none())
            + 1;
        if final_entries > MAX_ENTRIES {
            return Err(());
        }
        if let Some(identity) = stage {
            self.remove(STAGE, identity)?;
        }
        for record in removals {
            self.remove(&record.name, record.identity)?;
        }
        self.directory.sync_all().map_err(|_| ())
    }

    fn begin(self, report: &Report) -> Result<OpenRecord> {
        let created = report.started_at_ms();
        let mut random = [0u8; 16];
        SystemRandom::new().fill(&mut random).map_err(|_| ())?;
        let id = random
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();
        let name = format!("v1-{created:016x}-{id}.json");
        let bytes = encode(report, &id)?;
        let lock = self.lock()?;
        self.prepare(now_ms()?, None)?;
        match open_at(&self.directory, &name, libc::O_RDONLY, 0) {
            Err(error) if error.raw_os_error() == Some(libc::ENOENT) => {}
            _ => return Err(()),
        }
        let identity = self.publish(&name, &bytes, None)?;
        drop(lock);
        Ok(OpenRecord {
            store: self,
            name,
            id,
            created,
            identity,
        })
    }

    fn publish(&self, name: &str, bytes: &[u8], replacement: Option<Identity>) -> Result<Identity> {
        self.verify_path()?;
        let mut stage = open_at(
            &self.directory,
            STAGE,
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
            0o600,
        )
        .map_err(|_| ())?;
        private_file(&stage)?;
        let stage_identity = identity(&stage)?;
        stage.write_all(bytes).map_err(|_| ())?;
        stage.sync_all().map_err(|_| ())?;
        #[cfg(test)]
        if replacement.is_some() {
            exit_at_cut("stage");
        }
        self.verify_path()?;
        self.matches(STAGE, stage_identity)?;
        if let Some(expected) = replacement {
            self.matches(name, expected)?;
        }
        let source = CString::new(STAGE).map_err(|_| ())?;
        let target = CString::new(name).map_err(|_| ())?;
        if unsafe {
            libc::renameat(
                self.directory.as_raw_fd(),
                source.as_ptr(),
                self.directory.as_raw_fd(),
                target.as_ptr(),
            )
        } != 0
        {
            return Err(());
        }
        #[cfg(test)]
        if replacement.is_some() {
            exit_at_cut("rename");
        }
        self.directory.sync_all().map_err(|_| ())?;
        self.verify_path()?;
        self.matches(name, stage_identity)?;
        Ok(stage_identity)
    }
}

impl OpenRecord {
    pub(super) fn clear_retained(&self) -> Result<()> {
        let _lock = self.store.lock()?;
        let (records, stage, _) = self.store.inventory()?;
        if let Some(stage) = stage {
            self.store.remove(STAGE, stage)?;
        }
        for record in records {
            self.store.remove(&record.name, record.identity)?;
        }
        self.store.directory.sync_all().map_err(|_| ())?;
        self.store.verify_path()
    }

    pub(super) fn finish(self, report: &Report) -> Result<PathBuf> {
        if report.started_at_ms() != self.created {
            return Err(());
        }
        let bytes = encode(report, &self.id)?;
        let _lock = self.store.lock()?;
        self.store
            .prepare(now_ms()?, Some((&self.name, self.identity)))?;
        self.store
            .publish(&self.name, &bytes, Some(self.identity))?;
        Ok(self.store.path.join(self.name))
    }
}

fn header(id: &str, created: u64) -> String {
    format!("{{\"schema\":1,\"id\":\"{id}\",\"created_ms\":{created},\n")
}

fn encode(report: &Report, id: &str) -> Result<Vec<u8>> {
    let payload = report.encode()?;
    let mut bytes = header(id, report.started_at_ms()).into_bytes();
    bytes.extend_from_slice(b"\"report\":");
    bytes.extend_from_slice(&payload);
    bytes.extend_from_slice(b"}\n");
    if bytes.len() > MAX_RECORD {
        return Err(());
    }
    bytes.resize(bytes.len().max(MIN_RECORD), b' ');
    Ok(bytes)
}

fn parse_name(name: &str) -> Result<(u64, String)> {
    let value = name
        .strip_prefix("v1-")
        .and_then(|s| s.strip_suffix(".json"))
        .ok_or(())?;
    let (created, id) = value.split_once('-').ok_or(())?;
    if created.len() != 16
        || id.len() != 32
        || !created
            .bytes()
            .chain(id.bytes())
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    {
        return Err(());
    }
    Ok((u64::from_str_radix(created, 16).map_err(|_| ())?, id.into()))
}

fn identity(file: &File) -> Result<Identity> {
    let metadata = file.metadata().map_err(|_| ())?;
    Ok(Identity {
        device: metadata.dev(),
        inode: metadata.ino(),
    })
}

fn private_file(file: &File) -> Result<()> {
    let metadata = file.metadata().map_err(|_| ())?;
    if !metadata.is_file()
        || metadata.uid() != unsafe { libc::geteuid() }
        || metadata.mode() & 0o7777 != 0o600
        || metadata.nlink() != 1
    {
        return Err(());
    }
    safe_acl(file)
}

fn owned_directory(file: &File, private: bool) -> Result<()> {
    let metadata = file.metadata().map_err(|_| ())?;
    if !metadata.is_dir()
        || metadata.uid() != unsafe { libc::geteuid() }
        || metadata.mode() & 0o022 != 0
        || (private && !matches!(metadata.mode() & 0o7777, 0o700 | 0o2700))
    {
        return Err(());
    }
    safe_acl(file)
}

fn ancestor(file: &File) -> Result<()> {
    let metadata = file.metadata().map_err(|_| ())?;
    let trusted = metadata.uid() == 0 || metadata.uid() == unsafe { libc::geteuid() };
    let sticky_root = metadata.uid() == 0 && metadata.mode() & 0o1000 != 0;
    if !metadata.is_dir() || !trusted || (metadata.mode() & 0o022 != 0 && !sticky_root) {
        return Err(());
    }
    safe_acl(file)
}

fn walk(path: &Path, create: bool) -> Result<File> {
    if !path.is_absolute()
        || path
            .as_os_str()
            .as_bytes()
            .split(|byte| *byte == b'/')
            .any(|component| component == b"." || component == b"..")
    {
        return Err(());
    }
    let root = CString::new("/").unwrap();
    let fd = unsafe {
        libc::open(
            root.as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
        )
    };
    if fd < 0 {
        return Err(());
    }
    let mut directory = unsafe { File::from_raw_fd(fd) };
    ancestor(&directory)?;
    for component in path.components() {
        match component {
            Component::RootDir => {}
            Component::Normal(name) => {
                directory = child_directory(&directory, name, create)?;
                ancestor(&directory)?;
            }
            _ => return Err(()),
        }
    }
    Ok(directory)
}

fn child_directory(parent: &File, name: &OsStr, create: bool) -> Result<File> {
    let name = CString::new(name.as_bytes()).map_err(|_| ())?;
    let mut fd = unsafe {
        libc::openat(
            parent.as_raw_fd(),
            name.as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
        )
    };
    if fd < 0 && create && std::io::Error::last_os_error().raw_os_error() == Some(libc::ENOENT) {
        let result = unsafe { libc::mkdirat(parent.as_raw_fd(), name.as_ptr(), 0o700) };
        if result == 0 {
            #[cfg(test)]
            if name.as_bytes() == b"neohaskell-diagnostics" {
                exit_at_cut("mkdir");
            }
        } else if std::io::Error::last_os_error().raw_os_error() != Some(libc::EEXIST) {
            return Err(());
        }
        fd = unsafe {
            libc::openat(
                parent.as_raw_fd(),
                name.as_ptr(),
                libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
            )
        };
    }
    if fd < 0 {
        return Err(());
    }
    let directory = unsafe { File::from_raw_fd(fd) };
    if create {
        let metadata = parent.metadata().map_err(|_| ())?;
        if metadata.uid() == unsafe { libc::geteuid() } || metadata.mode() & 0o022 != 0 {
            parent.sync_all().map_err(|_| ())?;
        }
    }
    Ok(directory)
}

fn open_at(directory: &File, name: &str, flags: i32, mode: libc::mode_t) -> std::io::Result<File> {
    let name =
        CString::new(name).map_err(|_| std::io::Error::from(std::io::ErrorKind::InvalidInput))?;
    let fd = unsafe {
        libc::openat(
            directory.as_raw_fd(),
            name.as_ptr(),
            flags | libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_NONBLOCK,
            libc::c_uint::from(mode),
        )
    };
    if fd < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(unsafe { File::from_raw_fd(fd) })
    }
}

fn names(directory: &File) -> Result<Vec<std::ffi::OsString>> {
    let fresh = open_at(directory, ".", libc::O_RDONLY | libc::O_DIRECTORY, 0).map_err(|_| ())?;
    use std::os::fd::IntoRawFd;
    let fd = fresh.into_raw_fd();
    let stream = unsafe { libc::fdopendir(fd) };
    if stream.is_null() {
        unsafe {
            libc::close(fd);
        }
        return Err(());
    }
    struct Entries(*mut libc::DIR);
    impl Drop for Entries {
        fn drop(&mut self) {
            unsafe {
                libc::closedir(self.0);
            }
        }
    }
    let stream = Entries(stream);
    let mut result = Vec::new();
    loop {
        set_errno(0);
        let entry = unsafe { libc::readdir(stream.0) };
        if entry.is_null() {
            if errno() != 0 {
                return Err(());
            }
            break;
        }
        let name = unsafe { CStr::from_ptr((*entry).d_name.as_ptr()) }.to_bytes();
        if name == b"." || name == b".." {
            continue;
        }
        if result.len() == MAX_ENTRIES {
            return Err(());
        }
        result.push(OsStr::from_bytes(name).to_owned());
    }
    Ok(result)
}

#[cfg(target_os = "macos")]
fn errno_pointer() -> *mut i32 {
    unsafe { libc::__error() }
}
#[cfg(target_os = "linux")]
fn errno_pointer() -> *mut i32 {
    unsafe { libc::__errno_location() }
}
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn errno() -> i32 {
    unsafe { *errno_pointer() }
}
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn set_errno(value: i32) {
    unsafe {
        *errno_pointer() = value;
    }
}

#[cfg(target_os = "linux")]
fn safe_acl(file: &File) -> Result<()> {
    for name in [c"system.posix_acl_access", c"system.posix_acl_default"] {
        let size =
            unsafe { libc::fgetxattr(file.as_raw_fd(), name.as_ptr(), std::ptr::null_mut(), 0) };
        if size == 0 {
            continue;
        }
        if size < 0
            && matches!(
                std::io::Error::last_os_error().raw_os_error(),
                Some(libc::ENODATA | libc::ENOTSUP)
            )
        {
            continue;
        }
        return Err(());
    }
    Ok(())
}

#[cfg(target_os = "macos")]
fn safe_acl(file: &File) -> Result<()> {
    use std::ffi::c_void;
    unsafe extern "C" {
        fn acl_get_fd_np(fd: libc::c_int, kind: i32) -> *mut c_void;
        fn acl_get_entry(acl: *mut c_void, entry_id: i32, entry: *mut *mut c_void) -> i32;
        fn acl_get_tag_type(entry: *mut c_void, tag: *mut i32) -> i32;
        fn acl_free(value: *mut c_void) -> i32;
    }
    struct Acl(*mut c_void);
    impl Drop for Acl {
        fn drop(&mut self) {
            unsafe {
                acl_free(self.0);
            }
        }
    }
    const ACL_TYPE_EXTENDED: i32 = 0x100;
    const ACL_FIRST_ENTRY: i32 = 0;
    const ACL_NEXT_ENTRY: i32 = -1;
    const ACL_EXTENDED_DENY: i32 = 2;

    let pointer = unsafe { acl_get_fd_np(file.as_raw_fd(), ACL_TYPE_EXTENDED) };
    if pointer.is_null() {
        return if std::io::Error::last_os_error().raw_os_error() == Some(libc::ENOENT) {
            Ok(())
        } else {
            Err(())
        };
    }
    let acl = Acl(pointer);
    let mut entry_id = ACL_FIRST_ENTRY;
    for _ in 0..=169 {
        let mut entry = std::ptr::null_mut();
        let status = unsafe { acl_get_entry(acl.0, entry_id, &mut entry) };
        if status != 0 {
            return if std::io::Error::last_os_error().raw_os_error() == Some(libc::EINVAL) {
                Ok(())
            } else {
                Err(())
            };
        }
        let mut tag = 0;
        if unsafe { acl_get_tag_type(entry, &mut tag) } != 0 || tag != ACL_EXTENDED_DENY {
            return Err(());
        }
        entry_id = ACL_NEXT_ENTRY;
    }
    Err(())
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn errno() -> i32 {
    1
}
#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn set_errno(_: i32) {}
#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn safe_acl(_: &File) -> Result<()> {
    Err(())
}

#[cfg(test)]
fn exit_at_cut(point: &str) {
    if std::env::var("NEO_DIAGNOSTIC_CUT").is_ok_and(|cut| cut == point) {
        std::process::exit(41);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::{Invocation, NativeRoute};
    use std::fs;
    use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

    fn setup() -> (tempfile::TempDir, Store) {
        let temporary = tempfile::tempdir().unwrap();
        let base = fs::canonicalize(temporary.path()).unwrap();
        let store = Store::open(&base).unwrap();
        (temporary, store)
    }

    fn started(created: u64) -> Report {
        Report::started(Invocation::Native(NativeRoute::Help), created)
    }

    fn seed(store: &Store, created: u64, serial: u64, size: usize) -> String {
        let id = format!("{serial:032x}");
        let name = format!("v1-{created:016x}-{id}.json");
        let mut bytes = encode(&started(created), &id).unwrap();
        assert!(bytes.len() <= size);
        bytes.resize(size, b' ');
        let mut file = open_at(
            &store.directory,
            &name,
            libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL,
            0o600,
        )
        .unwrap();
        file.write_all(&bytes).unwrap();
        name
    }

    #[test]
    fn setgid_parent_keeps_owner_only_access() {
        let temporary = tempfile::tempdir().unwrap();
        let base = fs::canonicalize(temporary.path()).unwrap();
        fs::set_permissions(&base, fs::Permissions::from_mode(0o2700)).unwrap();
        let store = Store::open(&base).unwrap();
        let relative = store.path.clone();
        assert_eq!(fs::metadata(&relative).unwrap().mode() & 0o777, 0o700);
        #[cfg(target_os = "linux")]
        assert_eq!(fs::metadata(&relative).unwrap().mode() & 0o7777, 0o2700);
        let report = started(now_ms().unwrap());
        let record = store.begin(&report).unwrap();
        assert_eq!(
            fs::metadata(relative.join(record.name)).unwrap().mode() & 0o7777,
            0o600
        );
    }

    #[test]
    fn begin_and_finish_keep_one_name_and_replace_the_inode() {
        let (_temporary, store) = setup();
        let report = started(now_ms().unwrap());
        let open = store.begin(&report).unwrap();
        let name = open.name.clone();
        let identity = open.identity;
        let path = open.finish(&report).unwrap();
        assert_eq!(path.file_name().unwrap(), name.as_str());
        let file = File::open(&path).unwrap();
        assert!(super::identity(&file).unwrap() != identity);
        let value: serde_json::Value = serde_json::from_slice(&fs::read(path).unwrap()).unwrap();
        assert_eq!(value["schema"], 1);
    }

    #[test]
    fn retention_uses_exact_millisecond_boundary_and_keeps_future_records() {
        let (_temporary, store) = setup();
        let now = RETENTION_MS + 100;
        let expired = seed(&store, 100, 1, MIN_RECORD);
        let fresh = seed(&store, 101, 2, MIN_RECORD);
        let future = seed(&store, now + 1, 3, MIN_RECORD);
        let _lock = store.lock().unwrap();
        store.prepare(now, None).unwrap();
        assert!(!store.path.join(expired).exists());
        assert!(store.path.join(fresh).exists());
        assert!(store.path.join(future).exists());
    }

    #[test]
    fn full_store_reserves_stage_bytes_and_evicts_oldest_only() {
        let (_temporary, store) = setup();
        let now = now_ms().unwrap();
        for serial in 0..CAP / MAX_RECORD as u64 {
            seed(&store, now, serial, MAX_RECORD);
        }
        let _lock = store.lock().unwrap();
        store.prepare(now, None).unwrap();
        let (records, stage, _) = store.inventory().unwrap();
        assert!(stage.is_none());
        assert_eq!(
            records.iter().map(|record| record.size).sum::<u64>() + MAX_RECORD as u64,
            CAP
        );
        assert!(
            !store
                .path
                .join(format!("v1-{now:016x}-{:032x}.json", 0))
                .exists()
        );
        assert!(
            store
                .path
                .join(format!("v1-{now:016x}-{:032x}.json", 1))
                .exists()
        );
        let bytes = vec![b' '; MAX_RECORD];
        let stage = open_at(
            &store.directory,
            STAGE,
            libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL,
            0o600,
        )
        .unwrap();
        (&stage).write_all(&bytes).unwrap();
        let total =
            records.iter().map(|record| record.size).sum::<u64>() + stage.metadata().unwrap().len();
        assert_eq!(total, CAP);
    }

    #[test]
    fn foreign_entries_survive_and_overflow_refuses_before_pruning() {
        let (_temporary, store) = setup();
        let expired = seed(&store, 0, 1, MIN_RECORD);
        for serial in 0..MAX_ENTRIES {
            fs::write(store.path.join(format!("foreign-{serial}")), b"untouched").unwrap();
        }
        let _lock = store.lock().unwrap();
        assert!(store.prepare(RETENTION_MS, None).is_err());
        assert!(store.path.join(expired).exists());
        assert_eq!(
            fs::read(store.path.join("foreign-0")).unwrap(),
            b"untouched"
        );
    }

    #[test]
    fn corrupt_reserved_header_prevents_all_pruning() {
        let (_temporary, store) = setup();
        let expired = seed(&store, 0, 1, MIN_RECORD);
        let bad = seed(&store, 0, 2, MIN_RECORD);
        fs::write(store.path.join(&bad), vec![b'x'; MIN_RECORD]).unwrap();
        let _lock = store.lock().unwrap();
        assert!(store.prepare(RETENTION_MS, None).is_err());
        assert!(store.path.join(expired).exists());
        assert!(store.path.join(bad).exists());
    }

    #[test]
    fn hardlinked_report_is_neither_read_as_owned_nor_removed() {
        let (_temporary, store) = setup();
        let name = seed(&store, 0, 1, MIN_RECORD);
        let outside = store.path.parent().unwrap().join("external-copy");
        fs::hard_link(store.path.join(&name), &outside).unwrap();
        let _lock = store.lock().unwrap();
        assert!(store.prepare(RETENTION_MS, None).is_err());
        assert!(outside.exists());
        assert!(store.path.join(name).exists());
    }

    #[test]
    fn symlink_and_fifo_reserved_entries_refuse_without_following_or_blocking() {
        for fifo in [false, true] {
            let (_temporary, store) = setup();
            let path = store
                .path
                .join("v1-0000000000000000-00000000000000000000000000000001.json");
            if fifo {
                let path = CString::new(path.as_os_str().as_bytes()).unwrap();
                assert_eq!(unsafe { libc::mkfifo(path.as_ptr(), 0o600) }, 0);
            } else {
                std::os::unix::fs::symlink("/", &path).unwrap();
            }
            let _lock = store.lock().unwrap();
            assert!(store.prepare(RETENTION_MS, None).is_err());
            assert!(fs::symlink_metadata(path).is_ok());
        }
    }

    #[test]
    fn malformed_lock_is_preserved() {
        let (_temporary, store) = setup();
        let path = store.path.join(LOCK);
        fs::OpenOptions::new()
            .create_new(true)
            .write(true)
            .mode(0o600)
            .open(&path)
            .unwrap()
            .write_all(b"foreign")
            .unwrap();
        assert!(store.lock().is_err());
        assert_eq!(fs::read(path).unwrap(), b"foreign");
    }

    #[test]
    fn lock_contention_declines_and_keeps_stable_inode() {
        let (_temporary, store) = setup();
        let lock = store.lock().unwrap();
        let expected = identity(&lock.0).unwrap();
        assert!(store.lock().is_err());
        drop(lock);
        assert!(identity(&store.lock().unwrap().0).unwrap() == expected);
    }

    #[test]
    fn stale_empty_stage_is_removed_only_after_whole_inventory_validation() {
        let (_temporary, store) = setup();
        let file = open_at(
            &store.directory,
            STAGE,
            libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL,
            0o600,
        )
        .unwrap();
        drop(file);
        let _lock = store.lock().unwrap();
        store.prepare(now_ms().unwrap(), None).unwrap();
        assert!(!store.path.join(STAGE).exists());
    }

    #[test]
    fn finish_never_resurrects_deleted_started_record_and_export_survives() {
        let (_temporary, store) = setup();
        let report = started(now_ms().unwrap());
        let open = store.begin(&report).unwrap();
        let path = open.store.path.join(&open.name);
        let export = open.store.path.parent().unwrap().join("user-export.json");
        fs::copy(&path, &export).unwrap();
        fs::remove_file(&path).unwrap();
        assert!(open.finish(&report).is_err());
        assert!(!path.exists());
        assert!(export.exists());
    }

    #[test]
    fn uninstall_clears_owned_reports_and_staging_but_preserves_exports() {
        let (_temporary, store) = setup();
        let other = Store::open(store.path.parent().unwrap()).unwrap();
        let first = store.begin(&started(now_ms().unwrap())).unwrap();
        let second = other.begin(&started(now_ms().unwrap())).unwrap();
        let store = &second.store;
        let export = store.path.parent().unwrap().join("user-export.json");
        fs::copy(store.path.join(&first.name), &export).unwrap();
        let exported = fs::read(&export).unwrap();
        let unrelated = store.path.join("notes.json");
        fs::write(&unrelated, b"user data").unwrap();
        let _stage = open_at(
            &store.directory,
            STAGE,
            libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL,
            0o600,
        )
        .unwrap();
        second.clear_retained().unwrap();
        assert!(!store.path.join(STAGE).exists());
        assert!(!store.path.join(&first.name).exists());
        assert!(!store.path.join(&second.name).exists());
        assert_eq!(fs::read(export).unwrap(), exported);
        assert_eq!(fs::read(unrelated).unwrap(), b"user data");
        assert!(store.path.join(LOCK).exists());
    }

    #[test]
    fn uninstall_preserves_reserved_foreign_file_and_reports_on_inventory_failure() {
        let (_temporary, store) = setup();
        let open = store.begin(&started(now_ms().unwrap())).unwrap();
        let store = &open.store;
        let report = open.store.path.join(&open.name);
        let prior = fs::read(&report).unwrap();
        let foreign = store.path.join("v1-not an owned report");
        fs::write(&foreign, b"foreign bytes").unwrap();
        assert!(open.clear_retained().is_err());
        assert_eq!(fs::read(report).unwrap(), prior);
        assert_eq!(fs::read(foreign).unwrap(), b"foreign bytes");
    }

    #[test]
    fn moved_store_refuses_a_misleading_report_locator() {
        let (temporary, store) = setup();
        let report = started(now_ms().unwrap());
        let open = store.begin(&report).unwrap();
        let base = fs::canonicalize(temporary.path()).unwrap();
        fs::rename(base.join("neohaskell-diagnostics"), base.join("moved")).unwrap();
        assert!(open.finish(&report).is_err());
    }

    #[test]
    fn wrong_modes_and_symlinked_parents_are_not_repaired() {
        let (temporary, store) = setup();
        fs::set_permissions(&store.path, fs::Permissions::from_mode(0o755)).unwrap();
        assert!(store.lock().is_err());
        assert_eq!(fs::metadata(&store.path).unwrap().mode() & 0o777, 0o755);
        let base = fs::canonicalize(temporary.path()).unwrap();
        let alias = base.join("alias");
        std::os::unix::fs::symlink(&base, &alias).unwrap();
        assert!(Store::open(&alias.join("state")).is_err());
        assert!(!base.join("state").exists());
    }

    #[test]
    fn display_control_characters_are_refused_before_creating_state() {
        let temporary = tempfile::tempdir().unwrap();
        let base = fs::canonicalize(temporary.path())
            .unwrap()
            .join("bad\npath");
        assert!(Store::open(&base).is_err());
        assert!(!base.exists());
    }
    #[cfg(target_os = "macos")]
    #[test]
    fn darwin_allow_acl_refuses_private_file() {
        let (_temporary, store) = setup();
        let name = seed(&store, 0, 1, MIN_RECORD);
        let path = store.path.join(&name);
        let status = std::process::Command::new("/bin/chmod")
            .args(["+a", "everyone allow read"])
            .arg(&path)
            .status()
            .unwrap();
        assert!(status.success());
        let _lock = store.lock().unwrap();
        assert!(store.prepare(RETENTION_MS, None).is_err());
        assert!(path.exists());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_default_acl_refuses_before_creating_store() {
        let temporary = tempfile::tempdir().unwrap();
        let base = fs::canonicalize(temporary.path()).unwrap();
        let directory = File::open(&base).unwrap();
        let mut acl = 2u32.to_le_bytes().to_vec();
        for (tag, permission) in [(1u16, 7u16), (4, 0), (32, 0)] {
            acl.extend_from_slice(&tag.to_le_bytes());
            acl.extend_from_slice(&permission.to_le_bytes());
            acl.extend_from_slice(&u32::MAX.to_le_bytes());
        }
        let result = unsafe {
            libc::fsetxattr(
                directory.as_raw_fd(),
                c"system.posix_acl_default".as_ptr(),
                acl.as_ptr().cast(),
                acl.len(),
                0,
            )
        };
        assert_eq!(
            result, 0,
            "ACL fixture requires a filesystem supporting POSIX ACLs"
        );
        assert!(Store::open(&base).is_err());
        assert!(!base.join("neohaskell-diagnostics").exists());
    }

    #[test]
    fn process_exit_at_publication_cuts_leaves_a_recoverable_store() {
        for cut in ["mkdir", "started", "stage", "rename"] {
            let temporary = tempfile::tempdir().unwrap();
            let base = fs::canonicalize(temporary.path()).unwrap();
            let output = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "diagnostics::store::tests::publication_cut_child",
                    "--nocapture",
                ])
                .env_clear()
                .env("NEO_DIAGNOSTIC_CUT", cut)
                .env("NEO_DIAGNOSTIC_CUT_BASE", &base)
                .output()
                .unwrap();
            assert_eq!(
                output.status.code(),
                Some(41),
                "{}",
                String::from_utf8_lossy(&output.stderr)
            );
            let store = Store::open(&base).unwrap();
            let (records, stage, _) = store.inventory().unwrap();
            assert_eq!(records.len(), usize::from(cut != "mkdir"));
            assert_eq!(stage.is_some(), cut == "stage");
            if let Some(record) = records.first() {
                let value: serde_json::Value =
                    serde_json::from_slice(&fs::read(store.path.join(&record.name)).unwrap())
                        .unwrap();
                assert_eq!(
                    value["report"]["state"]["kind"],
                    if cut == "rename" {
                        "finished"
                    } else {
                        "started"
                    }
                );
            }
            let report = started(now_ms().unwrap());
            store.begin(&report).unwrap();
            let store = Store::open(&base).unwrap();
            assert_eq!(store.inventory().unwrap().0.len(), records.len() + 1);
            assert!(!store.path.join(STAGE).exists());
        }
    }

    #[test]
    fn publication_cut_child() {
        let Ok(cut) = std::env::var("NEO_DIAGNOSTIC_CUT") else {
            return;
        };
        let base = PathBuf::from(std::env::var_os("NEO_DIAGNOSTIC_CUT_BASE").unwrap());
        let mut report = started(now_ms().unwrap());
        let open = Store::open(&base).unwrap().begin(&report).unwrap();
        exit_at_cut("started");
        assert!(matches!(cut.as_str(), "stage" | "rename"));
        report.state = super::super::State::Finished {
            finished_at_ms: Some(now_ms().unwrap()),
            duration_ms: 0,
            exit_code: 0,
            outcome: super::super::DiagnosticOutcome::HelpDisplayed,
        };
        open.finish(&report).unwrap();
        panic!("publication did not reach the requested cut");
    }
}
