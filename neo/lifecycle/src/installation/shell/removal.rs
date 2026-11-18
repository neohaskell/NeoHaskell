use super::*;
use std::os::unix::fs::FileExt as UnixFileExt;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(crate) struct RemovalPlan {
    name: String,
    escrow: String,
    before: Identity,
    after: Identity,
    before_digest: String,
    after_digest: String,
    metadata: ProfileMetadata,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
struct ProfileMetadata {
    mode: u32,
    gid: u32,
    attributes: String,
}

impl RemovalPlan {
    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn validate(&self) -> Result<(), TransactionError> {
        if !matches!(
            self.name.as_str(),
            ".bash_profile" | ".bash_login" | ".profile" | ".bashrc" | ".zprofile" | ".zshrc"
        ) || !self.escrow.starts_with("removed-")
            || self.escrow.len() > 100
            || !self
                .escrow
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'-')
            || self.before == self.after
            || self.before.uid != unsafe { libc::geteuid() }
            || self.after.uid != self.before.uid
            || self.before.device != self.after.device
            || self.metadata.mode & !0o777 != 0
            || self.metadata.mode & 0o022 != 0
            || [
                &self.before_digest,
                &self.after_digest,
                &self.metadata.attributes,
            ]
            .iter()
            .any(|s| s.len() != 64 || !s.bytes().all(|b| b.is_ascii_hexdigit()))
        {
            return Err(TransactionError::InvalidState("shell removal plan"));
        }
        Ok(())
    }
}

impl Receipt {
    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

impl Ledger {
    pub(crate) fn home(&self) -> PathBuf {
        self.claim.home.clone()
    }
    pub(crate) fn receipts(&self) -> &[Receipt] {
        &self.receipts
    }
    pub(crate) fn validate_removal(&self, plan: &RemovalPlan) -> Result<(), TransactionError> {
        plan.validate()?;
        if self.pending.is_some()
            || !self
                .receipts
                .iter()
                .any(|r| r.name == plan.name && r.identity == plan.before)
        {
            return Err(TransactionError::InvalidState("shell removal ownership"));
        }
        Ok(())
    }
}

fn read(file: &File) -> Result<Vec<u8>, TransactionError> {
    let m = file.metadata().map_err(|_| TransactionError::Io)?;
    if !m.is_file() || m.nlink() != 1 || m.len() > MAX_PROFILE {
        return Err(TransactionError::UnsafePath);
    }
    let mut result = Vec::new();
    let mut chunk = [0u8; 8192];
    loop {
        let n = file
            .read_at(&mut chunk, result.len() as u64)
            .map_err(|_| TransactionError::Io)?;
        if n == 0 {
            break;
        }
        result.extend_from_slice(&chunk[..n]);
        if result.len() as u64 > MAX_PROFILE {
            return Err(TransactionError::UnsafePath);
        }
    }
    Ok(result)
}

fn attributes(file: &File) -> Result<String, TransactionError> {
    #[cfg(target_os = "macos")]
    unsafe {
        unsafe extern "C" {
            fn acl_get_fd_np(fd: libc::c_int, kind: libc::c_int) -> *mut libc::c_void;
            fn acl_get_entry(
                acl: *mut libc::c_void,
                entry_id: libc::c_int,
                entry: *mut *mut libc::c_void,
            ) -> libc::c_int;
            fn acl_free(obj: *mut libc::c_void) -> libc::c_int;
        }
        let acl = acl_get_fd_np(file.as_raw_fd(), 0x100);
        if acl.is_null() {
            if std::io::Error::last_os_error().raw_os_error() != Some(libc::ENOENT) {
                return Err(TransactionError::UnsafePath);
            }
        } else {
            let mut entry = std::ptr::null_mut();
            let status = acl_get_entry(acl, 0, &mut entry);
            let error = std::io::Error::last_os_error().raw_os_error();
            acl_free(acl);
            if status != -1 || error != Some(libc::EINVAL) {
                return Err(TransactionError::UnsafePath);
            }
        }
        let mut stat: libc::stat = std::mem::zeroed();
        if libc::fstat(file.as_raw_fd(), &mut stat) != 0 || stat.st_flags != 0 {
            return Err(TransactionError::UnsafePath);
        }
        let size = libc::flistxattr(file.as_raw_fd(), std::ptr::null_mut(), 0, 0);
        if !(0..=4096).contains(&size) {
            return Err(TransactionError::UnsafePath);
        }
        let mut names = vec![0u8; size as usize];
        if libc::flistxattr(file.as_raw_fd(), names.as_mut_ptr().cast(), names.len(), 0) != size {
            return Err(TransactionError::UnsafePath);
        }
        if names.is_empty() {
            return Ok(digest(&[]));
        }
        if names != b"com.apple.provenance\0" {
            return Err(TransactionError::UnsafePath);
        }
        let mut value = [0u8; 1024];
        let count = libc::fgetxattr(
            file.as_raw_fd(),
            c"com.apple.provenance".as_ptr(),
            value.as_mut_ptr().cast(),
            value.len(),
            0,
            0,
        );
        if count < 0 {
            return Err(TransactionError::UnsafePath);
        }
        return Ok(digest(&value[..count as usize]));
    }
    #[cfg(target_os = "linux")]
    unsafe {
        let count = libc::flistxattr(file.as_raw_fd(), std::ptr::null_mut(), 0);
        if count != 0 {
            return Err(TransactionError::UnsafePath);
        }
        let mut flags: libc::c_long = 0;
        if libc::ioctl(file.as_raw_fd(), 0x80086601 as libc::c_ulong, &mut flags) != 0 {
            if !matches!(
                std::io::Error::last_os_error().raw_os_error(),
                Some(libc::ENOTTY) | Some(libc::EOPNOTSUPP)
            ) {
                return Err(TransactionError::UnsafePath);
            }
        } else if flags & !0x80000 != 0 {
            return Err(TransactionError::UnsafePath);
        }
        Ok(digest(&[]))
    }
}

fn metadata(file: &File) -> Result<ProfileMetadata, TransactionError> {
    let m = file.metadata().map_err(|_| TransactionError::Io)?;
    if !m.is_file()
        || m.nlink() != 1
        || m.uid() != unsafe { libc::geteuid() }
        || m.mode() & 0o7022 != 0
    {
        return Err(TransactionError::UnsafePath);
    }
    Ok(ProfileMetadata {
        mode: m.mode() & 0o777,
        gid: m.gid(),
        attributes: attributes(file)?,
    })
}

fn without_block(value: &[u8], receipt: &Receipt) -> Result<Vec<u8>, TransactionError> {
    let positions: Vec<_> = value
        .windows(receipt.block.len())
        .enumerate()
        .filter_map(|(i, b)| (b == receipt.block.as_bytes()).then_some(i))
        .collect();
    if positions.len() != 1 {
        return Err(TransactionError::InvalidState(
            "owned shell block is absent or duplicated",
        ));
    }
    let start = positions[0];
    let end = start + receipt.block.len();
    let mut result = value[..start].to_vec();
    if start > 0 && value[start - 1] != b'\n' && end < value.len() && value[end] != b'\n' {
        result.push(b'\n');
    }
    result.extend_from_slice(&value[end..]);
    Ok(result)
}

impl HomeLease {
    pub(crate) fn inspect_removal(
        &self,
        receipt: &Receipt,
    ) -> Result<ProfilePreimage, TransactionError> {
        self.check()?;
        let file = at(&self.home, &receipt.name, libc::O_RDONLY, 0)?;
        if Identity::of(&file)? != receipt.identity {
            return Err(TransactionError::UnsafePath);
        }
        metadata(&file)?;
        let value = read(&file)?;
        without_block(&value, receipt)?;
        self.check()?;
        Ok(ProfilePreimage {
            identity: receipt.identity.clone(),
            length: value.len() as u64,
            digest: digest(&value),
        })
    }

    pub(crate) fn plan_removal(&self, receipt: &Receipt) -> Result<RemovalPlan, TransactionError> {
        self.check()?;
        let file = at(&self.home, &receipt.name, libc::O_RDONLY, 0)?;
        if Identity::of(&file)? != receipt.identity {
            return Err(TransactionError::UnsafePath);
        }
        let original_metadata = metadata(&file)?;
        let before = read(&file)?;
        let after = without_block(&before, receipt)?;
        let key = serde_json::to_vec(&(receipt, digest(&before), &original_metadata))
            .map_err(|_| TransactionError::Io)?;
        let escrow = format!("removed-{}", digest(&key));
        let staged = match at(
            &self.directory,
            &escrow,
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
            0o600,
        ) {
            Ok(mut staged) => {
                staged.write_all(&after).map_err(|_| TransactionError::Io)?;
                if unsafe { libc::fchown(staged.as_raw_fd(), u32::MAX, original_metadata.gid) } != 0
                    || unsafe {
                        libc::fchmod(staged.as_raw_fd(), original_metadata.mode as libc::mode_t)
                    } != 0
                {
                    return Err(TransactionError::UnsafePath);
                }
                staged
            }
            Err(_) => {
                let staged = at(&self.directory, &escrow, libc::O_RDONLY, 0)?;
                if read(&staged)? != after {
                    return Err(TransactionError::InvalidState(
                        "shell removal escrow already occupied",
                    ));
                }
                staged
            }
        };
        if metadata(&staged)? != original_metadata {
            return Err(TransactionError::UnsafePath);
        }
        staged.sync_all().map_err(|_| TransactionError::Io)?;
        self.directory
            .sync_all()
            .map_err(|_| TransactionError::Io)?;
        let plan = RemovalPlan {
            name: receipt.name.clone(),
            escrow,
            before: receipt.identity.clone(),
            after: Identity::of(&staged)?,
            before_digest: digest(&before),
            after_digest: digest(&after),
            metadata: original_metadata,
        };
        plan.validate()?;
        self.check()?;
        Ok(plan)
    }

    pub(crate) fn remove(&self, plan: &RemovalPlan) -> Result<(), TransactionError> {
        plan.validate()?;
        self.check()?;
        let active = at(&self.home, &plan.name, libc::O_RDONLY, 0)?;
        let escrow = at(&self.directory, &plan.escrow, libc::O_RDONLY, 0)?;
        let active_identity = Identity::of(&active)?;
        let escrow_identity = Identity::of(&escrow)?;
        if active_identity == plan.after && escrow_identity == plan.before {
            return self.removal_completed(plan);
        }
        if active_identity != plan.before
            || escrow_identity != plan.after
            || digest(&read(&active)?) != plan.before_digest
            || digest(&read(&escrow)?) != plan.after_digest
            || metadata(&active)? != plan.metadata
            || metadata(&escrow)? != plan.metadata
        {
            return Err(TransactionError::InvalidState(
                "shell removal changed before exchange",
            ));
        }
        self.check()?;
        let name = std::ffi::CString::new(plan.name.as_str()).unwrap();
        let slot = std::ffi::CString::new(plan.escrow.as_str()).unwrap();
        #[cfg(target_os = "macos")]
        let status = unsafe {
            libc::renameatx_np(
                self.home.as_raw_fd(),
                name.as_ptr(),
                self.directory.as_raw_fd(),
                slot.as_ptr(),
                libc::RENAME_SWAP,
            )
        };
        #[cfg(target_os = "linux")]
        let status = unsafe {
            libc::renameat2(
                self.home.as_raw_fd(),
                name.as_ptr(),
                self.directory.as_raw_fd(),
                slot.as_ptr(),
                libc::RENAME_EXCHANGE,
            )
        };
        if status != 0 {
            return Err(TransactionError::Io);
        }
        self.home.sync_all().map_err(|_| TransactionError::Io)?;
        self.directory
            .sync_all()
            .map_err(|_| TransactionError::Io)?;
        self.removal_completed(plan)
    }

    fn removal_completed(&self, plan: &RemovalPlan) -> Result<(), TransactionError> {
        self.check()?;
        let active = at(&self.home, &plan.name, libc::O_RDONLY, 0)?;
        let escrow = at(&self.directory, &plan.escrow, libc::O_RDONLY, 0)?;
        if Identity::of(&active)? != plan.after
            || Identity::of(&escrow)? != plan.before
            || digest(&read(&active)?) != plan.after_digest
            || digest(&read(&escrow)?) != plan.before_digest
            || metadata(&active)? != plan.metadata
            || metadata(&escrow)? != plan.metadata
        {
            return Err(TransactionError::InvalidState(
                "shell removal conflict retained in private escrow",
            ));
        }
        self.home.sync_all().map_err(|_| TransactionError::Io)?;
        self.directory
            .sync_all()
            .map_err(|_| TransactionError::Io)?;
        self.check()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    fn fixture(prefix: &str) -> (tempfile::TempDir, Arc<HomeLease>, Receipt) {
        let temp = tempfile::tempdir().unwrap();
        let home = temp.path().canonicalize().unwrap().join("home");
        let state = home.with_file_name("state");
        fs::create_dir(&home).unwrap();
        fs::create_dir(&state).unwrap();
        fs::set_permissions(&state, fs::Permissions::from_mode(0o700)).unwrap();
        fs::write(home.join(".bashrc"), prefix).unwrap();
        let layout = Layout::new(state, home.join("bin"));
        let lease = HomeLease::acquire(home, &layout).unwrap();
        let receipt = lease
            .plan(
                ".bashrc".into(),
                path_block(&layout.command_directory).unwrap(),
            )
            .unwrap();
        lease.apply(&receipt).unwrap();
        (temp, lease, receipt)
    }

    #[test]
    fn removal_preserves_prefix_suffix_and_completed_retry() {
        let (_temp, lease, receipt) = fixture("export KEEP=one");
        let path = lease.claim.home.join(".bashrc");
        OpenOptions::new()
            .append(true)
            .open(&path)
            .unwrap()
            .write_all(b"export OTHER=two\n")
            .unwrap();
        let before = fs::read(&path).unwrap();
        let plan = lease.plan_removal(&receipt).unwrap();
        lease.remove(&plan).unwrap();
        lease.remove(&plan).unwrap();
        assert_eq!(
            fs::read(&path).unwrap(),
            b"export KEEP=one\nexport OTHER=two\n"
        );
        assert_eq!(
            fs::read(lease.claim.home.join(CLAIM).join(&plan.escrow)).unwrap(),
            before
        );
    }

    #[test]
    fn removal_refuses_editor_replacement_before_exchange() {
        let (_temp, lease, receipt) = fixture("original\n");
        let plan = lease.plan_removal(&receipt).unwrap();
        let path = lease.claim.home.join(".bashrc");
        fs::rename(&path, path.with_file_name("editor-backup")).unwrap();
        fs::write(&path, "editor\n").unwrap();
        assert!(lease.remove(&plan).is_err());
        assert_eq!(fs::read(&path).unwrap(), b"editor\n");
    }

    #[test]
    fn removal_retains_late_editor_writes_and_refuses_unknown_retry() {
        let (_temp, lease, receipt) = fixture("original\n");
        let path = lease.claim.home.join(".bashrc");
        let mut editor = OpenOptions::new().append(true).open(&path).unwrap();
        let plan = lease.plan_removal(&receipt).unwrap();
        lease.remove(&plan).unwrap();
        editor.write_all(b"late edit\n").unwrap();
        assert!(lease.remove(&plan).is_err());
        assert_eq!(fs::read(&path).unwrap(), b"original\n");
        assert!(
            fs::read(lease.claim.home.join(CLAIM).join(&plan.escrow))
                .unwrap()
                .ends_with(b"late edit\n")
        );
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn removal_refuses_unknown_xattr_before_active_mutation() {
        let (_temp, lease, receipt) = fixture("original\n");
        let path = lease.claim.home.join(".bashrc");
        let file = File::open(&path).unwrap();
        assert_eq!(
            unsafe {
                libc::fsetxattr(
                    file.as_raw_fd(),
                    c"user.neo-test".as_ptr(),
                    b"v".as_ptr().cast(),
                    1,
                    0,
                    0,
                )
            },
            0
        );
        let before = fs::read(&path).unwrap();
        assert!(lease.plan_removal(&receipt).is_err());
        assert_eq!(fs::read(&path).unwrap(), before);
    }
}
