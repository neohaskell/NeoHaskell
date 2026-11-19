use super::*;
use std::cell::RefCell;
use std::collections::btree_map::Entry;
use std::ffi::CString;

pub(super) struct ContinuitySources {
    configuration: PathBuf,
    service: PathBuf,
    secondary: UserSources,
}

pub(super) fn capture(inputs: &PolicyInputs) -> Result<ContinuitySources, PolicyResult> {
    let kernel = daemon_process::observe(inputs.pid, &inputs.executable, read)
        .map_err(|_| PolicyResult::InaccessibleInputs)?;
    Ok(ContinuitySources {
        configuration: inputs.source.clone(),
        service: inputs.service.clone(),
        secondary: kernel
            .conventional_user_sources()
            .ok_or(PolicyResult::UnknownLaunch)?,
    })
}

impl ContinuitySources {
    pub(super) fn verify(&self, expected: [u8; 32]) -> Result<(), PolicyResult> {
        if self.stamp()? != expected {
            return Err(PolicyResult::ChangedInputs);
        }
        Ok(())
    }

    pub(super) fn stamp(&self) -> Result<[u8; 32], PolicyResult> {
        let paths = RefCell::new(PathObservations::default());
        let protect = |path: &Path| paths.borrow_mut().protect(path, false);
        let configuration = configuration_input_with(&self.configuration, protect, read)
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        if !configuration.has_complete_stock_policy() {
            return Err(PolicyResult::ChangedInputs);
        }

        let identity = target_identity().map_err(|_| PolicyResult::UnprovedService)?;
        let packaged_units = Path::new(identity.package_root).join("lib/systemd/system");
        let units = [
            (self.service.clone(), identity.service_sha256),
            (
                PathBuf::from("/etc/systemd/system/nix-daemon.socket"),
                identity.socket_sha256,
            ),
            (
                packaged_units.join("nix-daemon.service"),
                identity.service_sha256,
            ),
            (
                packaged_units.join("nix-daemon.socket"),
                identity.socket_sha256,
            ),
        ];
        let mut unit_sources = Vec::new();
        for (path, expected) in units {
            let (unit, _) = configuration_source_with(
                &path,
                &|path| paths.borrow_mut().protect(path, true),
                &mut read,
            )
            .map_err(|_| PolicyResult::UnprovedService)?;
            if !matches!(&unit, ConfigurationFile::Present { digest, .. } if digest == expected) {
                return Err(PolicyResult::UnprovedService);
            }
            unit_sources.push((path, unit));
        }

        let mut account_sources = Vec::new();
        let secondary = match self.secondary {
            UserSources::Disabled => SecondarySources::Disabled,
            UserSources::Stock(home) => {
                if home == StockHome::Passwd {
                    account_sources = stock_account_sources(&protect)?;
                }
                let missing = |path: &str| {
                    let (source, _) = configuration_source_with(
                        Path::new(path),
                        &|path| {
                            let expected = protected_secondary_path(path)?;
                            let resolved = protect(path)?;
                            if resolved != expected {
                                return Err(io::Error::other(
                                    "secondary configuration path changed",
                                ));
                            }
                            Ok(resolved)
                        },
                        &mut read,
                    )
                    .map_err(|_| PolicyResult::InaccessibleInputs)?;
                    match source {
                        ConfigurationFile::Missing { .. } => Ok(source),
                        ConfigurationFile::Present { .. } => Err(PolicyResult::UnknownLaunch),
                    }
                };
                SecondarySources::Stock {
                    home: missing("/root/.config/nix/nix.conf")?,
                    xdg: missing("/etc/xdg/nix/nix.conf")?,
                }
            }
        };

        let paths = paths.into_inner();
        paths.revalidate()?;
        let bytes = serde_json::to_vec(&(
            &self.configuration,
            configuration.input,
            configuration.attachment,
            unit_sources,
            self.secondary,
            secondary,
            account_sources,
            paths,
        ))
        .map_err(|_| PolicyResult::InaccessibleInputs)?;
        let mut hash = Sha256::new();
        hash.update(b"NEO-POLICY-INPUT-CONTINUITY-V1\0");
        hash.update(bytes);
        Ok(hash.finalize().into())
    }
}

fn stock_account_sources(
    protect: &impl Fn(&Path) -> io::Result<PathBuf>,
) -> Result<Vec<ConfigurationFile>, PolicyResult> {
    let lookup = |path| {
        configuration_source_with(Path::new(path), protect, &mut read)
            .map_err(|_| PolicyResult::UnknownLaunch)
    };
    let (selector, nsswitch) = lookup("/etc/nsswitch.conf")?;
    let (account, passwd) = lookup("/etc/passwd")?;
    let (cache, _) = lookup("/var/run/nscd/socket")?;
    if !matches!(cache, ConfigurationFile::Missing { .. }) {
        return Err(PolicyResult::UnknownLaunch);
    }
    let nsswitch = std::str::from_utf8(&nsswitch).map_err(|_| PolicyResult::UnknownLaunch)?;
    let mut passwd_lookups = nsswitch.lines().filter_map(|line| {
        let line = line.split('#').next()?;
        let (database, entries) = line.split_once(':')?;
        (database.trim() == "passwd").then_some(entries)
    });
    let sources = passwd_lookups
        .next()
        .ok_or(PolicyResult::UnknownLaunch)?
        .split_ascii_whitespace()
        .collect::<Vec<_>>();
    if !matches!(sources.as_slice(), ["files"] | ["files", "systemd"])
        || passwd_lookups.next().is_some()
    {
        return Err(PolicyResult::UnknownLaunch);
    }

    let mut root = passwd.split(|byte| *byte == b'\n').filter_map(|line| {
        let fields = line.split(|byte| *byte == b':').collect::<Vec<_>>();
        (fields.get(2) == Some(&b"0".as_slice())).then_some(fields)
    });
    if !matches!(
        root.next().as_deref(),
        Some([_, _, b"0", _, _, b"/root", _])
    ) || root.next().is_some()
    {
        return Err(PolicyResult::UnknownLaunch);
    }
    managed::verify_stock_account_home()?;
    Ok(vec![selector, account, cache])
}

#[derive(Default, Serialize)]
struct PathObservations {
    components: BTreeMap<PathBuf, InputIdentity>,
    absence_parents: BTreeMap<PathBuf, InputIdentity>,
}

impl PathObservations {
    fn protect(&mut self, path: &Path, allow_shared_store: bool) -> io::Result<PathBuf> {
        let resolved =
            protected_configuration_path_with(path, allow_shared_store, |path, meta| {
                if !meta.is_symlink() {
                    require_timestamped_filesystem(path)?;
                }
                Self::record(&mut self.components, path, Self::component(path, meta))
            })?;
        let metadata = fs::symlink_metadata(&resolved)?;
        Self::record(
            &mut self.components,
            &resolved,
            Self::component(&resolved, &metadata),
        )?;
        if metadata.is_dir() {
            Self::record(
                &mut self.absence_parents,
                &resolved,
                Self::absence_parent(&metadata),
            )?;
        }
        Ok(resolved)
    }

    fn component(path: &Path, metadata: &fs::Metadata) -> InputIdentity {
        let mut stamp = InputIdentity::of(metadata);
        // Store additions change the directory's times. The /nix stamp guards its name.
        if metadata.is_dir() && path != Path::new("/") && path != Path::new("/nix/store") {
            stamp.changed = (metadata.ctime(), metadata.ctime_nsec());
        }
        stamp
    }

    fn absence_parent(metadata: &fs::Metadata) -> InputIdentity {
        let mut stamp = InputIdentity::of(metadata);
        stamp.modified = (metadata.mtime(), metadata.mtime_nsec());
        stamp.changed = (metadata.ctime(), metadata.ctime_nsec());
        stamp
    }

    fn record(
        observed: &mut BTreeMap<PathBuf, InputIdentity>,
        path: &Path,
        stamp: InputIdentity,
    ) -> io::Result<()> {
        match observed.entry(path.to_owned()) {
            Entry::Vacant(entry) => {
                entry.insert(stamp);
            }
            Entry::Occupied(entry) if entry.get() != &stamp => {
                return Err(io::Error::other("policy input changed during observation"));
            }
            Entry::Occupied(_) => {}
        }
        Ok(())
    }

    fn revalidate(&self) -> Result<(), PolicyResult> {
        let metadata = |path| fs::symlink_metadata(path).map_err(|_| PolicyResult::ChangedInputs);
        for (path, expected) in &self.components {
            if Self::component(path, &metadata(path)?) != *expected {
                return Err(PolicyResult::ChangedInputs);
            }
        }
        for (path, expected) in &self.absence_parents {
            if Self::absence_parent(&metadata(path)?) != *expected {
                return Err(PolicyResult::ChangedInputs);
            }
        }
        Ok(())
    }
}

fn require_timestamped_filesystem(path: &Path) -> io::Result<()> {
    let name = CString::new(path.as_os_str().as_encoded_bytes())?;
    let mut stats = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::statfs(name.as_ptr(), stats.as_mut_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    match unsafe { stats.assume_init() }.f_type {
        libc::BTRFS_SUPER_MAGIC
        | libc::EXT4_SUPER_MAGIC
        | libc::F2FS_SUPER_MAGIC
        | libc::OVERLAYFS_SUPER_MAGIC
        | libc::TMPFS_MAGIC
        | libc::XFS_SUPER_MAGIC => Ok(()),
        _ => Err(io::Error::other("unproved policy input filesystem")),
    }
}
