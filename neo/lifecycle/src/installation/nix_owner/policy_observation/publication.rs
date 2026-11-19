use super::super::include_inputs::recovery::{self, Evidence, Stage};
use super::*;
use std::ffi::CString;
use std::io::{Seek, SeekFrom, Write};
use std::os::fd::AsRawFd;
use std::os::unix::fs::DirBuilderExt;

mod receipt;
pub(in crate::installation::nix_owner) use receipt::PolicyAttempt;

const FS_IOC_GETFLAGS_64: libc::c_ulong = 0x80086601;
const FS_EXTENT_FL: libc::c_long = 0x80000;

const SOURCE: &str = "/etc/nix/nix.conf";
const SERVICE: &str = "/etc/systemd/system/nix-daemon.service";
const STAGE: &str = "/etc/nix/.neo-policy-publication";

#[cfg(test)]
pub(in crate::installation::nix_owner) struct FixturePublicationBarrier {
    pub caller: (i32, u64),
    pub helper: (i32, u64),
    pub sudo: (i32, u64),
}

enum PublicationMoment {
    BeforeChallenge,
    AfterPublication,
    AfterRestartIntent,
}

#[cfg(test)]
impl FixturePublicationBarrier {
    fn pause(&self, moment: PublicationMoment) -> Result<(), PolicyResult> {
        let root = Path::new(match moment {
            PublicationMoment::BeforeChallenge => "/run/neo-fixture-before-policy-challenge",
            PublicationMoment::AfterPublication => "/run/neo-fixture-frontend-loss",
            PublicationMoment::AfterRestartIntent => "/run/neo-fixture-after-policy-restart-intent",
        });
        let armed = root.join("armed");
        match fs::symlink_metadata(&armed) {
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(()),
            Err(_) => return Err(PolicyResult::PublicationUnknown),
            Ok(_) => {}
        }
        let held = directory(root)?;
        let metadata = held
            .metadata()
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        if metadata.gid() != 0 {
            return Err(PolicyResult::PublicationUnknown);
        }
        let marker = |name| -> Result<bool, PolicyResult> {
            anchored(&held, root)?;
            let file = match OpenOptions::new()
                .read(true)
                .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_CLOEXEC)
                .open(descriptor_path(&held, name))
            {
                Ok(file) => file,
                Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(false),
                Err(_) => return Err(PolicyResult::PublicationUnknown),
            };
            let m = file
                .metadata()
                .map_err(|_| PolicyResult::PublicationUnknown)?;
            if !m.is_file()
                || m.uid() != 0
                || m.gid() != 0
                || m.mode() & 0o022 != 0
                || m.nlink() != 1
            {
                return Err(PolicyResult::PublicationUnknown);
            }
            Ok(true)
        };
        if !marker("armed")? {
            return Ok(());
        }
        assert_eq!(self.helper.0, unsafe { libc::getpid() });
        let mut ready = OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
            .open(descriptor_path(&held, "ready.tmp"))
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        write!(
            ready,
            "{} {} {} {} {} {}\n",
            self.caller.0, self.caller.1, self.sudo.0, self.sudo.1, self.helper.0, self.helper.1,
        )
        .and_then(|_| ready.sync_all())
        .map_err(|_| PolicyResult::PublicationUnknown)?;
        anchored(&held, root)?;
        fs::rename(
            descriptor_path(&held, "ready.tmp"),
            descriptor_path(&held, "ready"),
        )
        .and_then(|_| held.sync_all())
        .map_err(|_| PolicyResult::PublicationUnknown)?;
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(6);
        let mut delay = std::time::Duration::from_millis(1);
        loop {
            if marker("release")? || std::time::Instant::now() >= deadline {
                return Ok(());
            }
            std::thread::sleep(
                delay.min(deadline.saturating_duration_since(std::time::Instant::now())),
            );
            delay *= 2;
        }
    }
}

fn plain(file: &File) -> Result<fs::Metadata, PolicyResult> {
    let m = file
        .metadata()
        .map_err(|_| PolicyResult::PublicationRefused)?;
    if !m.is_file()
        || m.uid() != 0
        || m.nlink() != 1
        || m.mode() & 0o7022 != 0
        || unsafe { libc::flistxattr(file.as_raw_fd(), std::ptr::null_mut(), 0) } != 0
    {
        return Err(PolicyResult::PublicationRefused);
    }
    let mut flags: libc::c_long = 0;
    let result = unsafe { libc::ioctl(file.as_raw_fd(), FS_IOC_GETFLAGS_64, &mut flags) };
    if (result == 0 && flags & !FS_EXTENT_FL != 0)
        || (result != 0
            && !matches!(
                io::Error::last_os_error().raw_os_error(),
                Some(libc::ENOTTY) | Some(libc::EOPNOTSUPP)
            ))
    {
        return Err(PolicyResult::PublicationRefused);
    }
    Ok(m)
}

fn open(path: &Path) -> Result<File, PolicyResult> {
    OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_CLOEXEC)
        .open(path)
        .map_err(|_| PolicyResult::PublicationRefused)
}

fn staged(path: &Path, bytes: &[u8], metadata: &fs::Metadata) -> Result<File, PolicyResult> {
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create_new(true)
        .mode(0o600)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(path)
        .map_err(|_| PolicyResult::PublicationRefused)?;
    if unsafe { libc::fchown(file.as_raw_fd(), metadata.uid(), metadata.gid()) } != 0
        || unsafe { libc::fchmod(file.as_raw_fd(), metadata.mode() & 0o777) } != 0
    {
        return Err(PolicyResult::PublicationRefused);
    }
    file.write_all(bytes)
        .and_then(|_| file.sync_all())
        .map_err(|_| PolicyResult::PublicationRefused)?;
    plain(&file)?;
    Ok(file)
}

fn descriptor_path(directory: &File, name: &str) -> PathBuf {
    PathBuf::from(format!("/proc/self/fd/{}/{}", directory.as_raw_fd(), name))
}

fn directory(path: &Path) -> Result<File, PolicyResult> {
    if protected_path(path).map_err(|_| PolicyResult::PublicationRefused)? != path {
        return Err(PolicyResult::PublicationRefused);
    }
    let file = OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_DIRECTORY | libc::O_CLOEXEC)
        .open(path)
        .map_err(|_| PolicyResult::PublicationRefused)?;
    let m = file
        .metadata()
        .map_err(|_| PolicyResult::PublicationRefused)?;
    if m.uid() != 0 || m.mode() & 0o022 != 0 {
        return Err(PolicyResult::PublicationRefused);
    }
    Ok(file)
}

fn anchored(file: &File, path: &Path) -> Result<(), PolicyResult> {
    let held = file
        .metadata()
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    let named = fs::symlink_metadata(path).map_err(|_| PolicyResult::PublicationUnknown)?;
    if InputIdentity::of(&held) != InputIdentity::of(&named) {
        return Err(PolicyResult::PublicationUnknown);
    }
    Ok(())
}

fn locked_parent(mode: libc::c_int) -> Result<File, PolicyResult> {
    let path = Path::new("/etc/nix");
    let parent = directory(path)?;
    if unsafe { libc::flock(parent.as_raw_fd(), mode | libc::LOCK_NB) } != 0 {
        return Err(PolicyResult::PublicationRefused);
    }
    anchored(&parent, path)?;
    Ok(parent)
}

struct UnchangedRestart {
    request: ReviewedPolicyRequest,
    configuration: ConfigurationObservation,
    old: super::managed::ManagedSnapshot,
    secondary: [u8; 32],
    caller: StockCaller,
}

struct StockApplyInputs {
    caller: StockCaller,
    secondary: [u8; 32],
}

impl UnchangedRestart {
    fn capture(
        inputs: &PolicyInputs,
        request: ReviewedPolicyRequest,
        caller: StockCaller,
    ) -> Result<Self, PolicyResult> {
        let (actual, configuration) = inputs.capture_unchanged()?;
        if actual != request {
            return Err(PolicyResult::ChangedInputs);
        }
        configuration.verify_caller_access(caller)?;
        let old = inputs
            .managed_snapshot()?
            .ok_or(PolicyResult::UnprovedService)?;
        Ok(Self {
            request,
            configuration,
            old,
            secondary: inputs.secondary_commitment()?,
            caller,
        })
    }

    fn recheck(&self, inputs: &PolicyInputs, parent: &File) -> Result<(), PolicyResult> {
        anchored(parent, Path::new("/etc/nix"))?;
        let (actual, configuration) = inputs.capture_unchanged()?;
        if actual != self.request
            || inputs.managed_snapshot()?.as_ref() != Some(&self.old)
            || inputs.secondary_commitment()? != self.secondary
        {
            return Err(PolicyResult::ChangedInputs);
        }
        configuration.verify_caller_access(self.caller)
    }

    fn restart(
        self,
        inputs: &PolicyInputs,
        parent: &File,
        mut writer: receipt::ReceiptWriter<'_>,
    ) -> Result<[u8; 32], PolicyResult> {
        self.recheck(inputs, parent)?;
        writer.verify_prepared_unchanged(&self.old, self.secondary)?;
        writer.queue_restart()?;
        let replacement = (|| {
            self.recheck(inputs, parent)?;
            writer.verify_unchanged()?;
            inputs.restart_managed(&self.old, Some(self.secondary), &|| {
                writer.verify_unchanged()?;
                self.configuration.verify_caller_access(self.caller)
            })
        })()
        .map_err(|_| PolicyResult::PublicationUnknown)?;
        writer.observe_restart(&replacement)?;
        writer.verify_unchanged()?;
        replacement_input_stamp(
            &self.configuration,
            replacement.pid,
            replacement.started,
            replacement.invocation,
            replacement.socket,
        )
    }
}

impl PolicyInputs {
    pub(super) fn unchanged_stage_absent(&self) -> Result<(), PolicyResult> {
        if !self.managed {
            return Err(PolicyResult::UnprovedService);
        }
        let parent = locked_parent(libc::LOCK_SH).map_err(|_| PolicyResult::InaccessibleInputs)?;
        receipt::require_absent(&descriptor_path(&parent, POLICY_STAGE))
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        anchored(&parent, Path::new("/etc/nix")).map_err(|_| PolicyResult::InaccessibleInputs)
    }

    pub(super) fn verify_existing_receipt(
        &self,
        caller_uid: u32,
        lease: crate::installation::PolicyStateLease,
        common: [u8; 32],
    ) -> Result<(), PolicyResult> {
        let parent = locked_parent(libc::LOCK_SH)?;
        let stage = directory(Path::new(STAGE))?;
        receipt::verify_existing(self, &parent, &stage, caller_uid, lease, common)
    }

    pub(in crate::installation::nix_owner) fn inspect_completed_attempt(
        &self,
        operation: &PolicyAttempt,
        request: ReviewedPolicyRequest,
        caller: Option<StockCaller>,
    ) -> PolicyInspectionResult {
        let inspect = || -> Result<PolicyInspectionResult, PolicyResult> {
            let parent = self.locked_stock_parent()?;
            let stage = directory(Path::new(STAGE))?;
            if receipt::verify_completed(self, &parent, &stage, operation, request).is_ok() {
                return Ok(PolicyInspectionResult::VerifiedApplied);
            }
            let writer = receipt::ReceiptWriter::reopen_prepared_unchanged(
                &parent, &stage, operation, request,
            )?;
            let restart = UnchangedRestart::capture(
                self,
                request,
                caller.ok_or(PolicyResult::InaccessibleInputs)?,
            )?;
            restart.recheck(self, &parent)?;
            writer.verify_prepared_unchanged(&restart.old, restart.secondary)?;
            Ok(PolicyInspectionResult::VerifiedPrepared)
        };
        inspect().unwrap_or(PolicyInspectionResult::Pending)
    }

    #[cfg(test)]
    pub(in crate::installation::nix_owner) fn publish_fixture(
        &self,
        request: ReviewedPolicyRequest,
    ) -> PolicyResult {
        match self.publish_guarded(request, None, None, |_| Ok(()), || Ok(())) {
            Ok(result) => result,
            Err(result) => result,
        }
    }

    #[cfg(test)]
    pub(in crate::installation::nix_owner) fn publish_after_commit(
        &self,
        request: ReviewedPolicyRequest,
        barrier: FixturePublicationBarrier,
        attempt: PolicyAttempt,
        require_commit: impl FnOnce() -> Result<(), PolicyResult>,
    ) -> PolicyResult {
        match self.publish_guarded(
            request,
            Some(attempt),
            None,
            |moment| barrier.pause(moment),
            require_commit,
        ) {
            Ok(result) | Err(result) => result,
        }
    }

    pub(super) fn publish_stock(
        &self,
        request: ReviewedPolicyRequest,
        attempt: PolicyAttempt,
        caller: StockCaller,
        require_commit: impl FnOnce() -> Result<(), PolicyResult>,
    ) -> Result<PolicyResult, PolicyResult> {
        let secondary = self.secondary_commitment()?;
        self.publish_guarded(
            request,
            Some(attempt),
            Some(StockApplyInputs { caller, secondary }),
            |_| Ok(()),
            require_commit,
        )
    }

    pub(super) fn apply_stock_unchanged(
        &self,
        request: ReviewedPolicyRequest,
        attempt: PolicyAttempt,
        caller: StockCaller,
        require_commit: impl FnOnce() -> Result<(), PolicyResult>,
    ) -> Result<[u8; 32], PolicyResult> {
        let parent = self.locked_stock_parent()?;
        let stage_path = descriptor_path(&parent, POLICY_STAGE);
        receipt::require_absent(&stage_path).map_err(|_| PolicyResult::PublicationRefused)?;
        let restart = UnchangedRestart::capture(self, request, caller)?;
        require_commit()?;
        restart.recheck(self, &parent)?;
        receipt::require_absent(&stage_path)?;
        fs::DirBuilder::new()
            .mode(0o700)
            .create(&stage_path)
            .map_err(|_| PolicyResult::PublicationRefused)?;
        let stage = directory(Path::new(STAGE))?;
        let writer = receipt::ReceiptWriter::prepare_unchanged(
            &parent,
            &stage,
            attempt,
            request,
            &restart.configuration,
            &restart.old,
            restart.secondary,
        )?;
        restart.restart(self, &parent, writer)
    }

    pub(super) fn resume_stock_unchanged(
        &self,
        request: ReviewedPolicyRequest,
        attempt: PolicyAttempt,
        caller: StockCaller,
        require_commit: impl FnOnce() -> Result<(), PolicyResult>,
    ) -> Result<[u8; 32], PolicyResult> {
        let parent = self.locked_stock_parent()?;
        let stage = directory(Path::new(STAGE))?;
        let writer =
            receipt::ReceiptWriter::reopen_prepared_unchanged(&parent, &stage, &attempt, request)?;
        let restart = UnchangedRestart::capture(self, request, caller)?;
        restart.recheck(self, &parent)?;
        writer.verify_prepared_unchanged(&restart.old, restart.secondary)?;
        require_commit()?;
        restart.restart(self, &parent, writer)
    }

    fn locked_stock_parent(&self) -> Result<File, PolicyResult> {
        if !self.managed
            || unsafe { libc::geteuid() } != 0
            || self.source != Path::new(SOURCE)
            || self.service != Path::new(SERVICE)
        {
            return Err(PolicyResult::PublicationRefused);
        }
        target_identity().map_err(|_| PolicyResult::PublicationRefused)?;
        locked_parent(libc::LOCK_EX)
    }

    fn publish_guarded(
        &self,
        request: ReviewedPolicyRequest,
        attempt: Option<PolicyAttempt>,
        stock: Option<StockApplyInputs>,
        pause: impl Fn(PublicationMoment) -> Result<(), PolicyResult>,
        require_commit: impl FnOnce() -> Result<(), PolicyResult>,
    ) -> Result<PolicyResult, PolicyResult> {
        if unsafe { libc::geteuid() } != 0
            || self.source != Path::new(SOURCE)
            || self.service != Path::new(SERVICE)
        {
            return Err(PolicyResult::PublicationRefused);
        }
        target_identity().map_err(|_| PolicyResult::PublicationRefused)?;
        let parent = locked_parent(libc::LOCK_EX)?;
        let old_managed = self.managed_snapshot()?;
        let (actual, baseline) = self.capture()?;
        if actual != request {
            return Err(PolicyResult::ChangedInputs);
        }
        let verify_caller = || {
            if let Some(stock) = &stock {
                baseline.verify_caller_access(stock.caller)?;
            }
            Ok::<(), PolicyResult>(())
        };
        verify_caller()?;
        let secondary = stock.as_ref().map(|inputs| inputs.secondary);
        let NixIncludePreparation::Append(desired) = &baseline.preparation else {
            return Err(PolicyResult::PublicationRefused);
        };
        if (secondary.is_some() && baseline.has_complete_stock_policy())
            || !matches!(
                baseline.attachment,
                include_inputs::AttachmentInputs::VerifiedAppend(_)
            )
        {
            return Err(PolicyResult::PublicationRefused);
        }
        let mut original = open(&descriptor_path(&parent, "nix.conf"))?;
        let metadata = plain(&original)?;
        let baseline_bytes = read(&mut original).map_err(|_| PolicyResult::PublicationRefused)?;
        let ConfigurationFile::Present { file: expected, .. } = &baseline.input else {
            return Err(PolicyResult::PublicationRefused);
        };
        if InputIdentity::of(&metadata) != *expected
            || format!("{:x}", Sha256::digest(&baseline_bytes)) != desired.observed_digest
        {
            return Err(PolicyResult::ChangedInputs);
        }
        pause(PublicationMoment::BeforeChallenge)?;
        anchored(&parent, Path::new("/etc/nix"))?;
        require_commit()?;
        anchored(&parent, Path::new("/etc/nix"))?;
        if self.review()? != request {
            return Err(PolicyResult::ChangedInputs);
        }
        let verify_secondary = || {
            verify_caller()?;
            if let Some(expected) = secondary {
                if self.secondary_commitment()? != expected {
                    return Err(PolicyResult::ChangedInputs);
                }
            }
            Ok(())
        };
        verify_secondary()?;
        fs::DirBuilder::new()
            .mode(0o700)
            .create(descriptor_path(&parent, POLICY_STAGE))
            .map_err(|_| PolicyResult::PublicationRefused)?;
        let stage = directory(Path::new(STAGE))?;
        let source_slot = Path::new(STAGE).join("main-slot");
        let leaf_slot = Path::new(STAGE).join("leaf-slot");
        let source_anchor = staged(
            &descriptor_path(&stage, "main-slot"),
            &desired.source,
            &metadata,
        )?;
        let leaf_anchor = staged(
            &descriptor_path(&stage, "leaf-slot"),
            &desired.leaf,
            &metadata,
        )?;
        let source = configuration_source_with(&source_slot, &protected_path, &mut read)
            .map_err(|_| PolicyResult::PublicationRefused)?
            .0;
        let leaf = configuration_source_with(&leaf_slot, &protected_path, &mut read)
            .map_err(|_| PolicyResult::PublicationRefused)?
            .0;
        let evidence = Evidence {
            baseline: &baseline,
            baseline_bytes: &baseline_bytes,
            source_slot: &source_slot,
            source: &source,
            leaf_slot: &leaf_slot,
            leaf: &leaf,
            leaf_anchor: &leaf_anchor,
        };
        let check = |expected| -> Result<(), PolicyResult> {
            anchored(&parent, Path::new("/etc/nix"))?;
            anchored(&stage, Path::new(STAGE))?;
            if recovery::observe(
                Path::new(SOURCE),
                &evidence,
                &protected_configuration_path,
                &mut read,
            )
            .map_err(|_| PolicyResult::PublicationUnknown)?
                != expected
            {
                return Err(PolicyResult::PublicationUnknown);
            }
            Ok(())
        };
        let sync = || {
            stage
                .sync_all()
                .and_then(|_| parent.sync_all())
                .map_err(|_| PolicyResult::PublicationUnknown)
        };
        sync()?;
        if self.review()? != request {
            return Err(PolicyResult::ChangedInputs);
        }
        plain(&original)?;
        plain(&source_anchor)?;
        plain(&leaf_anchor)?;
        anchored(&source_anchor, &source_slot)?;
        anchored(&original, Path::new(SOURCE))?;
        check(Stage::Baseline)?;
        if old_managed != self.managed_snapshot()? {
            return Err(PolicyResult::ChangedInputs);
        }
        let mut receipt = match attempt {
            Some(attempt) => {
                let old = old_managed.as_ref().ok_or(PolicyResult::UnprovedService)?;
                let receipt = receipt::ReceiptWriter::prepare(
                    &parent, &stage, attempt, request, &evidence, old, secondary,
                )?;
                check(Stage::Baseline)?;
                Some(receipt)
            }
            None => None,
        };
        verify_secondary()?;
        fs::hard_link(
            descriptor_path(&stage, "leaf-slot"),
            descriptor_path(&parent, POLICY_LEAF),
        )
        .map_err(|_| PolicyResult::PublicationUnknown)?;
        sync()?;
        check(Stage::CompanionOnly)?;
        original
            .seek(SeekFrom::Start(0))
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        if read(&mut original).map_err(|_| PolicyResult::PublicationUnknown)? != baseline_bytes {
            return Err(PolicyResult::PublicationUnknown);
        }
        verify_secondary()?;
        let name = CString::new("nix.conf").unwrap();
        let slot = CString::new("main-slot").unwrap();
        if unsafe {
            libc::syscall(
                libc::SYS_renameat2,
                parent.as_raw_fd(),
                name.as_ptr(),
                stage.as_raw_fd(),
                slot.as_ptr(),
                libc::RENAME_EXCHANGE,
            )
        } != 0
        {
            return Err(PolicyResult::PublicationUnknown);
        }
        sync()?;
        check(Stage::SourcePublishedRetained)?;
        anchored(&source_anchor, Path::new(SOURCE))?;
        anchored(&original, &source_slot)?;
        fs::remove_file(descriptor_path(&stage, "leaf-slot"))
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        sync()?;
        let verify_published = || {
            verify_caller()?;
            check(Stage::SourcePublishedRetired)?;
            anchored(&source_anchor, Path::new(SOURCE))?;
            anchored(&original, &source_slot)?;
            plain(&original).map_err(|_| PolicyResult::PublicationUnknown)?;
            plain(&source_anchor).map_err(|_| PolicyResult::PublicationUnknown)?;
            plain(&leaf_anchor).map_err(|_| PolicyResult::PublicationUnknown)?;
            Ok::<(), PolicyResult>(())
        };
        verify_published()?;
        if let Some(old) = old_managed {
            let current = self.managed_snapshot();
            verify_published()?;
            if matches!(current, Ok(Some(actual)) if actual == old) {
                pause(PublicationMoment::AfterPublication)?;
                verify_published()?;
                verify_secondary()?;
                if let Some(receipt) = &mut receipt {
                    receipt.queue_restart()?;
                    pause(PublicationMoment::AfterRestartIntent)?;
                    if !matches!(self.managed_snapshot(), Ok(Some(actual)) if actual == old) {
                        return Ok(PolicyResult::SourcePublishedApplicationPending);
                    }
                    verify_published()?;
                }
                verify_secondary()?;
                let observed_restart = self.restart_managed(&old, secondary, &verify_published);
                verify_published()?;
                match observed_restart {
                    Err(PolicyResult::PublicationUnknown) => {
                        return Err(PolicyResult::PublicationUnknown);
                    }
                    Ok(replacement) => {
                        if let Some(receipt) = &mut receipt {
                            receipt.observe_restart(&replacement)?;
                        }
                        return Ok(PolicyResult::ManagedRestartObserved);
                    }
                    Err(_) => {}
                }
            }
        }
        Ok(PolicyResult::SourcePublishedApplicationPending)
    }
}

#[test]
#[ignore = "requires the explicit disposable root daemon fixture; performs no publication"]
fn publication_refuses_stale_commitment_and_held_lock() {
    assert_eq!(
        std::env::var("NEO_TEST_POLICY_PUBLISH").as_deref(),
        Ok("disposable-root-fixture")
    );
    assert_eq!(unsafe { libc::geteuid() }, 0);
    let inputs = PolicyInputs::fixture(
        PathBuf::from(SOURCE),
        PathBuf::from(SERVICE),
        PathBuf::from(std::env::var_os("NEO_TEST_DAEMON_EXECUTABLE").unwrap()),
        std::env::var("NEO_TEST_DAEMON_PID")
            .unwrap()
            .parse()
            .unwrap(),
    );
    let request = inputs.review().unwrap();
    let original = fs::read(SOURCE).unwrap();
    let mut stale = request;
    stale.0[0] ^= 1;
    assert_eq!(inputs.publish_fixture(stale), PolicyResult::ChangedInputs);
    let parent = directory(Path::new("/etc/nix")).unwrap();
    assert_eq!(
        unsafe { libc::flock(parent.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) },
        0
    );
    assert_eq!(
        inputs.publish_fixture(request),
        PolicyResult::PublicationRefused
    );
    assert_eq!(fs::read(SOURCE).unwrap(), original);
    assert!(!Path::new(STAGE).exists());
    assert!(!Path::new("/etc/nix/neohaskell-cache-policy.conf").exists());
}
