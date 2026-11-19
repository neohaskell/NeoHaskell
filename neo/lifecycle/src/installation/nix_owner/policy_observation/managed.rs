use super::*;
use std::os::fd::{AsRawFd, FromRawFd};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

#[derive(PartialEq, Eq, Serialize)]
pub(super) struct ManagedSnapshot {
    pub(super) pid: u32,
    pub(super) started: u64,
    pub(super) invocation: u128,
    kernel: KernelDaemonObservation,
    namespace: Vec<(u64, u64)>,
    pub(super) socket: (u64, u64),
    pub(super) listener: u64,
    units: Vec<ConfigurationFile>,
}

impl ManagedSnapshot {
    pub(super) fn is_replacement_of(&self, old: &Self) -> bool {
        self.pid != old.pid
            && self.invocation != old.invocation
            && self.started > old.started
            && self.socket == old.socket
            && self.listener == old.listener
            && self.namespace.first() == old.namespace.first()
            && self.units == old.units
            && matches!(
                fs::symlink_metadata(format!("/proc/{}", old.pid)),
                Err(error) if error.kind() == io::ErrorKind::NotFound
            )
    }
}

enum MountView {
    Current,
    Replacement,
}

struct ReplacementView {
    process: File,
    root: File,
    mount: File,
    snapshot: ManagedSnapshot,
}

fn file_identity(file: &File) -> Result<(u64, u64), PolicyResult> {
    let metadata = file.metadata().map_err(|_| PolicyResult::UnprovedService)?;
    Ok((metadata.dev(), metadata.ino()))
}

impl ReplacementView {
    fn pin(inputs: &PolicyInputs) -> Result<Self, PolicyResult> {
        let snapshot = inputs
            .managed_snapshot_in(MountView::Replacement)
            .and_then(|snapshot| snapshot.ok_or(PolicyResult::UnprovedService))?;
        let process = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC)
            .open(format!("/proc/{}", inputs.pid))
            .map_err(|_| PolicyResult::UnprovedService)?;
        let open = |name: &std::ffi::CStr, flags| {
            let descriptor = unsafe {
                libc::openat(process.as_raw_fd(), name.as_ptr(), flags | libc::O_CLOEXEC)
            };
            if descriptor < 0 {
                Err(PolicyResult::UnprovedService)
            } else {
                Ok(unsafe { File::from_raw_fd(descriptor) })
            }
        };
        let root = open(c"root", libc::O_RDONLY | libc::O_DIRECTORY)?;
        let mount = open(c"ns/mnt", libc::O_RDONLY)?;
        let pinned = Self {
            process,
            root,
            mount,
            snapshot,
        };
        pinned.revalidate(inputs)?;
        Ok(pinned)
    }

    fn revalidate(&self, inputs: &PolicyInputs) -> Result<(), PolicyResult> {
        let process = File::open(format!("/proc/{}", inputs.pid))
            .map_err(|_| PolicyResult::UnprovedService)?;
        if file_identity(&self.process)? != file_identity(&process)?
            || self.snapshot.namespace
                != vec![file_identity(&self.root)?, file_identity(&self.mount)?]
            || inputs.managed_snapshot_in(MountView::Replacement)?.as_ref() != Some(&self.snapshot)
        {
            return Err(PolicyResult::UnprovedService);
        }
        Ok(())
    }

    fn enter(&self) -> Result<(), PolicyResult> {
        if unsafe { libc::unshare(libc::CLONE_FS) } != 0
            || unsafe { libc::setns(self.mount.as_raw_fd(), libc::CLONE_NEWNS) } != 0
            || unsafe { libc::fchdir(self.root.as_raw_fd()) } != 0
            || unsafe { libc::chroot(c".".as_ptr()) } != 0
            || unsafe { libc::chdir(c"/".as_ptr()) } != 0
        {
            return Err(PolicyResult::UnprovedService);
        }
        Ok(())
    }
}

enum ManagerOperation {
    Service,
    Socket,
    Restart,
}

fn manager(operation: ManagerOperation) -> Result<Vec<u8>, PolicyResult> {
    let systemctl = protected_path(Path::new("/usr/bin/systemctl"))
        .map_err(|_| PolicyResult::UnprovedService)?;
    let mut command = Command::new(systemctl);
    match operation {
        ManagerOperation::Service | ManagerOperation::Socket => {
            command.args(["show", "--no-pager", "--property=Id,LoadState,ActiveState,SubState,NeedDaemonReload,FragmentPath,DropInPaths"]);
            if matches!(operation, ManagerOperation::Service) {
                command.args(["--property=MainPID,ExecMainStartTimestampMonotonic,InvocationID,ExecStart,User,RootDirectory", "nix-daemon.service"]);
            } else {
                command.args(["--property=Listen,Triggers", "nix-daemon.socket"]);
            }
        }
        ManagerOperation::Restart => {
            command.args(["--no-ask-password", "restart", "nix-daemon.service"]);
        }
    }
    let budget = match operation {
        ManagerOperation::Restart => Duration::from_secs(5),
        _ => Duration::from_millis(400),
    };
    bounded_output(command, budget)
}

fn bounded_output(mut command: Command, budget: Duration) -> Result<Vec<u8>, PolicyResult> {
    let mut child = command
        .env_clear()
        .current_dir("/")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|_| PolicyResult::UnprovedService)?;
    let mut stdout = child.stdout.take().ok_or(PolicyResult::UnprovedService)?;
    if unsafe { libc::fcntl(stdout.as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK) } < 0 {
        let _ = child.kill();
        let _ = child.wait();
        return Err(PolicyResult::UnprovedService);
    }
    let deadline = Instant::now() + budget;
    let mut output = Vec::new();
    let mut delay = Duration::from_millis(1);
    let result = loop {
        let mut buffer = [0; 4096];
        match stdout.read(&mut buffer) {
            Ok(n) => output.extend_from_slice(&buffer[..n]),
            Err(error) if error.kind() == io::ErrorKind::WouldBlock => (),
            Err(_) => break Err(PolicyResult::UnprovedService),
        }
        if output.len() > MAX_CONFIGURATION_BYTES || Instant::now() >= deadline {
            break Err(PolicyResult::UnprovedService);
        }
        match child.try_wait() {
            Ok(Some(status)) => {
                if !status.success() {
                    break Err(PolicyResult::UnprovedService);
                }
                loop {
                    match stdout.read(&mut buffer) {
                        Ok(0) => break,
                        Ok(n) if output.len() + n <= MAX_CONFIGURATION_BYTES => {
                            output.extend_from_slice(&buffer[..n])
                        }
                        _ => return Err(PolicyResult::UnprovedService),
                    }
                }
                break Ok(output);
            }
            Ok(None) => (),
            Err(_) => break Err(PolicyResult::UnprovedService),
        }
        std::thread::sleep(delay.min(deadline.saturating_duration_since(Instant::now())));
        delay = delay.saturating_mul(2);
    };
    if result.is_err() {
        let _ = child.kill();
        let _ = child.wait();
    }
    result
}

pub(super) fn verify_stock_account_home() -> Result<(), PolicyResult> {
    let getent =
        protected_path(Path::new("/usr/bin/getent")).map_err(|_| PolicyResult::UnknownLaunch)?;
    let mut command = Command::new(getent);
    command.args(["passwd", "0"]);
    let output = bounded_output(command, Duration::from_millis(400))
        .map_err(|_| PolicyResult::UnknownLaunch)?;
    let record = output
        .strip_suffix(b"\n")
        .ok_or(PolicyResult::UnknownLaunch)?;
    let fields = record.split(|byte| *byte == b':').collect::<Vec<_>>();
    if record.contains(&b'\n') || !matches!(fields.as_slice(), [_, _, b"0", _, _, b"/root", _]) {
        return Err(PolicyResult::UnknownLaunch);
    }
    Ok(())
}

fn verify_stock_executable(identity: &NixTargetIdentity) -> Result<(), PolicyResult> {
    let path = trusted_client().map_err(|_| PolicyResult::UnprovedService)?;
    let executable = OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_CLOEXEC)
        .open(&path)
        .map_err(|_| PolicyResult::UnprovedService)?;
    let metadata = executable
        .metadata()
        .map_err(|_| PolicyResult::UnprovedService)?;
    const MAX_EXECUTABLE_BYTES: u64 = 8 * 1024 * 1024;
    if !metadata.is_file() || metadata.len() > MAX_EXECUTABLE_BYTES {
        return Err(PolicyResult::UnprovedService);
    }
    let mut bytes = Vec::new();
    executable
        .take(MAX_EXECUTABLE_BYTES + 1)
        .read_to_end(&mut bytes)
        .map_err(|_| PolicyResult::UnprovedService)?;
    let current = fs::symlink_metadata(&path).map_err(|_| PolicyResult::UnprovedService)?;
    if bytes.len() as u64 != metadata.len()
        || InputIdentity::of(&metadata) != InputIdentity::of(&current)
        || trusted_client().map_err(|_| PolicyResult::UnprovedService)? != path
        || format!("{:x}", Sha256::digest(&bytes)) != identity.client_sha256
    {
        return Err(PolicyResult::UnprovedService);
    }
    Ok(())
}

pub(super) fn discover_stock_policy() -> Result<DiscoveredInputs, PolicyResult> {
    with_stock_inputs(|inputs| inputs.capture_discovery())
}

pub(super) fn with_stock_inputs<T: Send>(
    observe: impl FnOnce(&PolicyInputs) -> Result<T, PolicyResult> + Send,
) -> Result<T, PolicyResult> {
    run_in_stock_view(|inputs, view| {
        let result = observe(inputs)?;
        view.revalidate(inputs)?;
        Ok(result)
    })
}

pub(super) fn mutate_in_stock_view<T: Send>(
    mutate: impl FnOnce(&PolicyInputs) -> Result<T, PolicyResult> + Send,
) -> Result<T, PolicyResult> {
    run_in_stock_view(|inputs, _view| mutate(inputs))
}

fn run_in_stock_view<T: Send>(
    operation: impl FnOnce(&PolicyInputs, &ReplacementView) -> Result<T, PolicyResult> + Send,
) -> Result<T, PolicyResult> {
    if unsafe { libc::geteuid() } != 0 {
        return Err(PolicyResult::InaccessibleInputs);
    }
    let identity = target_identity().map_err(|_| PolicyResult::UnprovedService)?;
    let service = service_observation(&manager(ManagerOperation::Service)?, identity)
        .map_err(|_| PolicyResult::UnprovedService)?;
    let mut inputs = PolicyInputs::conventional_process(service.main_pid)?;
    inputs.managed = true;
    let view = ReplacementView::pin(&inputs)?;
    if view.snapshot.started != service.started
        || view.snapshot.invocation != service.invocation_id.get()
    {
        return Err(PolicyResult::ChangedInputs);
    }
    view.revalidate(&inputs)?;
    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .spawn_scoped(scope, || {
                view.revalidate(&inputs)?;
                view.enter()?;
                verify_stock_executable(identity)?;
                operation(&inputs, &view)
            })
            .map_err(|_| PolicyResult::UnprovedService)?
            .join()
            .unwrap_or(Err(PolicyResult::UnprovedService))
    })
}

fn managed_listener(pid: u32) -> Result<u64, PolicyResult> {
    let mut table = File::open("/proc/net/unix").map_err(|_| PolicyResult::UnprovedService)?;
    let bytes = read(&mut table).map_err(|_| PolicyResult::UnprovedService)?;
    let table = std::str::from_utf8(&bytes).map_err(|_| PolicyResult::UnprovedService)?;
    let mut matching = table.lines().filter_map(|line| {
        let columns = line.split_ascii_whitespace().collect::<Vec<_>>();
        match columns.as_slice() {
            [_, _, _, "00010000", "0001", "01", inode, SOCKET] => inode.parse::<u64>().ok(),
            _ => None,
        }
    });
    let inode = matching
        .next()
        .filter(|inode| *inode > 0)
        .ok_or(PolicyResult::UnprovedService)?;
    if matching.next().is_some() {
        return Err(PolicyResult::UnprovedService);
    }
    let descriptor_target = PathBuf::from(format!("socket:[{inode}]"));
    for owner in [1, pid] {
        let descriptors =
            fs::read_dir(format!("/proc/{owner}/fd")).map_err(|_| PolicyResult::UnprovedService)?;
        if !descriptors.take(512).flatten().any(|descriptor| {
            matches!(fs::read_link(descriptor.path()), Ok(target) if target == descriptor_target)
        }) {
            return Err(PolicyResult::UnprovedService);
        }
    }
    Ok(inode)
}

impl PolicyInputs {
    #[cfg(test)]
    pub(in crate::installation::nix_owner) fn set_managed_fixture(&mut self, enabled: bool) {
        self.managed = enabled;
    }

    pub(super) fn managed_snapshot(&self) -> Result<Option<ManagedSnapshot>, PolicyResult> {
        self.managed_snapshot_in(MountView::Current)
    }

    fn managed_snapshot_in(
        &self,
        view: MountView,
    ) -> Result<Option<ManagedSnapshot>, PolicyResult> {
        if !self.managed {
            return Ok(None);
        }
        let identity = target_identity().map_err(|_| PolicyResult::UnprovedService)?;
        if self.source != Path::new("/etc/nix/nix.conf")
            || self.service != Path::new("/etc/systemd/system/nix-daemon.service")
            || self
                .executable
                .canonicalize()
                .map_err(|_| PolicyResult::UnprovedService)?
                != Path::new(identity.package_root).join("bin/nix")
        {
            return Err(PolicyResult::UnprovedService);
        }
        let mut units = Vec::new();
        for (path, digest) in [
            (
                "/etc/systemd/system/nix-daemon.service",
                identity.service_sha256,
            ),
            (
                "/etc/systemd/system/nix-daemon.socket",
                identity.socket_sha256,
            ),
        ] {
            let unit = configuration_source_with(Path::new(path), &protected_path, &mut read)
                .map_err(|_| PolicyResult::UnprovedService)?
                .0;
            if !matches!(&unit, ConfigurationFile::Present { digest: actual, .. } if actual == digest)
            {
                return Err(PolicyResult::UnprovedService);
            }
            units.push(unit);
        }
        let service = service_observation(&manager(ManagerOperation::Service)?, identity)
            .map_err(|_| PolicyResult::UnprovedService)?;
        if service.main_pid != self.pid {
            return Err(PolicyResult::ChangedInputs);
        }
        socket_observation(&manager(ManagerOperation::Socket)?, identity)
            .map_err(|_| PolicyResult::UnprovedService)?;
        protected_path(Path::new(SOCKET).parent().unwrap())
            .map_err(|_| PolicyResult::UnprovedService)?;
        let socket = fs::symlink_metadata(SOCKET).map_err(|_| PolicyResult::UnprovedService)?;
        if !socket.file_type().is_socket() || socket.uid() != 0 {
            return Err(PolicyResult::UnprovedService);
        }
        let kernel = daemon_process::observe(self.pid, &self.executable, read)
            .map_err(|_| PolicyResult::UnprovedService)?;
        if kernel.conventional_user_sources().is_none()
            || self.command_line()? != b"nix-daemon\0--daemon\0"
        {
            return Err(PolicyResult::UnknownLaunch);
        }
        Ok(Some(ManagedSnapshot {
            pid: service.main_pid,
            started: service.started,
            invocation: service.invocation_id.get(),
            kernel,
            namespace: {
                let root = File::open("/").map_err(|_| PolicyResult::UnprovedService)?;
                let daemon_root = File::open(format!("/proc/{}/root", self.pid))
                    .map_err(|_| PolicyResult::UnprovedService)?;
                let daemon_mount = File::open(format!("/proc/{}/ns/mnt", self.pid))
                    .map_err(|_| PolicyResult::UnprovedService)?;
                if file_identity(&root)? != file_identity(&daemon_root)? {
                    return Err(PolicyResult::UnknownLaunch);
                }
                if matches!(view, MountView::Current) {
                    let current = File::open("/proc/thread-self/ns/mnt")
                        .map_err(|_| PolicyResult::UnprovedService)?;
                    if file_identity(&current)? != file_identity(&daemon_mount)? {
                        return Err(PolicyResult::UnknownLaunch);
                    }
                }
                vec![file_identity(&daemon_root)?, file_identity(&daemon_mount)?]
            },
            socket: (socket.dev(), socket.ino()),
            listener: managed_listener(self.pid)?,
            units,
        }))
    }

    pub(super) fn restart_managed(
        &self,
        old: &ManagedSnapshot,
        secondary: Option<[u8; 32]>,
        verify_published: &(impl Fn() -> Result<(), PolicyResult> + Sync),
    ) -> Result<ManagedSnapshot, PolicyResult> {
        if !self.managed || unsafe { libc::geteuid() } != 0 {
            return Err(PolicyResult::UnprovedService);
        }
        traced("manager restart", manager(ManagerOperation::Restart))?;
        let identity = target_identity().map_err(|_| PolicyResult::UnprovedService)?;
        let manager_after = traced(
            "manager show after restart",
            manager(ManagerOperation::Service),
        )?;
        let new_service = traced(
            "parse candidate service",
            service_observation(&manager_after, identity)
                .map_err(|_| PolicyResult::UnprovedService),
        )?;
        let replacement = Self {
            source: self.source.clone(),
            service: self.service.clone(),
            service_digest: self.service_digest.clone(),
            executable: self.executable.clone(),
            pid: new_service.main_pid,
            #[cfg(test)]
            fixture: self.fixture,
            managed: true,
        };
        let view = traced("pin candidate view", ReplacementView::pin(&replacement))?;
        if !view.snapshot.is_replacement_of(old)
            || view.snapshot.invocation != new_service.invocation_id.get()
            || view.snapshot.started != new_service.started
        {
            return traced(
                "replacement identity mismatch",
                Err(PolicyResult::UnprovedService),
            );
        }
        replacement.verify_managed_view(view, secondary, verify_published)
    }

    pub(super) fn inspect_managed_view(
        &self,
        secondary: Option<[u8; 32]>,
        verify_published: &(impl Fn() -> Result<(), PolicyResult> + Sync),
    ) -> Result<ManagedSnapshot, PolicyResult> {
        if !self.managed || unsafe { libc::geteuid() } != 0 {
            return Err(PolicyResult::UnprovedService);
        }
        let view = ReplacementView::pin(self)?;
        self.verify_managed_view(view, secondary, verify_published)
    }

    fn verify_managed_view(
        &self,
        view: ReplacementView,
        secondary: Option<[u8; 32]>,
        verify_published: &(impl Fn() -> Result<(), PolicyResult> + Sync),
    ) -> Result<ManagedSnapshot, PolicyResult> {
        let observed = std::thread::scope(|scope| {
            std::thread::Builder::new()
                .spawn_scoped(scope, || {
                    traced(
                        "stable candidate prior to namespace entry",
                        view.revalidate(self),
                    )?;
                    traced(
                        "enter replacement view",
                        view.enter().map_err(|_| PolicyResult::PublicationUnknown),
                    )?;
                    let verify = || {
                        verify_published().map_err(|_| PolicyResult::PublicationUnknown)?;
                        if let Some(expected) = secondary {
                            if self.secondary_commitment()? != expected {
                                return Err(PolicyResult::ChangedInputs);
                            }
                        }
                        Ok(())
                    };
                    verify()?;
                    let service = (|| {
                        if traced("inside replacement snapshot", self.managed_snapshot())?.as_ref()
                            != Some(&view.snapshot)
                        {
                            return traced(
                                "inside replacement snapshot changed",
                                Err(PolicyResult::UnprovedService),
                            );
                        }
                        traced("inside replacement view recheck", view.revalidate(self))
                    })();
                    verify()?;
                    service
                })
                .map_err(|_| PolicyResult::PublicationUnknown)?
                .join()
                .unwrap_or(Err(PolicyResult::PublicationUnknown))
        });
        verify_published().map_err(|_| PolicyResult::PublicationUnknown)?;
        observed?;
        traced("outside replacement final recheck", view.revalidate(self))?;
        Ok(view.snapshot)
    }
}

fn traced<T>(_stage: &str, result: Result<T, PolicyResult>) -> Result<T, PolicyResult> {
    #[cfg(test)]
    if let Err(error) = &result {
        eprintln!("managed restart fixture {_stage}: {error:?}");
    }
    result
}
