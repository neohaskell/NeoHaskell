use super::*;
use std::io::Read;
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::process::{Command, Stdio};

const ROOT_HELPER: &str =
    "installation::nix_owner::peer_handoff::launcher::admin_fixture::root_sudo_helper";
const ADMIN_REQUEST: &[u8; 16] = b"NEO-ADMIN-REQ001";
const ADMIN_TIMEOUT: Duration = Duration::from_secs(15);

struct AdminAuthority {
    executable: ExactExecutable,
    policy: PolicyInputs,
}

struct SudoAncestry {
    caller: PeerIdentity,
    sudo_pid: i32,
    process_chain: Vec<(i32, String)>,
}

fn root_owned(path: &Path) -> fs::Metadata {
    let metadata = fs::symlink_metadata(path).unwrap();
    assert!(!metadata.file_type().is_symlink());
    assert_eq!(
        (metadata.uid(), metadata.gid(), metadata.mode() & 0o022),
        (0, 0, 0)
    );
    metadata
}

fn exact_process(executable: &ExactExecutable, pid: i32) {
    assert!(pid > 0);
    executable.recheck().unwrap();
    let opened = ExactExecutable::open(&executable.path, &executable.release).unwrap();
    assert_eq!(
        filesystem::helper_snapshot(&opened.identity),
        filesystem::helper_snapshot(&executable.identity)
    );
    let running = fs::metadata(format!("/proc/{pid}/exe")).unwrap();
    assert_eq!(
        filesystem::helper_snapshot(&running),
        filesystem::helper_snapshot(&executable.identity)
    );
}

impl AdminAuthority {
    fn read() -> Self {
        root_owned(Path::new("/"));
        root_owned(Path::new("/etc"));
        let path = Path::new("/etc/neo-fixture-admin");
        let before = root_owned(path);
        assert!(before.is_file() && before.len() <= 8192);
        let file = fs::OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
            .open(path)
            .unwrap();
        assert_eq!(
            filesystem::helper_snapshot(&before),
            filesystem::helper_snapshot(&file.metadata().unwrap())
        );
        let mut text = String::new();
        file.take(8193).read_to_string(&mut text).unwrap();
        assert!(text.len() <= 8192 && text.ends_with('\n'));
        assert_eq!(
            filesystem::helper_snapshot(&before),
            filesystem::helper_snapshot(&root_owned(path))
        );
        let lines: Vec<_> = text.lines().collect();
        assert_eq!(lines.len(), 7);
        let release = crate::candidate::FileIdentity {
            name: "neo-policy-helper".into(),
            sha: lines[1].into(),
            size: lines[2].parse().unwrap(),
        };
        let executable = ExactExecutable::open(Path::new(lines[0]), &release).unwrap();
        exact_process(&executable, unsafe { libc::getpid() });
        for path in &lines[3..6] {
            assert!(Path::new(path).is_absolute());
        }
        let mut policy = PolicyInputs::fixture(
            lines[3].into(),
            lines[4].into(),
            lines[5].into(),
            lines[6].parse().unwrap(),
        );
        policy.set_managed_fixture(true);
        Self { executable, policy }
    }
}

fn process_stat(pid: i32) -> (i32, String) {
    let value = fs::read_to_string(format!("/proc/{pid}/stat")).unwrap();
    let (_, fields) = value.rsplit_once(") ").unwrap();
    let fields: Vec<_> = fields.split_whitespace().collect();
    (fields[1].parse().unwrap(), fields[19].to_owned())
}

impl SudoAncestry {
    fn verify(caller: PeerIdentity, executable: &ExactExecutable) -> Self {
        assert_eq!((caller.uid, caller.gid), (1001, 1001));
        exact_process(executable, caller.pid);
        let sudo = root_owned(Path::new("/usr/bin/sudo"));
        assert!(sudo.is_file() && sudo.mode() & 0o4000 != 0);
        let mut pid = unsafe { libc::getpid() };
        let mut process_chain = Vec::new();
        let mut sudo_pid = None;
        for _ in 0..16 {
            let (parent, start) = process_stat(pid);
            process_chain.push((pid, start));
            if pid == caller.pid {
                return Self {
                    caller,
                    sudo_pid: sudo_pid.expect("live exact sudo ancestor"),
                    process_chain,
                };
            }
            let running = fs::metadata(format!("/proc/{pid}/exe")).unwrap();
            if (running.dev(), running.ino()) == (sudo.dev(), sudo.ino()) {
                let status = fs::read_to_string(format!("/proc/{pid}/status")).unwrap();
                let uid = status
                    .lines()
                    .find(|line| line.starts_with("Uid:"))
                    .unwrap();
                assert_eq!(uid.split_whitespace().nth(2), Some("0"));
                sudo_pid = Some(pid);
            }
            assert!(parent > 1 && parent != pid);
            pid = parent;
        }
        panic!("ordinary caller not in bounded live sudo ancestry");
    }

    fn recheck(&self, executable: &ExactExecutable) {
        let current = Self::verify(self.caller, executable);
        assert_eq!(self.sudo_pid, current.sudo_pid);
        assert_eq!(self.process_chain, current.process_chain);
    }
}

fn arm(socket: &OwnedFd) {
    let enabled: i32 = 1;
    assert_eq!(
        unsafe {
            libc::setsockopt(
                socket.as_raw_fd(),
                libc::SOL_SOCKET,
                libc::SO_PASSCRED,
                (&enabled as *const i32).cast(),
                size_of::<i32>() as libc::socklen_t,
            )
        },
        0
    );
}

fn send(socket: &OwnedFd, domain: &[u8; 16], epoch: &[u8; 32]) {
    let mut frame = [0; super::super::FRAME_BYTES];
    frame[..16].copy_from_slice(domain);
    frame[16..].copy_from_slice(epoch);
    assert_eq!(
        unsafe {
            libc::send(
                socket.as_raw_fd(),
                frame.as_ptr().cast(),
                frame.len(),
                libc::MSG_NOSIGNAL,
            )
        },
        frame.len() as isize
    );
}

fn wait_readable(socket: &OwnedFd) {
    let mut poll = libc::pollfd {
        fd: socket.as_raw_fd(),
        events: libc::POLLIN,
        revents: 0,
    };
    assert_eq!(
        unsafe { libc::poll(&mut poll, 1, ADMIN_TIMEOUT.as_millis() as i32) },
        1
    );
    assert_ne!(poll.revents & libc::POLLIN, 0);
}

fn reap(child: &mut ChildLease) -> std::process::ExitStatus {
    let deadline = Instant::now() + ADMIN_TIMEOUT;
    let mut delay = Duration::from_millis(1);
    loop {
        if let Some(status) = child.0.try_wait().unwrap() {
            return status;
        }
        assert!(Instant::now() < deadline, "sudo exit timeout");
        std::thread::sleep(delay.min(deadline.saturating_duration_since(Instant::now())));
        delay *= 2;
    }
}

enum ManagedInstruction {
    Discover,
    Compare(ReviewedPolicyRequest),
    Publish(ReviewedPolicyRequest),
    Inspect {
        attempt: [u8; 32],
        reviewed: ReviewedPolicyRequest,
    },
}

impl ManagedInstruction {
    fn decode(frame: &[u8]) -> Option<Self> {
        if frame == DISCOVER {
            return Some(Self::Discover);
        }
        if frame.len() == 80 && frame.starts_with(INSPECT) {
            let attempt = frame[16..48].try_into().ok()?;
            let reviewed = ReviewedPolicyRequest::from_bytes(frame[48..80].try_into().ok()?);
            return Some(Self::Inspect { attempt, reviewed });
        }
        for (domain, publish) in [(REQUEST, false), (PUBLISH, true)] {
            if frame.len() == domain.len() + 32 && frame.starts_with(domain) {
                let commitment =
                    ReviewedPolicyRequest::from_bytes(frame[domain.len()..].try_into().ok()?);
                return Some(if publish {
                    Self::Publish(commitment)
                } else {
                    Self::Compare(commitment)
                });
            }
        }
        None
    }
}

pub(super) enum ManagedFixtureRequest<'a> {
    Compare(ReviewedPolicyRequest),
    Discover,
    Publish(ApprovedPublish<'a>),
    Inspect {
        attempt: [u8; 32],
        reviewed: ReviewedPolicyRequest,
    },
}

pub(super) fn observe_managed(
    executable: &ExactExecutable,
    rendezvous: Rendezvous,
    request: ManagedFixtureRequest<'_>,
) -> Result<QuiescentReply, HandoffError> {
    assert_eq!(
        unsafe {
            (
                libc::getuid(),
                libc::geteuid(),
                libc::getgid(),
                libc::getegid(),
            )
        },
        (1001, 1001, 1001, 1001)
    );
    exact_process(executable, unsafe { libc::getpid() });
    let epoch = rendezvous.epoch;
    let mut command = sudo_command(executable);
    command.env("NEO_TEST_ADMIN_EPOCH", name(&epoch));
    command.stdin(File::open("/tmp/neo-fixture-password").map_err(|_| HandoffError::Executable)?);
    let mut child = ChildLease(command.spawn().map_err(|_| HandoffError::Executable)?);
    wait_readable(&rendezvous.listener);
    if child
        .0
        .try_wait()
        .map_err(|_| HandoffError::Transport)?
        .is_some()
    {
        return Err(HandoffError::Peer);
    }
    let fd = unsafe {
        libc::accept4(
            rendezvous.listener.as_raw_fd(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
        )
    };
    if fd < 0 {
        return Err(HandoffError::Transport);
    }
    let socket = unsafe { OwnedFd::from_raw_fd(fd) };
    let identity = linux::connected_identity(&socket)?;
    if identity.pid <= 0 || (identity.uid, identity.gid) != (0, 0) {
        return Err(HandoffError::Peer);
    }
    let expected = PeerExpectation { identity, epoch };
    wait_readable(&socket);
    linux::receive_inner(&socket, &expected)?;
    executable.recheck()?;
    let endpoint = PeerEndpoint { socket };
    let request = match request {
        ManagedFixtureRequest::Compare(reviewed) => HelperRequest::Compare(reviewed),
        ManagedFixtureRequest::Discover => HelperRequest::Discover,
        ManagedFixtureRequest::Inspect { attempt, reviewed } => {
            HelperRequest::Inspect { attempt, reviewed }
        }
        ManagedFixtureRequest::Publish(approved) => {
            return endpoint.publish_managed(child, executable, expected, approved, ADMIN_TIMEOUT);
        }
    };
    endpoint.observe(child, executable, expected, request, ADMIN_TIMEOUT)
}

#[test]
fn root_sudo_helper() {
    let Ok(encoded) = std::env::var("NEO_TEST_ADMIN_EPOCH") else {
        return;
    };
    assert_eq!(
        unsafe {
            (
                libc::getuid(),
                libc::geteuid(),
                libc::getgid(),
                libc::getegid(),
            )
        },
        (0, 0, 0, 0)
    );
    let authority = AdminAuthority::read();
    let hex = encoded.strip_prefix("neo-review-").unwrap();
    assert_eq!(hex.len(), 64);
    let mut epoch = [0; 32];
    for (index, byte) in epoch.iter_mut().enumerate() {
        *byte = u8::from_str_radix(&hex[index * 2..index * 2 + 2], 16).unwrap();
    }
    let socket = socket(libc::SOCK_SEQPACKET).unwrap();
    arm(&socket);
    let (address, length) = address(&epoch);
    assert_eq!(
        unsafe {
            libc::connect(
                socket.as_raw_fd(),
                (&address as *const libc::sockaddr_un).cast(),
                length,
            )
        },
        0
    );
    let caller = linux::connected_identity(&socket).unwrap();
    let ancestry = SudoAncestry::verify(caller, &authority.executable);
    send(&socket, super::super::DOMAIN, &epoch);
    wait_readable(&socket);
    ancestry.recheck(&authority.executable);
    let mut request = [0; 80];
    let count = linux::receive_request_from(&socket, &mut request, caller).unwrap();
    let request = &request[..count];
    let instruction = if request.len() == super::super::FRAME_BYTES
        && &request[..16] == ADMIN_REQUEST
        && request[16..] == epoch
    {
        None
    } else {
        Some(ManagedInstruction::decode(request).expect("closed managed instruction"))
    };
    ancestry.recheck(&authority.executable);
    exact_process(&authority.executable, unsafe { libc::getpid() });
    let mut frame = Vec::from(*super::super::DOMAIN);
    frame.extend_from_slice(&epoch);
    match instruction {
        None => {
            authority
                .policy
                .discover()
                .expect("root independently discovers pinned managed policy");
            frame[..16].copy_from_slice(REPLY);
        }
        Some(ManagedInstruction::Discover) => {
            frame[..16].copy_from_slice(DISCOVER);
            let payload = match authority.policy.discover() {
                Ok(inputs) => inputs.encode(),
                Err(reason) => {
                    let mut bytes = [0; 65];
                    bytes[0] = reason as u8;
                    bytes
                }
            };
            frame.extend_from_slice(&payload);
        }
        Some(ManagedInstruction::Inspect { attempt, reviewed }) => {
            let operation = super::super::super::policy_observation::PolicyAttempt::authenticated(
                caller.uid,
                attempt,
                &authority.executable.path,
                &authority.executable.release,
                &authority.executable.identity,
                None,
            );
            let result = authority
                .policy
                .inspect_completed_attempt(&operation, reviewed);
            eprintln!("protected Nix policy inspection {result:?}");
            frame[..16].copy_from_slice(INSPECTION);
            frame[15] = result as u8;
        }
        Some(instruction) => {
            let result = match instruction {
                ManagedInstruction::Compare(commitment) => authority.policy.evaluate(commitment),
                ManagedInstruction::Publish(commitment) => {
                    let identity = |pid| {
                        let (_, start) = ancestry
                            .process_chain
                            .iter()
                            .find(|(id, _)| *id == pid)
                            .unwrap();
                        (pid, start.parse::<u64>().unwrap())
                    };
                    let barrier =
                        super::super::super::policy_observation::FixturePublicationBarrier {
                            caller: identity(ancestry.caller.pid),
                            helper: identity(unsafe { libc::getpid() }),
                            sudo: identity(ancestry.sudo_pid),
                        };
                    authority.policy.publish_after_commit(
                        commitment,
                        barrier,
                        super::super::super::policy_observation::PolicyAttempt::authenticated(
                            caller.uid,
                            epoch,
                            &authority.executable.path,
                            &authority.executable.release,
                            &authority.executable.identity,
                            None,
                        ),
                        || {
                            let result = policy_commit::PublishChallenge::new(epoch, commitment)
                                .and_then(|challenge| {
                                    challenge.require_commit(
                                        &socket,
                                        caller,
                                        Instant::now() + ADMIN_TIMEOUT,
                                    )
                                });
                            result.map_err(|error| {
                                eprintln!("policy publication commit refused: {error:?}");
                                PolicyResult::PublicationRefused
                            })?;
                            ancestry.recheck(&authority.executable);
                            Ok(())
                        },
                    )
                }
                ManagedInstruction::Discover | ManagedInstruction::Inspect { .. } => unreachable!(),
            };
            frame[..16].copy_from_slice(b"NEO-POLICY-V001\0");
            frame[15] = result as u8;
        }
    }
    ancestry.recheck(&authority.executable);
    exact_process(&authority.executable, unsafe { libc::getpid() });
    assert_eq!(
        unsafe {
            libc::send(
                socket.as_raw_fd(),
                frame.as_ptr().cast(),
                frame.len(),
                libc::MSG_NOSIGNAL,
            )
        },
        frame.len() as isize
    );
}

fn sudo_command(executable: &ExactExecutable) -> Command {
    let mut command = Command::new("/usr/bin/sudo");
    command
        .env_clear()
        .env("PATH", "/usr/sbin:/usr/bin:/sbin:/bin")
        .env("LC_ALL", "C");
    command.arg("-k");
    command.arg("-S").arg("-p").arg("[neo fixture password]");
    command
        .arg("--")
        .arg(&executable.path)
        .arg("--exact")
        .arg(ROOT_HELPER)
        .arg("--nocapture")
        .arg("--test-threads=1");
    command
}

#[test]
fn inspection_request_requires_exact_framing() {
    let mut frame = INSPECT.to_vec();
    frame.extend_from_slice(&[1; 32]);
    frame.extend_from_slice(&[2; 32]);
    assert!(matches!(
        ManagedInstruction::decode(&frame),
        Some(ManagedInstruction::Inspect { attempt, reviewed })
            if attempt == [1; 32] && reviewed.bytes() == &[2; 32]
    ));
    assert!(ManagedInstruction::decode(&frame[..79]).is_none());
    frame.push(0);
    assert!(ManagedInstruction::decode(&frame).is_none());
}

#[test]
#[ignore = "requires a disposable guest with the exact PASSWD sudoers rule"]
fn ordinary_user_sudo_handoff() {
    assert_eq!(
        std::env::var("NEO_TEST_ADMIN_HANDOFF").as_deref(),
        Ok("disposable-guest")
    );
    assert_eq!(
        unsafe {
            (
                libc::getuid(),
                libc::geteuid(),
                libc::getgid(),
                libc::getegid(),
            )
        },
        (1001, 1001, 1001, 1001)
    );
    let path = PathBuf::from(std::env::var_os("NEO_TEST_PINNED_STORE").unwrap());
    let release = crate::candidate::FileIdentity {
        name: "neo-policy-helper".into(),
        sha: std::env::var("NEO_TEST_PINNED_SHA").unwrap(),
        size: std::env::var("NEO_TEST_PINNED_SIZE")
            .unwrap()
            .parse()
            .unwrap(),
    };
    let executable = ExactExecutable::open(&path, &release).unwrap();
    exact_process(&executable, unsafe { libc::getpid() });
    let mut cold = Command::new("/usr/bin/sudo");
    cold.env_clear()
        .env("PATH", "/usr/sbin:/usr/bin:/sbin:/bin")
        .env("LC_ALL", "C");
    cold.args(["-n", "-k", "--"])
        .arg(&path)
        .arg("--exact")
        .arg(ROOT_HELPER)
        .args(["--nocapture", "--test-threads=1"])
        .stdin(Stdio::null());
    let mut cold = ChildLease(cold.spawn().unwrap());
    assert!(
        !reap(&mut cold).success(),
        "cold authentication must refuse"
    );
    let rendezvous = Rendezvous::fresh().unwrap();
    let epoch = rendezvous.epoch;
    exact_process(&executable, unsafe { libc::getpid() });
    let mut command = sudo_command(&executable);
    command.env("NEO_TEST_ADMIN_EPOCH", name(&epoch));
    command.stdin(File::open("/tmp/neo-fixture-password").unwrap());
    let mut child = ChildLease(command.spawn().unwrap());
    let sudo_pid = child.0.id();
    wait_readable(&rendezvous.listener);
    assert!(child.0.try_wait().unwrap().is_none());
    let fd = unsafe {
        libc::accept4(
            rendezvous.listener.as_raw_fd(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
        )
    };
    assert!(fd >= 0);
    let socket = unsafe { OwnedFd::from_raw_fd(fd) };
    let peer = linux::connected_identity(&socket).unwrap();
    assert!(peer.pid > 0);
    assert_eq!((peer.uid, peer.gid), (0, 0));
    let expected = PeerExpectation {
        identity: peer,
        epoch,
    };
    wait_readable(&socket);
    linux::receive_inner(&socket, &expected).unwrap();
    send(&socket, ADMIN_REQUEST, &epoch);
    wait_readable(&socket);
    linux::receive_domain(&socket, &expected, REPLY).unwrap();
    assert!(reap(&mut child).success());
    linux::require_eof(&socket).unwrap();
    exact_process(&executable, unsafe { libc::getpid() });
    assert_eq!(
        unsafe { libc::waitpid(sudo_pid as i32, std::ptr::null_mut(), libc::WNOHANG) },
        -1
    );
    assert_eq!(
        std::io::Error::last_os_error().raw_os_error(),
        Some(libc::ECHILD)
    );
    println!(
        "ordinary caller authenticated a root worker through actual sudo; fixed read-only policy discovery completed; clean EOF and reaped sudo"
    );
}
