use super::{
    CFDataCreate, CFDictionaryCreate, CfRef, FileIdentity, INSPECTION_BYTES, OwnedCf,
    SecCodeCheckValidity, SecCodeCopyGuestWithAttributes, inspect_frame,
    kCFTypeDictionaryKeyCallBacks, kCFTypeDictionaryValueCallBacks, kSecGuestAttributeAudit,
    policy_helper_protected_path,
};
use crate::candidate::OutputPath;
use std::ffi::{CStr, OsStr, OsString};
use std::fs;
use std::io::{self, Read, Write};
use std::mem::{MaybeUninit, size_of, size_of_val};
use std::net::Shutdown;
use std::os::fd::{AsFd, AsRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::os::unix::net::UnixStream;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::ptr::null;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

pub(in crate::installation) const LAUNCH_ARGUMENT: &str = "--internal-darwin-discovery-launch";
pub(in crate::installation) const ATTENDED_ARGUMENT: &str =
    "--internal-darwin-discovery-launch-attended";
pub(in crate::installation) const CLEANUP_UNKNOWN: i32 = 124;
const ROOT_ARGUMENT: &str = "--internal-darwin-discovery-root";
const SUDO: &str = "/usr/bin/sudo";

const AUTHENTICATION_LIMIT: Duration = Duration::from_secs(90);
const OBSERVATION_LIMIT: Duration = Duration::from_secs(15);
const ROOT_EXCHANGE_LIMIT: Duration = Duration::from_secs(3);
const CLEANUP_LIMIT: Duration = Duration::from_secs(5);
const RELAY_LIMIT: Duration = Duration::from_secs(2);
const NONCE_BYTES: usize = 16;
const HEADER_BYTES: usize = 4 + NONCE_BYTES;
const REQUEST: &[u8; 4] = b"NDQ1";
const HELLO: &[u8; 4] = b"NDH1";
const START: &[u8; 4] = b"NDS1";
const RESULT: &[u8; 4] = b"NDR2";
const ACCEPT: &[u8; 4] = b"NDA1";
static CANCELLED: AtomicBool = AtomicBool::new(false);

extern "C" fn request_stop(_: libc::c_int) {
    CANCELLED.store(true, Ordering::Relaxed);
}

fn install_stop_handlers() -> bool {
    let mut action: libc::sigaction = unsafe { std::mem::zeroed() };
    action.sa_sigaction = request_stop as *const () as libc::sighandler_t;
    if unsafe { libc::sigemptyset(&mut action.sa_mask) } != 0 {
        return false;
    }
    [libc::SIGINT, libc::SIGTERM, libc::SIGHUP]
        .into_iter()
        .all(|signal| unsafe { libc::sigaction(signal, &action, std::ptr::null_mut()) == 0 })
}

pub(super) fn cancelled() -> bool {
    CANCELLED.load(Ordering::Relaxed)
}

pub(crate) fn dispatch(args: &[OsString]) -> Option<i32> {
    let [argument] = args else { return None };
    let status = match argument.to_str()? {
        LAUNCH_ARGUMENT => {
            if !install_stop_handlers() {
                return Some(1);
            }
            launch(false)
        }
        ATTENDED_ARGUMENT => {
            if !install_stop_handlers() {
                return Some(1);
            }
            launch(true)
        }
        ROOT_ARGUMENT => i32::from(!install_stop_handlers() || !observe_as_root()),
        _ => return None,
    };
    Some(status)
}

struct Executable {
    path: PathBuf,
    identity: FileIdentity,
    signing_information: SigningInformation,
}

impl Executable {
    fn current() -> Option<Self> {
        let pid = unsafe { libc::getpid() };
        let path = process_path(pid)?;
        OutputPath::parse(path.to_str()?).ok()?;
        if path.parent()? != Path::new("/nix/store")
            || path.file_name()?.to_str()?.split_once('-')?.1 != "neo-policy-helper"
        {
            return None;
        }
        let identity = helper_identity(&path)?;
        let mut code = null();
        let status = unsafe { SecCodeCopySelf(0, &mut code) };
        let code = OwnedCf::created(code)?;
        if status != 0 {
            return None;
        }
        Some(Self {
            path,
            identity,
            signing_information: SigningInformation::verified(&code)?,
        })
    }

    fn still_protected(&self) -> bool {
        helper_identity(&self.path).is_some_and(|observed| observed == self.identity)
    }

    fn verifies(&self, socket: &UnixStream, writer: AuditWriter) -> bool {
        process_path(writer.pid()).as_deref() == Some(self.path.as_path())
            && writer
                .signing_information()
                .is_some_and(|info| self.signing_information.same_image(&info))
            && AuditWriter::read(socket) == Some(writer)
    }
}

fn helper_identity(path: &Path) -> Option<FileIdentity> {
    if policy_helper_protected_path(path).ok().as_deref() != Some(path) {
        return None;
    }
    let metadata = fs::symlink_metadata(path).ok()?;
    (metadata.is_file() && metadata.uid() == 0 && metadata.mode() & 0o7777 == 0o555)
        .then(|| metadata.into())
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct AuditWriter([u32; 8]);

impl AuditWriter {
    fn read(socket: &UnixStream) -> Option<Self> {
        let mut token = [0; 8];
        let mut length = size_of_val(&token) as libc::socklen_t;
        // The peer credentials name the socketpair creator. The audit token names the writer.
        let status = unsafe {
            libc::getsockopt(
                socket.as_raw_fd(),
                libc::SOL_LOCAL,
                libc::LOCAL_PEERTOKEN,
                token.as_mut_ptr().cast(),
                &mut length,
            )
        };
        (status == 0
            && length as usize == size_of_val(&token)
            && token[5] > 1
            && i32::try_from(token[5]).is_ok()
            && token[7] != 0)
            .then_some(Self(token))
    }

    fn pid(self) -> libc::pid_t {
        self.0[5] as libc::pid_t
    }

    fn has_uid(self, uid: libc::uid_t) -> bool {
        self.0[1] == uid && self.0[3] == uid
    }

    fn signing_information(self) -> Option<SigningInformation> {
        let token = OwnedCf::created(unsafe {
            CFDataCreate(
                null(),
                self.0.as_ptr().cast(),
                size_of_val(&self.0) as isize,
            )
        })?;
        let attributes = OwnedCf::created(unsafe {
            CFDictionaryCreate(
                null(),
                &kSecGuestAttributeAudit,
                &token.as_ptr(),
                1,
                &raw const kCFTypeDictionaryKeyCallBacks,
                &raw const kCFTypeDictionaryValueCallBacks,
            )
        })?;
        let mut guest = null();
        let status =
            unsafe { SecCodeCopyGuestWithAttributes(null(), attributes.as_ptr(), 0, &mut guest) };
        let guest = OwnedCf::created(guest)?;
        if status != 0 {
            return None;
        }
        SigningInformation::verified(&guest)
    }
}

#[link(name = "CoreFoundation", kind = "framework")]
unsafe extern "C" {
    fn CFDictionaryGetValue(dictionary: CfRef, key: CfRef) -> CfRef;
    fn CFEqual(a: CfRef, b: CfRef) -> u8;
}

#[link(name = "Security", kind = "framework")]
unsafe extern "C" {
    static kSecCodeInfoUnique: CfRef;
    fn SecCodeCopySelf(flags: u32, code: *mut CfRef) -> i32;
    fn SecCodeCopySigningInformation(code: CfRef, flags: u32, info: *mut CfRef) -> i32;
}

struct SigningInformation(OwnedCf);

impl SigningInformation {
    fn verified(code: &OwnedCf) -> Option<Self> {
        const SIGNING_INFO: u32 = 1 << 1;
        if unsafe { SecCodeCheckValidity(code.as_ptr(), 0, null()) } != 0 {
            return None;
        }
        let mut info = null();
        let status =
            unsafe { SecCodeCopySigningInformation(code.as_ptr(), SIGNING_INFO, &mut info) };
        let info = Self(OwnedCf::created(info)?);
        (status == 0 && !info.code_hash().is_null()).then_some(info)
    }

    fn code_hash(&self) -> CfRef {
        unsafe { CFDictionaryGetValue(self.0.as_ptr(), kSecCodeInfoUnique) }
    }

    fn same_image(&self, other: &Self) -> bool {
        unsafe { CFEqual(self.code_hash(), other.code_hash()) != 0 }
    }
}

fn process_path(pid: libc::pid_t) -> Option<PathBuf> {
    let mut bytes = [0_u8; libc::PROC_PIDPATHINFO_MAXSIZE as usize];
    let length = unsafe { libc::proc_pidpath(pid, bytes.as_mut_ptr().cast(), bytes.len() as u32) };
    if length <= 0 || length as usize >= bytes.len() {
        return None;
    }
    let path = CStr::from_bytes_until_nul(&bytes).ok()?;
    Some(PathBuf::from(OsStr::from_bytes(path.to_bytes())))
}

fn process_parent(pid: libc::pid_t) -> Option<libc::pid_t> {
    let mut info = MaybeUninit::<libc::proc_bsdshortinfo>::uninit();
    let length = size_of::<libc::proc_bsdshortinfo>() as libc::c_int;
    let returned = unsafe {
        libc::proc_pidinfo(
            pid,
            libc::PROC_PIDT_SHORTBSDINFO,
            0,
            info.as_mut_ptr().cast(),
            length,
        )
    };
    if returned != length {
        return None;
    }
    let info = unsafe { info.assume_init() };
    (info.pbsi_pid == pid as u32)
        .then(|| i32::try_from(info.pbsi_ppid).ok())
        .flatten()
}

fn protected_sudo() -> bool {
    let sudo = Path::new(SUDO);
    policy_helper_protected_path(sudo).ok().as_deref() == Some(sudo)
        && fs::symlink_metadata(sudo)
            .is_ok_and(|metadata| metadata.is_file() && metadata.mode() & 0o4111 == 0o4111)
}

fn control_socket() -> Option<UnixStream> {
    let fd = io::stdin().as_fd().try_clone_to_owned().ok()?;
    let mut kind = 0;
    let mut size = size_of_val(&kind) as libc::socklen_t;
    let mut address = MaybeUninit::<libc::sockaddr_un>::zeroed();
    let mut address_size = size_of::<libc::sockaddr_un>() as libc::socklen_t;
    let status = unsafe {
        libc::getsockopt(
            fd.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_TYPE,
            (&mut kind as *mut i32).cast(),
            &mut size,
        )
    };
    let connected = unsafe {
        libc::getpeername(
            fd.as_raw_fd(),
            address.as_mut_ptr().cast(),
            &mut address_size,
        )
    };
    if status != 0
        || kind != libc::SOCK_STREAM
        || connected != 0
        || unsafe { address.assume_init() }.sun_family != libc::AF_UNIX as libc::sa_family_t
    {
        return None;
    }
    channel(fd.into()).ok()
}

fn channel(socket: UnixStream) -> io::Result<UnixStream> {
    socket.set_nonblocking(true)?;
    let flag: libc::c_int = 1;
    let status = unsafe {
        libc::setsockopt(
            socket.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_NOSIGPIPE,
            (&flag as *const libc::c_int).cast(),
            size_of_val(&flag) as libc::socklen_t,
        )
    };
    if status != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(socket)
}

fn wait(
    socket: &UnixStream,
    events: libc::c_short,
    control: Option<&UnixStream>,
    deadline: Instant,
) -> io::Result<()> {
    let mut fds = [
        libc::pollfd {
            fd: socket.as_raw_fd(),
            events,
            revents: 0,
        },
        libc::pollfd {
            fd: control.map_or(-1, AsRawFd::as_raw_fd),
            events: libc::POLLIN,
            revents: 0,
        },
    ];
    loop {
        if cancelled() {
            return Err(io::ErrorKind::Interrupted.into());
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return Err(io::ErrorKind::TimedOut.into());
        }
        let timeout = i32::try_from(remaining.as_millis().max(1)).unwrap_or(i32::MAX);
        let ready = unsafe { libc::poll(fds.as_mut_ptr(), fds.len() as libc::nfds_t, timeout) };
        if ready < 0 {
            let error = io::Error::last_os_error();
            if error.kind() == io::ErrorKind::Interrupted {
                continue;
            }
            return Err(error);
        }
        if fds[1].revents != 0 {
            return Err(io::Error::other("administrator operation cancelled"));
        }
        if fds[0].revents & libc::POLLNVAL != 0 {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        if fds[0].revents != 0 {
            return Ok(());
        }
    }
}

struct Exchange<'a> {
    socket: &'a UnixStream,
    control: Option<&'a UnixStream>,
    deadline: Instant,
}

impl Exchange<'_> {
    fn receive(&self, mut buffer: &mut [u8]) -> io::Result<()> {
        while !buffer.is_empty() {
            wait(self.socket, libc::POLLIN, self.control, self.deadline)?;
            match (&*self.socket).read(buffer) {
                Ok(0) => return Err(io::ErrorKind::UnexpectedEof.into()),
                Ok(size) => buffer = &mut buffer[size..],
                Err(error)
                    if error.kind() == io::ErrorKind::WouldBlock
                        || error.kind() == io::ErrorKind::Interrupted => {}
                Err(error) => return Err(error),
            }
        }
        Ok(())
    }

    fn send(&self, mut data: &[u8]) -> io::Result<()> {
        while !data.is_empty() {
            wait(self.socket, libc::POLLOUT, self.control, self.deadline)?;
            match (&*self.socket).write(data) {
                Ok(0) => return Err(io::ErrorKind::WriteZero.into()),
                Ok(size) => data = &data[size..],
                Err(error)
                    if error.kind() == io::ErrorKind::WouldBlock
                        || error.kind() == io::ErrorKind::Interrupted => {}
                Err(error) => return Err(error),
            }
        }
        Ok(())
    }

    fn header(&self, tag: &[u8; 4], nonce: &[u8; NONCE_BYTES]) -> bool {
        let mut found = [0; HEADER_BYTES];
        self.receive(&mut found).is_ok() && found == header(tag, nonce)
    }
}

fn header(tag: &[u8; 4], nonce: &[u8; NONCE_BYTES]) -> [u8; HEADER_BYTES] {
    let mut bytes = [0; HEADER_BYTES];
    bytes[..4].copy_from_slice(tag);
    bytes[4..].copy_from_slice(nonce);
    bytes
}

fn control_open(control: &UnixStream) -> bool {
    let mut fd = libc::pollfd {
        fd: control.as_raw_fd(),
        events: libc::POLLIN,
        revents: 0,
    };
    unsafe { libc::poll(&mut fd, 1, 0) == 0 }
}

fn launch(attended: bool) -> i32 {
    let uid = unsafe { libc::getuid() };
    if uid == 0 || uid != unsafe { libc::geteuid() } || !protected_sudo() {
        return 1;
    }
    let Some(executable) = Executable::current() else {
        return 1;
    };
    let Some(control) = control_socket() else {
        return 1;
    };
    if !control_open(&control) {
        return 1;
    }

    let mut nonce = [0_u8; NONCE_BYTES];
    unsafe { libc::arc4random_buf(nonce.as_mut_ptr().cast(), nonce.len()) };
    let Ok((local, remote)) = UnixStream::pair() else {
        return 1;
    };
    let Ok(local) = channel(local) else { return 1 };
    let Ok(output) = remote.try_clone() else {
        return 1;
    };
    let (input, output): (OwnedFd, OwnedFd) = (remote.into(), output.into());

    if !executable.still_protected() {
        return 1;
    }
    let mut sudo = Command::new(SUDO);
    if !attended {
        sudo.arg("-n").stderr(Stdio::null());
    }
    let child = sudo
        .env_clear()
        .env("PATH", "/usr/bin:/bin:/usr/sbin:/sbin")
        .args(["-u", "root", "--"])
        .arg(&executable.path)
        .arg(ROOT_ARGUMENT)
        .stdin(Stdio::from(input))
        .stdout(Stdio::from(output))
        .spawn();
    drop(sudo);
    let Ok(mut sudo) = child else {
        return 1;
    };

    let sudo_pid = sudo.id() as libc::pid_t;
    let auth = Exchange {
        socket: &local,
        control: Some(&control),
        deadline: Instant::now() + AUTHENTICATION_LIMIT,
    };
    let mut root = None;
    let observation = (|| {
        auth.send(&header(REQUEST, &nonce)).ok()?;
        if !auth.header(HELLO, &nonce) {
            return None;
        }
        let writer = AuditWriter::read(&local)?;
        if !writer.has_uid(0)
            || process_parent(writer.pid()) != Some(sudo_pid)
            || !executable.verifies(&local, writer)
        {
            return None;
        }
        root = Some(writer.pid());
        let exchange = Exchange {
            socket: &local,
            control: Some(&control),
            deadline: Instant::now() + OBSERVATION_LIMIT,
        };
        exchange.send(&header(START, &nonce)).ok()?;
        if !exchange.header(RESULT, &nonce) {
            return None;
        }
        let mut result = [0; INSPECTION_BYTES];
        exchange.receive(&mut result).ok()?;
        if AuditWriter::read(&local) != Some(writer)
            || process_parent(writer.pid()) != Some(sudo_pid)
            || !executable.verifies(&local, writer)
        {
            return None;
        }
        exchange.send(&header(ACCEPT, &nonce)).ok()?;
        Some(result)
    })();

    match finish(&mut sudo, &local, root, observation.is_none()) {
        None => CLEANUP_UNKNOWN,
        Some(true) if !cancelled() && control_open(&control) && executable.still_protected() => {
            if let Some(result) = observation {
                let mut stdout = io::stdout().lock();
                return i32::from(
                    stdout
                        .write_all(&result)
                        .and_then(|()| stdout.flush())
                        .is_err(),
                );
            }
            1
        }
        Some(_) => 1,
    }
}

fn finish(
    sudo: &mut Child,
    socket: &UnixStream,
    root: Option<libc::pid_t>,
    refused: bool,
) -> Option<bool> {
    let _ = socket.shutdown(Shutdown::Write);
    let mut deadline = Instant::now() + CLEANUP_LIMIT;
    let mut status: Option<ExitStatus> = None;
    let mut eof = false;
    let mut extra = false;
    let mut relayed = false;
    loop {
        if !eof {
            let mut bytes = [0; HEADER_BYTES + INSPECTION_BYTES];
            match (&*socket).read(&mut bytes) {
                Ok(0) => eof = true,
                Ok(_) => extra = true,
                Err(error)
                    if error.kind() == io::ErrorKind::WouldBlock
                        || error.kind() == io::ErrorKind::Interrupted => {}
                Err(_) => return None,
            }
        }
        if status.is_none() {
            match sudo.try_wait() {
                Ok(result) => status = result,
                Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
                Err(_) => return None,
            }
        }
        if let Some(status) = status
            && eof
            && root.is_none_or(process_absent)
        {
            let stopped_before_start = cancelled()
                && refused
                && (status.code() == Some(130)
                    || matches!(
                        status.signal(),
                        Some(libc::SIGINT | libc::SIGTERM | libc::SIGHUP)
                    ));
            if root.is_none() && status.code() != Some(1) && !stopped_before_start {
                return None;
            }
            return Some(status.success() && !extra);
        }
        if Instant::now() >= deadline {
            if !refused || relayed || status.is_some() {
                return None;
            }
            unsafe { libc::kill(sudo.id() as libc::pid_t, libc::SIGTERM) };
            relayed = true;
            deadline = Instant::now() + RELAY_LIMIT;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
}

fn process_absent(pid: libc::pid_t) -> bool {
    (unsafe { libc::kill(pid, 0) != 0 })
        && io::Error::last_os_error().raw_os_error() == Some(libc::ESRCH)
}

fn observe_as_root() -> bool {
    if unsafe { libc::getuid() != 0 || libc::geteuid() != 0 } || !protected_sudo() {
        return false;
    }
    let Some(executable) = Executable::current() else {
        return false;
    };
    let Some(socket) = control_socket() else {
        return false;
    };
    let exchange = |deadline| Exchange {
        socket: &socket,
        control: None,
        deadline,
    };
    let initial = exchange(Instant::now() + ROOT_EXCHANGE_LIMIT);
    let mut request = [0; HEADER_BYTES];
    if initial.receive(&mut request).is_err() || &request[..4] != REQUEST {
        return false;
    }
    let nonce: [u8; NONCE_BYTES] = request[4..].try_into().expect("fixed frame");
    let Some(caller) = AuditWriter::read(&socket) else {
        return false;
    };
    let sudo = unsafe { libc::getppid() };
    let expected_caller = || {
        caller.0[1] != 0
            && caller.has_uid(caller.0[1])
            && unsafe { libc::getppid() } == sudo
            && process_path(sudo).as_deref() == Some(Path::new(SUDO))
            && process_parent(sudo) == Some(caller.pid())
            && executable.verifies(&socket, caller)
    };
    if !expected_caller()
        || initial.send(&header(HELLO, &nonce)).is_err()
        || !initial.header(START, &nonce)
        || !expected_caller()
    {
        return false;
    }

    let Some(observation) = inspect_frame() else {
        return false;
    };
    if !executable.still_protected() || !expected_caller() {
        return false;
    }
    let reply = exchange(Instant::now() + ROOT_EXCHANGE_LIMIT);
    let mut frame = [0; HEADER_BYTES + INSPECTION_BYTES];
    frame[..HEADER_BYTES].copy_from_slice(&header(RESULT, &nonce));
    frame[HEADER_BYTES..].copy_from_slice(&observation);
    reply.send(&frame).is_ok() && reply.header(ACCEPT, &nonce) && expected_caller()
}
