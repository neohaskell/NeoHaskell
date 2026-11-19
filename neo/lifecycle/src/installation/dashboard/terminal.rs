use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};

static SESSION: Mutex<()> = Mutex::new(());
static INSTALLED: Mutex<Vec<RegisteredAction>> = Mutex::new(Vec::new());
struct RegisteredAction {
    signal: libc::c_int,
    previous: libc::sigaction,
    installed: libc::sigaction,
}

fn same_action(left: &libc::sigaction, right: &libc::sigaction) -> bool {
    left.sa_sigaction == right.sa_sigaction
        && left.sa_flags == right.sa_flags
        && (1..=128).all(|signal| unsafe {
            libc::sigismember(&left.sa_mask, signal) == libc::sigismember(&right.sa_mask, signal)
        })
}

const ENTER: &[u8] = b"\x1b[?1049h\x1b[?25l\x1b[?2004h";
const LEAVE: &[u8] = b"\x1b[0m\x1b[?2004l\x1b[?25h\x1b[?1049l";

fn owned_above_stdio(fd: RawFd) -> io::Result<OwnedFd> {
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    let owned = unsafe { OwnedFd::from_raw_fd(fd) };
    if fd > 2 {
        return Ok(owned);
    }
    let moved = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 3) };
    if moved < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(moved) })
}

fn pipe() -> io::Result<(OwnedFd, OwnedFd)> {
    let mut fds = [-1; 2];
    if unsafe { libc::pipe(fds.as_mut_ptr()) } < 0 {
        return Err(io::Error::last_os_error());
    }
    let first = owned_above_stdio(fds[0]);
    let second = owned_above_stdio(fds[1]);
    let pair = (first?, second?);
    for fd in [&pair.0, &pair.1] {
        if unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC) } < 0
            || unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK) } < 0
        {
            return Err(io::Error::last_os_error());
        }
    }
    Ok(pair)
}

fn duplicate(fd: &OwnedFd) -> io::Result<OwnedFd> {
    owned_above_stdio(unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_DUPFD_CLOEXEC, 3) })
}

pub(super) struct Signals {
    cancel: OwnedFd,
    resize: OwnedFd,
    registrations: Vec<(
        signal_hook::SigId,
        libc::c_int,
        libc::sigaction,
        libc::sigaction,
    )>,
    _exclusive: MutexGuard<'static, ()>,
}

impl Signals {
    pub(super) fn new() -> io::Result<Self> {
        let exclusive = SESSION.try_lock().map_err(|_| io::ErrorKind::WouldBlock)?;
        let (cancel, cancel_writer) = pipe()?;
        let (resize, resize_writer) = pipe()?;
        let mut result = Self {
            cancel,
            resize,
            registrations: Vec::new(),
            _exclusive: exclusive,
        };
        for signal in [
            libc::SIGINT,
            libc::SIGTERM,
            libc::SIGHUP,
            libc::SIGTSTP,
            libc::SIGWINCH,
        ] {
            let mut previous = unsafe { std::mem::zeroed() };
            if unsafe { libc::sigaction(signal, std::ptr::null(), &mut previous) } < 0 {
                return Err(io::Error::last_os_error());
            }
            let mut known = INSTALLED.lock().map_err(|_| io::ErrorKind::Other)?;
            let cached = known.iter().find(|action| action.signal == signal);
            if cached.is_some_and(|action| !same_action(&action.previous, &previous)) {
                return Err(io::ErrorKind::InvalidData.into());
            }
            let writer = if signal == libc::SIGWINCH {
                &resize_writer
            } else {
                &cancel_writer
            };
            let id = signal_hook::low_level::pipe::register(signal, duplicate(writer)?)?;
            let mut installed = unsafe { std::mem::zeroed() };
            if unsafe { libc::sigaction(signal, std::ptr::null(), &mut installed) } < 0 {
                signal_hook::low_level::unregister(id);
                return Err(io::Error::last_os_error());
            }
            if let Some(action) = cached {
                installed = action.installed;
                if unsafe { libc::sigaction(signal, &installed, std::ptr::null_mut()) } < 0 {
                    signal_hook::low_level::unregister(id);
                    return Err(io::Error::last_os_error());
                }
            } else {
                if same_action(&previous, &installed) {
                    signal_hook::low_level::unregister(id);
                    return Err(io::ErrorKind::InvalidData.into());
                }
                known.push(RegisteredAction {
                    signal,
                    previous,
                    installed,
                });
            }
            result.registrations.push((id, signal, previous, installed));
        }
        Ok(result)
    }
    pub(super) fn descriptors(&self) -> [RawFd; 2] {
        [self.cancel.as_raw_fd(), self.resize.as_raw_fd()]
    }
    pub(super) fn drain(&self) -> (bool, bool) {
        (
            drain(self.cancel.as_raw_fd()),
            drain(self.resize.as_raw_fd()),
        )
    }
}

impl Signals {
    pub(super) fn restore(&mut self) -> bool {
        let mut restored = true;
        for (id, signal, previous, installed) in self.registrations.drain(..) {
            restored &= signal_hook::low_level::unregister(id);
            let mut current = unsafe { std::mem::zeroed::<libc::sigaction>() };
            if unsafe { libc::sigaction(signal, std::ptr::null(), &mut current) } == 0
                && same_action(&current, &installed)
            {
                restored &=
                    unsafe { libc::sigaction(signal, &previous, std::ptr::null_mut()) } == 0;
            } else {
                restored = false;
            }
        }
        restored
    }
}
impl Drop for Signals {
    fn drop(&mut self) {
        self.restore();
    }
}

fn drain(fd: RawFd) -> bool {
    let mut bytes = [0u8; 256];
    let mut received = false;
    for _ in 0..64 {
        let count = unsafe { libc::read(fd, bytes.as_mut_ptr().cast(), bytes.len()) };
        if count <= 0 {
            break;
        }
        received = true;
    }
    received
}

struct RestorationMask(Option<libc::sigset_t>);
impl RestorationMask {
    fn new() -> io::Result<Self> {
        let mut blocked = unsafe { std::mem::zeroed::<libc::sigset_t>() };
        let mut previous = blocked;
        unsafe {
            libc::sigemptyset(&mut blocked);
            libc::sigaddset(&mut blocked, libc::SIGTTOU);
        }
        let error = unsafe { libc::pthread_sigmask(libc::SIG_BLOCK, &blocked, &mut previous) };
        if error != 0 {
            return Err(io::Error::from_raw_os_error(error));
        }
        Ok(Self(Some(previous)))
    }
    fn restore(&mut self) -> bool {
        let Some(previous) = self.0 else {
            return true;
        };
        if unsafe { libc::pthread_sigmask(libc::SIG_SETMASK, &previous, std::ptr::null_mut()) } != 0
        {
            return false;
        }
        self.0 = None;
        true
    }
}
impl Drop for RestorationMask {
    fn drop(&mut self) {
        self.restore();
    }
}

#[cfg(all(
    target_os = "linux",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
type PlatformSettings = libc::termios2;
#[cfg(not(all(
    target_os = "linux",
    any(target_arch = "x86_64", target_arch = "aarch64")
)))]
type PlatformSettings = libc::termios;

#[derive(Clone, Copy)]
struct TerminalSettings(PlatformSettings);

#[cfg(all(
    target_os = "linux",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
impl TerminalSettings {
    fn capture(fd: RawFd) -> io::Result<Self> {
        let mut settings = unsafe { std::mem::zeroed::<PlatformSettings>() };
        if unsafe { libc::ioctl(fd, libc::TCGETS2, &mut settings) } < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Self(settings))
    }
    fn raw_mode(&self) -> Self {
        let mut raw = *self;
        raw.0.c_iflag &= !(libc::IGNBRK
            | libc::BRKINT
            | libc::PARMRK
            | libc::ISTRIP
            | libc::INLCR
            | libc::IGNCR
            | libc::ICRNL
            | libc::IXON);
        raw.0.c_oflag &= !libc::OPOST;
        raw.0.c_lflag &= !(libc::ECHO | libc::ECHONL | libc::ICANON | libc::ISIG | libc::IEXTEN);
        raw.0.c_cflag = (raw.0.c_cflag & !(libc::CSIZE | libc::PARENB)) | libc::CS8;
        raw.0.c_cc[libc::VMIN] = 1;
        raw.0.c_cc[libc::VTIME] = 0;
        raw
    }
    fn apply_now(&self, fd: RawFd) -> io::Result<()> {
        if unsafe { libc::ioctl(fd, libc::TCSETS2, &self.0) } < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }
    fn matches(&self, other: &Self) -> bool {
        let (left, right) = (&self.0, &other.0);
        left.c_iflag == right.c_iflag
            && left.c_oflag == right.c_oflag
            && left.c_cflag == right.c_cflag
            && left.c_lflag == right.c_lflag
            && left.c_line == right.c_line
            && left.c_cc == right.c_cc
            && left.c_ispeed == right.c_ispeed
            && left.c_ospeed == right.c_ospeed
    }
}

#[cfg(not(all(
    target_os = "linux",
    any(target_arch = "x86_64", target_arch = "aarch64")
)))]
impl TerminalSettings {
    fn capture(fd: RawFd) -> io::Result<Self> {
        if cfg!(target_os = "linux") {
            return Err(io::ErrorKind::Unsupported.into());
        }
        let mut settings = unsafe { std::mem::zeroed() };
        if unsafe { libc::tcgetattr(fd, &mut settings) } < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Self(settings))
    }
    fn raw_mode(&self) -> Self {
        let mut raw = *self;
        unsafe { libc::cfmakeraw(&mut raw.0) };
        raw
    }
    fn apply_now(&self, fd: RawFd) -> io::Result<()> {
        if unsafe { libc::tcsetattr(fd, libc::TCSANOW, &self.0) } < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }
    fn matches(&self, other: &Self) -> bool {
        let (left, right) = (&self.0, &other.0);
        left.c_iflag == right.c_iflag
            && left.c_oflag == right.c_oflag
            && left.c_cflag == right.c_cflag
            && left.c_lflag == right.c_lflag
            && left.c_cc == right.c_cc
            && unsafe {
                libc::cfgetispeed(left) == libc::cfgetispeed(right)
                    && libc::cfgetospeed(left) == libc::cfgetospeed(right)
            }
    }
}

pub(super) struct Terminal {
    fd: OwnedFd,
    original: TerminalSettings,
    active: Option<TerminalSettings>,
    changed: bool,
}

fn foreground(fd: RawFd) -> bool {
    unsafe { libc::tcgetsid(fd) == libc::getsid(0) && libc::tcgetpgrp(fd) == libc::getpgrp() }
}

impl Terminal {
    pub(super) fn probe() -> io::Result<Option<Self>> {
        let sid = unsafe { libc::getsid(0) };
        let group = unsafe { libc::getpgrp() };
        if unsafe { libc::isatty(1) } != 1
            || unsafe { libc::tcgetsid(1) } != sid
            || unsafe { libc::tcgetpgrp(1) } != group
        {
            return Ok(None);
        }
        let mut original_device = unsafe { std::mem::zeroed::<libc::stat>() };
        let mut name = [0 as libc::c_char; 1024];
        #[cfg(test)]
        let name_descriptor = if super::tests::fault_mode("alias-failure") {
            -1
        } else {
            1
        };
        #[cfg(not(test))]
        let name_descriptor = 1;
        if unsafe { libc::fstat(1, &mut original_device) } < 0
            || unsafe { libc::ttyname_r(name_descriptor, name.as_mut_ptr(), name.len()) } != 0
        {
            return Ok(None);
        }
        let fd = match owned_above_stdio(unsafe {
            libc::open(
                name.as_ptr(),
                libc::O_RDWR | libc::O_NOCTTY | libc::O_NONBLOCK | libc::O_CLOEXEC,
            )
        }) {
            Ok(fd) => fd,
            Err(_) => return Ok(None),
        };
        let mut opened_device = unsafe { std::mem::zeroed::<libc::stat>() };
        if unsafe { libc::fstat(fd.as_raw_fd(), &mut opened_device) } < 0
            || original_device.st_dev != opened_device.st_dev
            || original_device.st_ino != opened_device.st_ino
            || original_device.st_rdev != opened_device.st_rdev
            || unsafe { libc::tcgetsid(fd.as_raw_fd()) } != sid
            || unsafe { libc::tcgetpgrp(fd.as_raw_fd()) } != group
        {
            return Ok(None);
        }
        let original = match TerminalSettings::capture(fd.as_raw_fd()) {
            Ok(original) => original,
            Err(_) => return Ok(None),
        };
        let result = Self {
            fd,
            original,
            active: None,
            changed: false,
        };
        if result.size().is_err() {
            return Ok(None);
        }
        Ok(Some(result))
    }
    pub(super) fn descriptor(&self) -> RawFd {
        self.fd.as_raw_fd()
    }
    pub(super) fn foreground(&self) -> bool {
        foreground(self.descriptor())
    }
    pub(super) fn size(&self) -> io::Result<(u16, u16)> {
        let mut size: libc::winsize = unsafe { std::mem::zeroed() };
        if unsafe { libc::ioctl(self.descriptor(), libc::TIOCGWINSZ, &mut size) } < 0
            || size.ws_col == 0
            || size.ws_row == 0
        {
            return Err(io::ErrorKind::InvalidData.into());
        }
        Ok((size.ws_col.min(200), size.ws_row.min(80)))
    }
    pub(super) fn activate(&mut self) -> io::Result<()> {
        let raw = self.original.raw_mode();
        self.changed = true;
        raw.apply_now(self.descriptor())?;
        self.active = Some(TerminalSettings::capture(self.descriptor())?);
        #[cfg(test)]
        if super::tests::fault_mode("raw-failure") {
            return Err(io::ErrorKind::Other.into());
        }
        #[cfg(test)]
        if super::tests::fault_mode("alternate-failure") {
            self.write_until(&ENTER[..8], Instant::now() + Duration::from_secs(2))?;
            return Err(io::ErrorKind::Other.into());
        }
        self.write_until(ENTER, Instant::now() + Duration::from_secs(2))
    }
    pub(super) fn discard_input(&self) -> io::Result<()> {
        if unsafe { libc::tcflush(self.descriptor(), libc::TCIFLUSH) } < 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
    pub(super) fn read(&self) -> io::Result<Vec<u8>> {
        let mut result = Vec::new();
        let mut bytes = [0u8; 256];
        for _ in 0..64 {
            let count =
                unsafe { libc::read(self.descriptor(), bytes.as_mut_ptr().cast(), bytes.len()) };
            if count == 0 {
                return Err(io::ErrorKind::UnexpectedEof.into());
            }
            if count < 0 {
                let error = io::Error::last_os_error();
                if error.kind() == io::ErrorKind::WouldBlock {
                    return Ok(result);
                }
                if error.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(error);
            }
            if result.len() + count as usize > 4096 {
                return Err(io::ErrorKind::InvalidData.into());
            }
            result.extend_from_slice(&bytes[..count as usize]);
        }
        Err(io::ErrorKind::InvalidData.into())
    }
    pub(super) fn write(&self, bytes: &[u8]) -> io::Result<usize> {
        let count = unsafe { libc::write(self.descriptor(), bytes.as_ptr().cast(), bytes.len()) };
        if count < 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(count as usize)
        }
    }
    fn write_until(&self, bytes: &[u8], deadline: Instant) -> io::Result<()> {
        let mut offset = 0;
        while offset < bytes.len() {
            if Instant::now() >= deadline {
                return Err(io::ErrorKind::TimedOut.into());
            }
            match self.write(&bytes[offset..]) {
                Ok(0) => return Err(io::ErrorKind::WriteZero.into()),
                Ok(count) => offset += count,
                Err(error)
                    if matches!(
                        error.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) =>
                {
                    let mut descriptors = [libc::pollfd {
                        fd: self.descriptor(),
                        events: libc::POLLOUT,
                        revents: 0,
                    }];
                    wait(&mut descriptors, Some(deadline))?;
                    if descriptors[0].revents & (libc::POLLHUP | libc::POLLERR) != 0 {
                        return Err(io::ErrorKind::BrokenPipe.into());
                    }
                }
                Err(error) => return Err(error),
            }
        }
        Ok(())
    }
    fn owns_mode(&self) -> bool {
        if unsafe { libc::tcgetsid(self.descriptor()) } != unsafe { libc::getsid(0) } {
            return false;
        }
        if self.foreground() {
            return true;
        }
        TerminalSettings::capture(self.descriptor()).is_ok_and(|current| {
            self.active
                .as_ref()
                .is_some_and(|active| current.matches(active))
        })
    }
    pub(super) fn restore(&mut self) -> bool {
        if !self.changed {
            return true;
        }
        if !self.owns_mode() {
            return false;
        }
        let Ok(mut mask) = RestorationMask::new() else {
            return false;
        };
        let termios = self.original.apply_now(self.descriptor()).is_ok();
        let discarded = self.discard_input().is_ok();
        let output = self
            .write_until(LEAVE, Instant::now() + Duration::from_secs(2))
            .is_ok();
        let checked = TerminalSettings::capture(self.descriptor())
            .is_ok_and(|current| current.matches(&self.original));
        let mask_restored = mask.restore();
        self.changed = !(discarded && termios && output && checked && mask_restored);
        !self.changed
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        if self.changed && self.owns_mode() {
            if let Ok(_mask) = RestorationMask::new() {
                let _ = self.original.apply_now(self.descriptor());
            }
        }
    }
}

pub(super) fn wait(descriptors: &mut [libc::pollfd], deadline: Option<Instant>) -> io::Result<()> {
    let timeout = deadline.map_or(-1, |deadline| {
        deadline
            .saturating_duration_since(Instant::now())
            .as_nanos()
            .div_ceil(1_000_000)
            .min(i32::MAX as u128) as i32
    });
    if unsafe { libc::poll(descriptors.as_mut_ptr(), descriptors.len() as _, timeout) } < 0 {
        let error = io::Error::last_os_error();
        if error.kind() != io::ErrorKind::Interrupted {
            return Err(error);
        }
    }
    if descriptors
        .iter()
        .any(|descriptor| descriptor.revents & libc::POLLNVAL != 0)
    {
        return Err(io::ErrorKind::InvalidInput.into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Seek, SeekFrom, Write};

    #[test]
    fn input_read_rejects_bytes_beyond_its_budget() {
        let mut input = tempfile::tempfile().unwrap();
        input.write_all(&[b'x'; 4097]).unwrap();
        input.seek(SeekFrom::Start(0)).unwrap();
        let terminal = Terminal {
            fd: input.into(),
            original: unsafe { std::mem::zeroed() },
            active: None,
            changed: false,
        };
        assert_eq!(
            terminal.read().unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );
    }
    #[cfg(all(
        target_os = "linux",
        any(target_arch = "x86_64", target_arch = "aarch64")
    ))]
    #[test]
    fn settings_restore_kernel_encoding_and_input_speed_inheritance() {
        fn kernel_read(fd: RawFd) -> [u8; 44] {
            let mut bytes = [0; 44];
            assert_eq!(
                unsafe { libc::ioctl(fd, 0x802c542a as libc::c_ulong, bytes.as_mut_ptr()) },
                0
            );
            bytes
        }
        fn kernel_write(fd: RawFd, bytes: &[u8; 44]) {
            assert_eq!(
                unsafe { libc::ioctl(fd, 0x402c542b as libc::c_ulong, bytes.as_ptr()) },
                0
            );
        }
        fn word(bytes: &[u8; 44], offset: usize) -> u32 {
            u32::from_ne_bytes(bytes[offset..offset + 4].try_into().unwrap())
        }
        fn set_word(bytes: &mut [u8; 44], offset: usize, value: u32) {
            bytes[offset..offset + 4].copy_from_slice(&value.to_ne_bytes());
        }
        let master = owned_above_stdio(unsafe {
            libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY | libc::O_CLOEXEC)
        })
        .unwrap();
        assert_eq!(unsafe { libc::grantpt(master.as_raw_fd()) }, 0);
        assert_eq!(unsafe { libc::unlockpt(master.as_raw_fd()) }, 0);
        let mut name = [0; 1024];
        assert_eq!(
            unsafe { libc::ptsname_r(master.as_raw_fd(), name.as_mut_ptr(), name.len()) },
            0
        );
        let slave = owned_above_stdio(unsafe {
            libc::open(
                name.as_ptr(),
                libc::O_RDWR | libc::O_NOCTTY | libc::O_CLOEXEC,
            )
        })
        .unwrap();
        let fd = slave.as_raw_fd();
        for input_encoding in [0, 13 << 16] {
            let mut seed = kernel_read(fd);
            let flags = (word(&seed, 8) & !(0x100f | 0x100f0000)) | 15 | input_encoding;
            set_word(&mut seed, 8, flags);
            kernel_write(fd, &seed);
            let original = kernel_read(fd);
            assert_eq!(word(&original, 8) & 0x100f0000, input_encoding);
            let settings = TerminalSettings::capture(fd).unwrap();
            settings.raw_mode().apply_now(fd).unwrap();
            settings.apply_now(fd).unwrap();
            let restored = kernel_read(fd);
            assert_eq!(
                restored, original,
                "all kernel fields and encodings must restore"
            );
            let mut output_changed = restored;
            let flags = (word(&output_changed, 8) & !0x100f) | 14;
            set_word(&mut output_changed, 8, flags);
            kernel_write(fd, &output_changed);
            let updated = kernel_read(fd);
            assert_eq!(word(&updated, 40), 19200);
            assert_eq!(
                word(&updated, 36),
                if input_encoding == 0 { 19200 } else { 9600 }
            );
            assert_eq!(word(&updated, 8) & 0x100f0000, input_encoding);
        }
    }
}
